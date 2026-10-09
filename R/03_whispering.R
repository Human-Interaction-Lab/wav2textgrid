#' @title Whisper Transcription
#'
#' @description Applies the OpenAI Whisper model to transcribe the conversation
#'
#' @param ch1 channel 1 file
#' @param ch2 channel 2 file (NULL for single channel wav files)
#' @param folder folder of the files
#' @param model_type the type of Whisper model to run
#' @param prompt Can prompt the model with words, names, spellings you want it to use.
#' @param whisp the reticulated whisper model (e.g. produced via `whisper = reticulate::import("whisper"); model = whisper$load_model(model_type)`)
#' @param language the spoken language passed to Whisper (default "en"). Setting
#' it avoids Whisper guessing the language separately for every segment, which
#' is unreliable on short segments (backchannels, single words). Use NULL to
#' let Whisper detect the language.
#' @param drop_prompt_echo Whisper sometimes repeats its prompt back on
#' near-silent or unclear audio. If TRUE (default), segments whose text is
#' mostly a verbatim run of the prompt are treated as non-speech, with a warning
#' listing them.
#'
#' @details
#' If Whisper errors on a segment, that segment is treated as non-speech and the
#' run continues; a warning lists the failed segments with their times. If every
#' segment of a channel fails, the run stops with Whisper's first error.
#'
#' @importFrom readtextgrid read_textgrid
#' @importFrom glue glue
#' @import tuneR
#' @importFrom seewave cutw
#' @importFrom seewave resamp
#' @importFrom fs path
#' @import reticulate
#' @importFrom cli cli_progress_bar
#' @importFrom cli cli_progress_update
#'
#' @export
whispering <- function(ch1, ch2 = NULL, folder, model_type, prompt, whisp = NULL,
                       language = "en", drop_prompt_echo = TRUE){
  # declare python requirements before any reticulate call (np_array below)
  # initializes python, which locks in the environment
  ensure_whisper()

  # set up model if not provided
  if (is.null(whisp)){
    whisper = reticulate::import("whisper")
    model = whisper$load_model(model_type)
  } else {
    model = whisp
  }

  channel_files = c(ch1, ch2)
  results = vector("list", length = length(channel_files))
  for (chan in seq_along(channel_files)){
    results[[chan]] = whisper_channel(channel_files[chan], chan, folder, model, prompt,
                                      language = language, drop_prompt_echo = drop_prompt_echo)
  }
  return(results)
}


# segment one channel file at its silence boundaries and transcribe each segment
whisper_channel <- function(channel_file, chan, folder, model, prompt,
                            language = "en", drop_prompt_echo = TRUE){
  # grab silence/sounding timings
  silences = read_silences(folder, chan)
  timings = silences[silences$text == "sounding", c("annotation_num", "xmin", "xmax")]
  colnames(timings) = c("annotation_num", "start", "end")

  # nothing to transcribe if praat found no speech on this channel
  # (clean_up() turns this channel into a single non-speech interval)
  if (nrow(timings) == 0){
    cli::cli_alert_warning(paste0(
      "No speech detected on channel ", chan, "; its tier will be all non-speech. ",
      "If that is unexpected, try a lower (more negative) `threshold`."
    ))
    return(list())
  }

  # import channel
  audio = tuneR::readWave(channel_file)
  sample_freq = audio@samp.rate
  duration = length(audio@left)/sample_freq

  # cut, convert, and transcribe one segment at a time (keeps memory flat on long
  # recordings); a failing segment is recorded and skipped rather than ending the run
  n = nrow(timings)
  result = vector("list", length = n)
  failed = integer(0)
  echoed = integer(0)
  first_error = NULL
  message = paste0("Transcribing Channel ", chan, " | n = ", n, " |")
  cli::cli_progress_bar(message, total = n)
  for (i in seq_len(n)){
    start = max(timings$start[i] - 0.2, 0)
    end = min(timings$end[i] + 0.2, duration)
    res = tryCatch({
      audio_seg = seewave::cutw(audio, f = sample_freq, from = start, to = end, output = "Wave")
      model$transcribe(wave_to_whisper(audio_seg, sample_freq), fp16 = FALSE,
                       initial_prompt = prompt, language = language)
    }, error = function(e) e)

    if (inherits(res, "error")){
      failed = c(failed, i)
      if (is.null(first_error)) first_error = conditionMessage(res)
      res = list(text = "", segments = list())
    } else if (drop_prompt_echo && is_prompt_echo(res[["text"]], prompt)){
      echoed = c(echoed, i)
      res = list(text = "", segments = list())
    }
    result[[i]] = res
    cli::cli_progress_update(set = i)
  }
  cli::cli_progress_done()

  if (length(failed) == n)
    stop("Whisper failed on every segment of channel ", chan, ". First error:\n",
         first_error, call. = FALSE)
  if (length(failed) > 0)
    warning("Whisper failed on ", length(failed), " of ", n, " segments on channel ", chan,
            " (treated as non-speech): ", describe_segments(failed, timings),
            ". First error: ", first_error, call. = FALSE)
  if (length(echoed) > 0)
    warning("Whisper repeated the prompt instead of transcribing ", length(echoed),
            " segment(s) on channel ", chan, " (treated as non-speech): ",
            describe_segments(echoed, timings),
            ". Check these in Praat; use drop_prompt_echo = FALSE to keep the text.",
            call. = FALSE)
  return(result)
}


# convert a Wave segment to what whisper expects (mono float32 in [-1, 1] at 16 kHz)
# passed to transcribe() as a numpy array so whisper never shells out to ffmpeg
wave_to_whisper <- function(wave, sample_freq){
  if (sample_freq != 16000)
    wave = seewave::resamp(wave, f = sample_freq, g = 16000, output = "Wave")
  samples = as.numeric(wave@left) / (2^(wave@bit - 1))
  reticulate::np_array(samples, dtype = "float32")
}


# does whisper's text look like the prompt repeated back? TRUE when at least half
# of the text's n-word runs appear verbatim in the prompt. Requiring runs of
# n = 6 words keeps ordinary filler speech ("um yeah so") from matching.
is_prompt_echo <- function(text, prompt, n = 6){
  if (is.null(text) || is.null(prompt) || length(text) != 1 || is.na(text)) return(FALSE)
  normalize = function(x) stringr::str_squish(gsub("[^[:alnum:][:space:]]", "", tolower(x)))
  words = strsplit(normalize(text), " ", fixed = TRUE)[[1]]
  if (length(words) < n) return(FALSE)
  prompt_padded = paste0(" ", normalize(prompt), " ")
  runs = vapply(seq_len(length(words) - n + 1), function(i)
    paste0(" ", paste(words[i:(i + n - 1)], collapse = " "), " "), character(1))
  mean(vapply(runs, grepl, logical(1), x = prompt_padded, fixed = TRUE)) >= 0.5
}


# "#3 (12.4-15.1s), #9 (40.2-41.0s)" for warnings, capped at max_show segments
describe_segments <- function(idx, timings, max_show = 10){
  out = sprintf("#%d (%.1f-%.1fs)", idx, timings$start[idx], timings$end[idx])
  if (length(out) > max_show)
    out = c(out[seq_len(max_show)], paste("and", length(out) - max_show, "more"))
  paste(out, collapse = ", ")
}
