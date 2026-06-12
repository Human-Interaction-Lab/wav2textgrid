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
whispering <- function(ch1, ch2 = NULL, folder, model_type, prompt, whisp = NULL){
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
    results[[chan]] = whisper_channel(channel_files[chan], chan, folder, model, prompt)
  }
  return(results)
}


# segment one channel file at its silence boundaries and transcribe each segment
whisper_channel <- function(channel_file, chan, folder, model, prompt){
  # grab silence/sounding timings
  silences = readtextgrid::read_textgrid(fs::dir_ls(folder, regexp = paste0("ch", chan, ".wav_silences")))
  timings = silences[silences$text == "sounding", c("annotation_num", "xmin", "xmax")]
  colnames(timings) = c("annotation_num", "start", "end")

  # import channel
  audio = tuneR::readWave(channel_file)
  sample_freq = audio@samp.rate

  # create segmented audio
  duration = length(audio@left)/sample_freq
  segments = vector("list", length = nrow(timings))
  for (i in 1:nrow(timings)){
    rows = timings[i,]
    start = max(rows$start - 0.2, 0)
    end = min(rows$end + 0.2, duration)
    audio_seg = seewave::cutw(audio, f = sample_freq, from = start, to = end, output = "Wave")
    tuneR::writeWave(audio_seg, filename = fs::path(folder, paste0("ch", chan, "_segment_", i, ".wav")))
    segments[[i]] = wave_to_whisper(audio_seg, sample_freq)
  }

  # transcribe
  result = vector("list", length = length(segments))
  message = paste0("Transcribing Channel ", chan, " | n = ", length(segments), " |")
  cli::cli_progress_bar(message, total = length(segments))
  for (i in seq_along(segments)){
    result[[i]] = model$transcribe(segments[[i]], fp16 = FALSE, initial_prompt = prompt)
    cli::cli_progress_update(set = i)
  }
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
