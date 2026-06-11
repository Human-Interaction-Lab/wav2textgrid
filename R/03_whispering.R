#' @title Whisper Transcription
#'
#' @description Applies the OpenAI Whisper model to transcribe the conversation
#'
#' @param ch1 channel 1 file
#' @param ch2 channel 2 file
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
whispering <- function(ch1, ch2, folder, model_type, prompt, whisp = NULL){
  # grab silence/sounding timings
  chan1_silences = readtextgrid::read_textgrid(fs::dir_ls(folder, regexp = "ch1.wav_silences"))
  chan2_silences = readtextgrid::read_textgrid(fs::dir_ls(folder, regexp = "ch2.wav_silences"))
  timings1 = chan1_silences[chan1_silences$text == "sounding", c("annotation_num", "xmin", "xmax")]
  timings2 = chan2_silences[chan2_silences$text == "sounding", c("annotation_num", "xmin", "xmax")]
  colnames(timings1) = c("annotation_num", "start", "end")
  colnames(timings2) = c("annotation_num", "start", "end")

  # import channels
  ch1_audio = tuneR::readWave(ch1)
  ch2_audio = tuneR::readWave(ch2)
  sample_freq1 = ch1_audio@samp.rate
  sample_freq2 = ch2_audio@samp.rate

  # create segmented audio
  duration1 = length(ch1_audio@left)/sample_freq1
  duration2 = length(ch2_audio@left)/sample_freq2
  ch1_segments = vector("list", length = nrow(timings1))
  for (i in 1:nrow(timings1)){
    rows = timings1[i,]
    start = max(rows$start - 0.2, 0)
    end = min(rows$end + 0.2, duration1)
    audio_seg1 = seewave::cutw(ch1_audio, f = sample_freq1, from = start, to = end, output = "Wave")
    tuneR::writeWave(audio_seg1, filename = fs::path(folder, paste0("ch1_segment_", i, ".wav")))
    ch1_segments[[i]] = wave_to_whisper(audio_seg1, sample_freq1)
  }
  ch2_segments = vector("list", length = nrow(timings2))
  for (i in 1:nrow(timings2)){
    rows = timings2[i,]
    start = max(rows$start - 0.2, 0)
    end = min(rows$end + 0.2, duration2)
    audio_seg2 = seewave::cutw(ch2_audio, f = sample_freq2, from = start, to = end, output = "Wave")
    tuneR::writeWave(audio_seg2, filename = fs::path(folder, paste0("ch2_segment_", i, ".wav")))
    ch2_segments[[i]] = wave_to_whisper(audio_seg2, sample_freq2)
  }

  # set up model if not provided
  if (is.null(whisp)){
    ensure_whisper()
    whisper = reticulate::import("whisper")
    model = whisper$load_model(model_type)
  } else {
    model = whisp
  }

  # channel 1
  result1 = vector("list", length = length(ch1_segments))
  message1 = paste0("Transcribing Channel 1 | n = ", length(ch1_segments), " |")
  cli::cli_progress_bar(message1, total = length(ch1_segments))
  for (i in seq_along(ch1_segments)){
    result1[[i]] = model$transcribe(ch1_segments[[i]], fp16 = FALSE, initial_prompt = prompt)
    cli::cli_progress_update(set = i)
  }

  # channel 2
  result2 = vector("list", length = length(ch2_segments))
  message2 = paste0("Transcribing Channel 2 | n = ", length(ch2_segments), " |")
  cli::cli_progress_bar(message2, total = length(ch2_segments))
  for (i in seq_along(ch2_segments)){
    result2[[i]] = model$transcribe(ch2_segments[[i]], fp16 = FALSE, initial_prompt = prompt)
    cli::cli_progress_update(set = i)
  }

  return(list(result1, result2))
}


# convert a Wave segment to what whisper expects (mono float32 in [-1, 1] at 16 kHz)
# passed to transcribe() as a numpy array so whisper never shells out to ffmpeg
wave_to_whisper <- function(wave, sample_freq){
  if (sample_freq != 16000)
    wave = seewave::resamp(wave, f = sample_freq, g = 16000, output = "Wave")
  samples = as.numeric(wave@left) / (2^(wave@bit - 1))
  reticulate::np_array(samples, dtype = "float32")
}
