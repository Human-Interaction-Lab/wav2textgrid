#' @title Split Channels
#'
#' @description Splits the wav file into its channels (one wav file per
#' speaker tier). Mono wav files produce a single channel file; stereo wav
#' files produce two.
#'
#' @param wav_file The path to the wav file
#' @param noise_reduction whether the praat noise reduction script should be run before getting boundaries, default = FALSE
#' @param threshold noise level for removal
#' @param plot should the tuneR::plot() be made for each channel? Default is FALSE.
#' @param channels how to treat the wav file's channels. "auto" (default) uses
#' the number of channels in the file (mono = one tier, stereo = two tiers),
#' "mono" mixes a stereo file down to a single tier, and "stereo" requires a
#' two channel file.
#'
#' @return A character vector of the channel file paths (length 1 for mono,
#' length 2 for stereo).
#'
#' @import tuneR
#' @importFrom stringr str_replace regex
#'
#' @export
split_channels <- function(wav_file, noise_reduction = FALSE, threshold = 200, plot = FALSE, channels = "auto"){
  channels <- match.arg(channels, c("auto", "mono", "stereo"))

  # read in wave file
  wav <- tuneR::readWave(wav_file)
  wav <- tuneR::normalize(wav, unit = "16")
  n_chan <- tuneR::nchannel(wav)
  if (! n_chan %in% c(1, 2)) stop("wave file needs to have 1 or 2 channels")

  if (channels == "auto") channels <- if (n_chan == 1) "mono" else "stereo"
  if (channels == "stereo" && n_chan != 2) stop("wave file needs to have 2 channels (use channels = 'mono' or 'auto' for single channel files)")

  # one Wave object per output tier
  if (channels == "mono"){
    if (n_chan == 2){
      # mixing down averages the channels, which yields non-integer samples
      wav <- tuneR::mono(wav, which = "both")
      wav@left <- round(wav@left)
    }
    waves <- list(wav)
  } else {
    waves <- list(tuneR::channel(wav, which = "left"), tuneR::channel(wav, which = "right"))
  }

  # file names
  chs <- vapply(seq_along(waves), function(i){
    stringr::str_replace(wav_file, stringr::regex("\\.wav$", ignore_case = TRUE), paste0("_ch", i, ".wav"))
  }, character(1))

  # save channeled wav files
  for (i in seq_along(waves)){
    tuneR::writeWave(waves[[i]], chs[i])
    if (plot) tuneR::plot(waves[[i]])
  }

  # delete tmp folder if already exists
  if (fs::dir_exists(file.path(fs::path_dir(wav_file), "tmp")))
    fs::dir_delete(file.path(fs::path_dir(wav_file), "tmp"))

  # move files to tmp folder
  fs::dir_create(file.path(fs::path_dir(wav_file), "tmp"))
  for (i in seq_along(chs)){
    file_name = fs::path_file(chs[i])
    fs::file_move(chs[i], file.path(fs::path_dir(chs[i]), "tmp"))
    chs[i] = file.path(fs::path_dir(chs[i]), "tmp", file_name)
  }

  # noise reduction
  if (noise_reduction){
    for (ch in chs) noise_reduce(fs::path_dir(ch), ch)
  }

  # check that files exist
  if (! all(fs::file_exists(chs))) stop("error creating channels")

  # return names
  return(chs)
}



# create and run script for noise_reduction
noise_reduce <- function(folder, channel_file){
  script <- glue::glue({"
# Made by Lotte Eijk - 17-11-22
# Reduce noise using the standard Praat settings and save denoised file to specified folder

form Reduce noise
    sentence directory {folder}/
    sentence Word
    positive Channel: 1
endform"})
  script <- glue::glue(script, '\n\n', "Create Strings as file list... file-list 'directory$''word$'*.wav")
  script <- glue::glue(script, '\n\n', "numberOfFiles = Get number of strings")
  script <- glue::glue(script, '\n\n', "for ifile to numberOfFiles")
  script <- glue::glue(script, '\n', "    select Strings list")
  script <- glue::glue(script, '\n', "    fileName$ = Get string... ifile")
  script <- glue::glue(script, '\n', "    Read from file... 'director$''fileName$'")
  script <- glue::glue(script, '\n', '    Reduce noise: 0, 0, 0.025, 80, 10000, 40, -20, "spectral-subtraction"')
  script <- glue::glue(script, '\n', "    lengthFN = length (fileName$)")
  script <- glue::glue(script, '\n', "    newfilename$ = fileName$")
  script <- glue::glue(script, '\n', "    Write to WAV file... 'directory$''word$'*.wav")
  script <- glue::glue(script, '\n', "endfor")
  script <- glue::glue(script, '\n\n', "select all")
  script <- glue::glue(script, '\n', "Remove")
  script_file <- file.path(folder, "temp_noise.praat")
  writeLines(script, con = script_file)

  speakr::praat_run(script_file, folder, '""', 1)
}

# script <- glue::glue(script, '\n\n', "printline 'directory$'")
# script <- glue::glue(script, '\n', "printline 'word$'")
# script <- glue::glue(script, '\n', "printline 'number_of_files'")
