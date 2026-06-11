.onLoad <- function(libname = find.package("wav2textgrid"),
                    pkgname = "wav2textgrid") {

  # global vars
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("osVersion", "tier_xmax", "end", "xmin", "folder", "xmax", "annotation_num", ".", "1", "2", "channel", "dist", "dom", "end.x", "i.end", "i.id", "i.text", "id", "min_dist", "start", "start.x", "text", "start1", "end1"))
  }

  # declare python requirements early so reticulate includes them whenever
  # python is first initialized (a user-bound environment still takes precedence)
  tryCatch(ensure_whisper(), error = function(e) invisible(NULL))

  # find praat
  sys = Sys.info()[['sysname']]
  if (sys == "Darwin") path = "/Applications/Praat.app/Contents/MacOS/Praat"
  if (sys == "Linux") path = "/usr/bin/praat"
  if (sys == "Windows") path = "C:/Program Files/Praat.exe"
  options(wav2textgrid.praat.path = path)

  # finish it up
  invisible()
}

.onAttach <- function(libname = find.package("wav2textgrid"),
                      pkgname = "wav2textgrid") {

  path = getOption("wav2textgrid.praat.path")

  if (file.exists(path)){
    packageStartupMessage(paste("Praat found at", path))
  } else
    packageStartupMessage(paste("Did not find Praat at default location (", path, ").\nPlease run `set_praat_path()` with the path to your Praat application."))

  # finish it up
  invisible()
}




