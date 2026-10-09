.onLoad <- function(libname = find.package("wav2textgrid"),
                    pkgname = "wav2textgrid") {

  # global vars
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("osVersion", "tier_xmax", "end", "xmin", "folder", "xmax", "annotation_num", ".", "1", "2", "channel", "dist", "dom", "end.x", "i.end", "i.id", "i.text", "id", "min_dist", "start", "start.x", "text", "start1", "end1"))
  }

  # declare python requirements early so reticulate includes them whenever
  # python is first initialized (a user-bound environment still takes precedence)
  tryCatch(ensure_whisper(), error = function(e) invisible(NULL))

  # find praat (keep a path the user already set, e.g. in their .Rprofile)
  if (is.null(getOption("wav2textgrid.praat.path")))
    options(wav2textgrid.praat.path = default_praat_path())
  if (is.null(getOption("wav2textgrid.praat.timeout")))
    options(wav2textgrid.praat.timeout = 600)

  # finish it up
  invisible()
}

.onAttach <- function(libname = find.package("wav2textgrid"),
                      pkgname = "wav2textgrid") {

  path = getOption("wav2textgrid.praat.path")

  if (!is.null(path) && file.exists(path) && !dir.exists(path)){
    packageStartupMessage(paste("Praat found at", path))
  } else
    packageStartupMessage(paste("Did not find Praat at default location (", path, ").\nPlease run `set_praat_path()` with the path to your Praat application."))

  # finish it up
  invisible()
}




