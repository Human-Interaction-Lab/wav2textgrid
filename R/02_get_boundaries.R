#' @title Get Boundaries
#'
#' @description Uses Praat to find boundaries of speaking turns.
#'
#' @param folder the folder where the wav_file is located
#' @param min_pitch Minimum pitch (Hz)
#' @param time_step Time step (s)
#' @param threshold silence threshold, default is -45
#' @param min_silent_int Minimum silent interval (s)
#' @param min_sound_int Minimum sounding interval (s)
#'
#' @importFrom glue glue
#' @importFrom readtextgrid read_textgrid
#' @importFrom scales number
#'
#' @export
get_boundaries <- function(folder, min_pitch, time_step, threshold, min_silent_int, min_sound_int){
  # the parameters for the To TextGrid command:
  #   Minimum pitch (Hz)
  #   Time step (s)
  #   Silence threshold (dB)
  #   Minimum silent interval (s)
  #   Minimum sounding interval (s)

  if (stringr::str_detect(Sys.info()['sysname'], "Darwin|Linux")){
    script <- glue::glue({"
#start praat
form Enter directory and search string
    sentence Directory {folder}
    sentence Word
    boolean anotate_silences 1
endform"})
  } else {
    script <- glue::glue({"
#start praat
form Enter directory and search string
    sentence Directory {folder}\n
    sentence Word
    boolean anotate_silences 1
endform"})
  }

  script <- glue::glue(script, '\n\n', "Create Strings as file list... file-list 'directory$''word$'*.wav")
  script <- glue::glue(script, '\n\n', "number_of_files = Get number of strings")
  script <- glue::glue(script, '\n\n', "for x from 1 to number_of_files")
  script <- glue::glue(script, '\n', "    select Strings file-list")
  script <- glue::glue(script, '\n', "    current_file$ = Get string... x")
  script <- glue::glue(script, '\n', "    Read from file... 'directory$''current_file$'")
  script <- glue::glue(script, '\n', "    if anotate_silences = 1")
  script <- glue::glue(script, '\n', '        To TextGrid (silences): {min_pitch}, {time_step}, {threshold}, {min_silent_int}, {min_sound_int}, "silence", "sounding"')
  script <- glue::glue(script, '\n', "        Write to text file... 'directory$''current_file$'_silences.TextGrid")
  script <- glue::glue(script, '\n', "    endif")
  script <- glue::glue(script, '\n', "endfor")
  script <- glue::glue(script, '\n\n', "select all")
  script <- glue::glue(script, '\n', "Remove")
  # script <- glue::glue(script, "\n\n", 'writeInfoLine: "Done finding silences :)"')
  script_file <- paste0(folder, "temp.praat")
  writeLines(script, con = script_file)

  # run praat on the created script (form args: directory, word, annotate silences)
  run_praat(c("--run", shQuote(script_file), shQuote(folder), shQuote(""), "1"))

  # confirm praat wrote a silences TextGrid for every channel file
  wavs = fs::dir_ls(folder, regexp = "\\.wav$")
  expected = paste0(wavs, "_silences.TextGrid")
  missing = expected[!fs::file_exists(expected)]
  if (length(missing) > 0)
    stop("Praat ran but did not write the silences TextGrid for: ",
         paste(fs::path_file(wavs[!fs::file_exists(expected)]), collapse = ", "),
         call. = FALSE)

  # check output (only meaningful when there are two channels to compare)
  textgrid_to_check = fs::dir_ls(folder, regexp = "TextGrid$")
  if (length(textgrid_to_check) >= 2) check_shared_boundaries(textgrid_to_check)

  # if successful return 1
  return(1)
}


#' @title Praat File Path
#'
#' @description
#' This allows the user to define where their Praat installation is located if not in the default.
#'
#' @param path The path to the Praat executable. On macOS this can also be the
#' app bundle (e.g., "/Applications/Praat.app"). If NULL, searches the usual
#' install locations for the system and the PATH.
#'
#' @details
#' The path is stored in `options(wav2textgrid.praat.path)`. Praat calls are
#' stopped after `getOption("wav2textgrid.praat.timeout")` seconds (default 600)
#' so a stuck Praat process cannot stall the pipeline; raise it for very long
#' recordings, e.g. `options(wav2textgrid.praat.timeout = 1800)`.
#'
#' @return The path, invisibly.
#'
#' @export
set_praat_path <- function(path = NULL){
  if (is.null(path)) path = default_praat_path()
  path = path.expand(path)

  # accept the macOS app bundle and point at the executable inside it
  if (grepl("\\.app/?$", path) && dir.exists(path))
    path = file.path(sub("/$", "", path), "Contents", "MacOS", "Praat")

  if (!file.exists(path) || dir.exists(path))
    cli::cli_alert_warning(paste0("No Praat executable found at '", path, "'. Praat steps will fail until this is fixed."))

  options(wav2textgrid.praat.path = path)
  invisible(path)
}


# first existing Praat executable among the usual install locations and the PATH;
# falls back to the conventional location so messages can show where we looked
default_praat_path <- function(){
  sys = Sys.info()[["sysname"]]
  candidates = switch(sys,
    Darwin = c("/Applications/Praat.app/Contents/MacOS/Praat",
               path.expand("~/Applications/Praat.app/Contents/MacOS/Praat")),
    Windows = c("C:/Program Files/Praat/Praat.exe",
                "C:/Program Files/Praat.exe",
                "C:/Program Files (x86)/Praat/Praat.exe",
                file.path(Sys.getenv("USERPROFILE"), "Desktop", "Praat.exe")),
    c("/usr/bin/praat", "/usr/local/bin/praat", "/snap/bin/praat")
  )
  on_path = unname(Sys.which(c("praat", "Praat")))
  candidates = unique(c(candidates, on_path[nzchar(on_path)]))
  found = candidates[file.exists(candidates) & !dir.exists(candidates)]
  if (length(found) > 0) found[1] else candidates[1]
}


# stop with an actionable message if the configured Praat executable is missing
check_praat <- function(path = getOption("wav2textgrid.praat.path")){
  if (is.null(path) || !nzchar(path) || !file.exists(path) || dir.exists(path))
    stop("Praat was not found", if (!is.null(path)) paste0(" at '", path, "'"), ". ",
         "Install Praat (https://www.fon.hum.uva.nl/praat/) or run ",
         "`set_praat_path()` with the path to the Praat executable.", call. = FALSE)
  path
}


# run praat, failing loudly on a non-zero exit status or a timeout
run_praat <- function(args, timeout = getOption("wav2textgrid.praat.timeout", 600)){
  praat = check_praat()
  # system2() quotes the command itself; args are quoted by the caller
  out = suppressWarnings(
    system2(praat, args, stdout = TRUE, stderr = TRUE, timeout = timeout)
  )
  status = attr(out, "status")
  if (!is.null(status) && status != 0){
    if (status == 124)
      stop("Praat did not finish within ", timeout, " seconds and was stopped. ",
           "For long recordings, raise `options(wav2textgrid.praat.timeout = ...)`.",
           call. = FALSE)
    stop("Praat failed (exit status ", status, "). Praat output:\n",
         paste(utils::tail(out, 20), collapse = "\n"), call. = FALSE)
  }
  invisible(out)
}



# helper for shared boundaries
check_shared_boundaries <- function(textgrid_files){
  # read in textgrid
  textgrid1 = readtextgrid::read_textgrid(textgrid_files[1])
  textgrid2 = readtextgrid::read_textgrid(textgrid_files[2])

  # check mins and maxs
  xmin1 = textgrid1$xmin
  xmin2 = textgrid2$xmin
  xmax1 = textgrid1$xmax
  xmax2 = textgrid2$xmax

  # remove zeros
  xmin1 = xmin1[xmin1 != 0]
  xmin2 = xmin2[xmin2 != 0]
  xmax1 = xmax1[xmax1 != 0]
  xmax2 = xmax2[xmax2 != 0]

  # truncate to nearest tenth of a second
  xmin1 = scales::number(xmin1, accuracy = .1)
  xmin2 = scales::number(xmin2, accuracy = .1)
  xmax1 = scales::number(xmax1, accuracy = .1)
  xmax2 = scales::number(xmax2, accuracy = .1)

  # check overlap
  min_overlap1 = sum(xmin1 %in% xmin2)/length(xmin1)
  max_overlap1 = sum(xmax1 %in% xmax2)/length(xmax1)
  min_overlap2 = sum(xmin2 %in% xmin1)/length(xmin2)
  max_overlap2 = sum(xmax2 %in% xmax1)/length(xmax2)

  # warn if necessary
  if (min_overlap1 > .3 || max_overlap1 > .3){
    cli::cli_alert_warning(paste0("The Silence/Sounding TextGrid for Channel 1 found ", round(max(c(min_overlap1, max_overlap1))*100, 1), "% of the boundaries were identical to Channel 2."))
    cli::cli_alert_warning("This suggests there is an issue with the threshold parameter (or others).")
  }
  if (min_overlap2 > .3 || max_overlap2 > .3){
    cli::cli_alert_warning(paste0("The Silence/Sounding TextGrid for Channel 2 found ", round(max(c(min_overlap2, max_overlap2))*100, 1), "% of the boundaries were identical to Channel 1."))
    cli::cli_alert_warning("This suggests there is an issue with the threshold parameter (or others).")
  }
}


# read the silences TextGrid that praat wrote for one channel, failing with a
# clear message (rather than an obscure read error) if it is missing
read_silences <- function(folder, chan){
  file = fs::dir_ls(folder, regexp = paste0("_ch", chan, "\\.wav_silences\\.TextGrid$"))
  if (length(file) != 1)
    stop("Expected one silences TextGrid for channel ", chan, " in ", folder,
         " but found ", length(file), ". Did Praat run successfully in get_boundaries()?",
         call. = FALSE)
  readtextgrid::read_textgrid(file)
}
