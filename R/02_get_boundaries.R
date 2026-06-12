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

  # run praat on the created script
  run_praat(args = paste0('--run "', script_file, '" ', '"', folder, '" "" 1'))

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
#' @param path The path to a Praat executable file. If NULL, then uses the default for the system.
#'
#' @export
set_praat_path <- function(path = NULL){
  if (is.null(path)){
    sys = Sys.info()[['sysname']]

    if (sys == "Darwin") path = "/Applications/Praat.app/Contents/MacOS/Praat"
    if (sys == "Linux") path = "/usr/bin/praat"
    if (sys == "Windows") path = "C:/Program Files/Praat.exe"
  }
  options(wav2textgrid.praat.path = path)
}


# run praat
run_praat <- function(args){
  system2(getOption("wav2textgrid.praat.path"), args)
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
