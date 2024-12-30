#' @title Clean Up Whisper Output
#'
#' @description Cleans up the data to get it ready for making
#' the TextGrid file.
#'
#' @param whispered1 channel one whisper data output
#' @param whispered2 channel two whisper data output
#' @param folder the folder where the files are located
#' @param remove_partial Should the model keep words that are incomplete at the end of the sentence? Default is FALSE.
#' @param hyphen Should hyphens be retained or replaced? Options are "space" (hyphens are replaced with a space), "keep" (the hyphens are retained), "remove" the hyphens are removed with no white space added.
#' @param remove_apostrophe Should all apostraphes be removed? Default is FALSE.
#' @param remove_punct Should all punctuation be removed (other than hyphens and apostrophes)? Default is FALSE.
#' @param lowercase Should all text be lowercase? Default is FALSE.
#'
#' @importFrom reticulate import
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom dplyr lead
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom tidyr fill
#' @importFrom readtextgrid read_textgrid
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish
#' @importFrom furniture washer
#' @importFrom english english
#'
#' @export
clean_up <- function(whispered1, whispered2, folder, remove_partial, hyphen, remove_apostrophe, remove_punct, lowercase){
  # grab segments
  # grab segments
  chan1 = purrr::map(whispered1, ~.x[["segments"]])
  chan2 = purrr::map(whispered2, ~.x[["segments"]])
  lengths1 = purrr::map_dbl(chan1, ~length(.x))
  lengths2 = purrr::map_dbl(chan2, ~length(.x))

  # extract text
  chan1_text = text_single(lengths1, chan1)
  chan2_text = text_single(lengths2, chan2)

  chan1_text = tolower(chan1_text)
  chan1_text = stringr::str_squish(stringr::str_remove_all(chan1_text, "\\.|\\,"))
  chan1_text = data.frame(text = chan1_text)
  chan2_text = tolower(chan2_text)
  chan2_text = stringr::str_squish(stringr::str_remove_all(chan2_text, "\\.|\\,"))
  chan2_text = data.frame(text = chan2_text)

  # grab silences file
  chan1_silences = readtextgrid::read_textgrid(fs::dir_ls(folder, regexp = "ch1.wav_silences"))
  chan2_silences = readtextgrid::read_textgrid(fs::dir_ls(folder, regexp = "ch2.wav_silences"))
  colnames(chan1_silences)[which(colnames(chan1_silences) == "xmin")] = "start"
  colnames(chan1_silences)[which(colnames(chan1_silences) == "xmax")] = "end"
  colnames(chan2_silences)[which(colnames(chan2_silences) == "xmin")] = "start"
  colnames(chan2_silences)[which(colnames(chan2_silences) == "xmax")] = "end"
  chan1_silences = chan1_silences[chan1_silences$text == "sounding", ]
  chan2_silences = chan2_silences[chan2_silences$text == "sounding", ]
  chan1_silences = chan1_silences[, -which(colnames(chan1_silences) == "text")]
  chan2_silences = chan2_silences[, -which(colnames(chan2_silences) == "text")]

  # join with text
  chan1_joined = cbind(chan1_silences, chan1_text)
  chan2_joined = cbind(chan2_silences, chan2_text)

  # channels
  chan1_joined$channel = 1
  chan2_joined$channel = 2

  # add "n" for non-speech
  non1 = chan1_joined
  non1$start1 = non1$end
  non1$end1 = dplyr::lead(non1$start)
  non1 = dplyr::select(non1, start = start1, end = end1)
  non2 = chan2_joined
  non2$start1 = non2$end
  non2$end1 = dplyr::lead(non2$start)
  non2 = dplyr::select(non2, start = start1, end = end1)
  non1$text = "n"
  non2$text = "n"
  non1$channel = 1
  non2$channel = 2
  non1 = unique(non1)
  non2 = unique(non2)

  begin1 = dplyr::mutate(chan1_joined, end = min(start), start = 0, text = "n", channel = 1)
  begin1 = dplyr::select(begin1, file, start, end, text, channel)
  begin2 = dplyr::mutate(chan2_joined, end = min(start), start = 0, text = "n", channel = 2)
  begin2 = dplyr::select(begin2, start, end, text, channel)
  begin1 = unique(begin1)
  begin2 = unique(begin2)

  # combine
  chan1_joined = dplyr::bind_rows(list(chan1_joined, non1, begin1))
  chan2_joined = dplyr::bind_rows(list(chan2_joined, non2, begin2))
  chan1_joined = dplyr::arrange(chan1_joined, start)
  chan2_joined = dplyr::arrange(chan2_joined, start)
  chan1_joined = tidyr::fill(chan1_joined, file:tier_xmax, .direction = "updown")
  chan2_joined = tidyr::fill(chan2_joined, file:tier_xmax, .direction = "updown")

  # bind
  final = dplyr::bind_rows(list(chan1_joined, chan2_joined))
  final$channel = as.numeric(final$channel)
  final$text = gsub("\\.|\\?", " ", final$text)
  final$text = gsub("\\,", "", final$text)
  final$text = gsub("\\bok\\b", "okay", final$text)
  final$text = gsub("mm\\-hmm", "mmhmm", final$text)
  final$text = gsub("uh\\-huh", "uhhuh", final$text)
  final$text = gsub("\\bk\\b", "kay", final$text)

  # numbers
  final$text = convert_numerals_to_words(final$text)

  # options
  if (remove_partial)
    final$text = gsub("\\b\\w+-\\s*", "", final$text)
  if (hyphen == "space")
    final$text = gsub("\\-", " ", final$text)
  if (hyphen == "remove")
    final$text = gsub("\\-", "", final$text)
  if (remove_apostrophe)
    final$text = gsub("\\'", "", final$text)
  if (remove_punct)
    final$text = gsub("[^[:alnum:]'\\s-]", " ", final$text)
  if (lowercase)
    final$text = tolower(final$text)

  # clean up
  final$text = stringr::str_squish(final$text)
  final$end = furniture::washer(final$end, is.na, value = max(final$end, na.rm=TRUE))
  final = unique(final)
  return(final)
}


# numerals to words
convert_numerals_to_words <- function(text) {
  # Define a function to replace a single match
  replace_function <- function(match) {
    as.character(english::english(as.numeric(match)))
  }

  # Use stringr's str_replace_all with the replace function
  stringr::str_replace_all(text, "\\d+", replace_function)
}


# clean up text for segments
text_single = function(lens, text){
  output = vector(mode = "character", length = length(lens))
  for (i in seq_along(text)){
    if (lens[i] == 0){
      output[i] = "NA"
    } else if (lens[i] > 0){
      for (y in 1:lens[i]){
        output[i] = paste(output[i], text[[i]][y][[1]]$text, collapse = " ")
      }
    }
  }
  return(output)
}
