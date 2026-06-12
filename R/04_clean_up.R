#' @title Clean Up Whisper Output
#'
#' @description Cleans up the data to get it ready for making
#' the TextGrid file.
#'
#' @param whispered1 channel one whisper data output
#' @param whispered2 channel two whisper data output (NULL for single channel wav files)
#' @param folder the folder where the files are located
#' @param remove_partial Should the model keep words that are incomplete at the end of the sentence? Default is FALSE.
#' @param hyphen Should hyphens be retained or replaced? Options are "space" (hyphens are replaced with a space), "keep" (the hyphens are retained), "remove" the hyphens are removed with no white space added.
#' @param remove_apostrophe Should all apostraphes be removed? Default is FALSE.
#' @param remove_punct Should all punctuation be removed (other than hyphens and apostrophes)? Default is FALSE.
#' @param lowercase Should all text be lowercase? Default is FALSE.
#' @param nonspeech What symbol should be used for non-speech? Default = "n" but can be any string.
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
#' @importFrom english english
#'
#' @export
clean_up <- function(whispered1, whispered2 = NULL, folder, remove_partial, hyphen, remove_apostrophe, remove_punct, lowercase, nonspeech){
  whispered = list(whispered1, whispered2)
  whispered = whispered[!vapply(whispered, is.null, logical(1))]

  # per channel: join whisper text with silence timings and pad non-speech
  joined = vector("list", length = length(whispered))
  for (chan in seq_along(whispered)){
    joined[[chan]] = clean_channel(whispered[[chan]], chan, folder, nonspeech)
  }

  # bind
  final = dplyr::bind_rows(joined)
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
  final$end[is.na(final$end)] = max(final$end, na.rm = TRUE)
  final = unique(final)
  return(final)
}


# join one channel's whisper text with its silence timings and pad non-speech
clean_channel <- function(whispered, chan, folder, nonspeech){
  # grab segments
  segs = purrr::map(whispered, ~.x[["segments"]])
  lengths = purrr::map_dbl(segs, ~length(.x))

  # extract text
  chan_text = text_single(lengths, segs)
  chan_text = tolower(chan_text)
  chan_text = stringr::str_squish(stringr::str_remove_all(chan_text, "\\.|\\,"))
  chan_text = data.frame(text = chan_text)

  # grab silences file
  silences = readtextgrid::read_textgrid(fs::dir_ls(folder, regexp = paste0("ch", chan, ".wav_silences")))
  colnames(silences)[which(colnames(silences) == "xmin")] = "start"
  colnames(silences)[which(colnames(silences) == "xmax")] = "end"
  silences = silences[silences$text == "sounding", ]
  silences = silences[, -which(colnames(silences) == "text")]

  # join with text
  chan_joined = cbind(silences, chan_text)
  chan_joined$channel = chan

  # add non-speech between sounding intervals
  non = chan_joined
  non$start1 = non$end
  non$end1 = dplyr::lead(non$start)
  non = dplyr::select(non, start = start1, end = end1)
  non$text = nonspeech
  non$channel = chan
  non = unique(non)

  # add non-speech before the first sounding interval
  begin = dplyr::mutate(chan_joined, end = min(start), start = 0, text = nonspeech, channel = chan)
  begin = dplyr::select(begin, file, start, end, text, channel)
  begin = unique(begin)

  # combine
  chan_joined = dplyr::bind_rows(list(chan_joined, non, begin))
  chan_joined = dplyr::arrange(chan_joined, start)
  chan_joined = tidyr::fill(chan_joined, file:tier_xmax, .direction = "updown")
  return(chan_joined)
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
