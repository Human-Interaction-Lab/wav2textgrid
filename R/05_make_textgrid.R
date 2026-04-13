#' @title Make TextGrid
#'
#' @description Cleans up the data produced by Whisper and the dominant channel analysis.
#'
#' @param data cleaned data from clean_up().
#' @param wav_file The wave file
#'
#' @importFrom stringr str_remove regex
#'
#' @export
make_textgrid <- function(data, wav_file){
  textgrid_header <- paste0(
    'File type = "ooTextFile"\n',
    'Object class = "TextGrid"\n\nxmin = 0\n',
    'xmax = ', max(data$end, na.rm=TRUE), '\n',
    'tiers? <exists>\n',
    'size = 2\nitem []:\n'
  )

  chan_texgrid = list()
  for (chan in unique(data$channel)){
    d_chan = data[data$channel == chan, ]
    chan_name <- if (chan == 1) "one" else "two"

    textgrid_channel_start = paste0(
      sprintf('    item [%d]:\n', chan),
      sprintf('        class = "IntervalTier"\n        name = "%s"\n        xmin = %f\n        xmax = %f\n', chan_name, min(data$start, na.rm = TRUE), max(data$end, na.rm = TRUE)),
      '        intervals: size = ', nrow(d_chan), '\n'
    )

    textgrid_content = ""
    length_chan = length(d_chan$start)
    for (i in 1:length_chan) {
      # ensure last timing is the end of the audio file
      if (i == length_chan) {
        xmax_end = max(data$end, na.rm = TRUE)
      } else {
        xmax_end = d_chan$end[i]
      }

      textgrid_content <- paste0(
        textgrid_content,
        sprintf('        intervals [%d]:\n', i),
        sprintf('            xmin = %f\n', d_chan$start[i]),
        sprintf('            xmax = %f\n', xmax_end),
        sprintf('            text = "%s"\n', d_chan$text[i])
      )
    }

    chan_texgrid[[chan]] = paste0(textgrid_channel_start, textgrid_content)
  }

  textgrid <- paste0(textgrid_header, do.call("paste0", chan_texgrid))

  # Save the TextGrid to a file
  writeLines(textgrid, paste0(str_remove(wav_file, stringr::regex("\\.wav$", ignore_case = TRUE)), "_output.TextGrid"))
  invisible(textgrid)
}
