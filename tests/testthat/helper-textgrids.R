# shared test helpers (testthat loads helper-*.R files before the tests)

# write a silences TextGrid in the format get_boundaries() produces
write_silences <- function(folder, chan, labels, bounds){
  n <- length(labels)
  xmax <- max(bounds)
  intervals <- vapply(seq_len(n), function(i) sprintf(
    '        intervals [%d]:\n            xmin = %s\n            xmax = %s\n            text = "%s"\n',
    i, bounds[i], bounds[i + 1], labels[i]), character(1))
  tg <- paste0(
    'File type = "ooTextFile"\nObject class = "TextGrid"\n\nxmin = 0\nxmax = ', xmax,
    '\ntiers? <exists>\nsize = 1\nitem []:\n    item [1]:\n        class = "IntervalTier"\n',
    '        name = "silences"\n        xmin = 0\n        xmax = ', xmax,
    '\n        intervals: size = ', n, '\n', paste(intervals, collapse = "")
  )
  file <- file.path(folder, paste0("rec_ch", chan, ".wav_silences.TextGrid"))
  writeLines(tg, file)
  file
}

# mimic whisper transcribe() output: one result per segment; NA = no words heard
fake_whisper <- function(texts){
  lapply(texts, function(t){
    if (is.na(t)) list(segments = list()) else list(segments = list(list(text = t)))
  })
}
