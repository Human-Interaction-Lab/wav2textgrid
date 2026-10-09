# Regression tests: channels where Praat finds no speech, and sounding
# intervals where Whisper returns no words, should not crash the pipeline.

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

run_clean_up <- function(w1, w2 = NULL, folder, nonspeech = "n"){
  clean_up(w1, w2, folder = folder, remove_partial = FALSE, hyphen = "keep",
           remove_apostrophe = FALSE, remove_punct = FALSE, lowercase = FALSE,
           nonspeech = nonspeech)
}

test_that("whisper_channel returns an empty list (no model call) for a silent channel", {
  dir <- withr::local_tempdir()
  write_silences(dir, 1, "silence", c(0, 5))
  # model = NULL: any attempt to transcribe would error
  expect_message(
    out <- whisper_channel(file.path(dir, "rec_ch1.wav"), 1, dir, model = NULL, prompt = ""),
    "No speech detected on channel 1"
  )
  expect_identical(out, list())
})

test_that("clean_up gives a silent channel a single non-speech interval", {
  dir <- withr::local_tempdir()
  write_silences(dir, 1, c("silence", "sounding", "silence"), c(0, 1, 3, 5))
  write_silences(dir, 2, "silence", c(0, 5))

  out <- run_clean_up(fake_whisper("hello there"), list(), folder = dir)

  ch2 <- out[out$channel == 2, ]
  expect_equal(nrow(ch2), 1)
  expect_equal(c(ch2$start, ch2$end), c(0, 5))
  expect_equal(ch2$text, "n")
  expect_true("hello there" %in% out$text[out$channel == 1])
})

test_that("a silent channel still produces a readable two tier TextGrid", {
  dir <- withr::local_tempdir()
  write_silences(dir, 1, c("silence", "sounding", "silence"), c(0, 1, 3, 5))
  write_silences(dir, 2, "silence", c(0, 5))
  out <- run_clean_up(fake_whisper("hello there"), list(), folder = dir)

  wav <- file.path(dir, "rec.wav")
  make_textgrid(out, wav)
  tg <- readtextgrid::read_textgrid(file.path(dir, "rec_output.TextGrid"))
  expect_equal(sort(unique(tg$tier_name)), c("one", "two"))
  expect_equal(tg$text[tg$tier_name == "two"], "n")
})

test_that("both channels silent does not error", {
  dir <- withr::local_tempdir()
  write_silences(dir, 1, "silence", c(0, 5))
  write_silences(dir, 2, "silence", c(0, 5))
  out <- run_clean_up(list(), list(), folder = dir)
  expect_equal(nrow(out), 2)
  expect_true(all(out$text == "n"))
})

test_that("sounding intervals with no whisper words become non-speech, not 'na'", {
  dir <- withr::local_tempdir()
  write_silences(dir, 1, c("silence", "sounding", "silence", "sounding", "silence"),
                 c(0, 1, 2, 3, 4, 5))
  out <- run_clean_up(fake_whisper(c(NA, "  ")), folder = dir, nonspeech = "<sil>")
  expect_false(any(out$text %in% c("na", "NA", "")))
  expect_true(all(out$text == "<sil>"))
})

test_that("a missing silences TextGrid gives a clear error", {
  dir <- withr::local_tempdir()
  expect_error(read_silences(dir, 1), "Did Praat run successfully")
})
