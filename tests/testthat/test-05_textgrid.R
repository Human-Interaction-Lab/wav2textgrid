# TextGrids should cover the whole recording, contain no zero-length
# intervals, and survive quotes in the transcribed text.

clean <- function(w1, folder, w2 = NULL){
  clean_up(w1, w2, folder = folder, remove_partial = FALSE, hyphen = "keep",
           remove_apostrophe = FALSE, remove_punct = FALSE, lowercase = FALSE,
           nonspeech = "n")
}

test_that("trailing silence is kept: the tier ends at the audio duration", {
  dir <- withr::local_tempdir()
  write_silences(dir, 1, c("silence", "sounding", "silence"), c(0, 1, 3, 5))
  out <- clean(fake_whisper("hello there"), dir)

  expect_equal(max(out$end), 5)
  expect_equal(out$text[out$start == 3], "n")

  make_textgrid(out, file.path(dir, "rec.wav"))
  tg <- readtextgrid::read_textgrid(file.path(dir, "rec_output.TextGrid"))
  expect_equal(max(tg$xmax), 5)
  expect_equal(unique(tg$tier_xmax), 5)
})

test_that("stereo tiers both end at the audio duration", {
  dir <- withr::local_tempdir()
  write_silences(dir, 1, c("silence", "sounding", "silence"), c(0, 1, 2, 5))
  write_silences(dir, 2, c("silence", "sounding", "silence"), c(0, 2.5, 3.5, 5))
  out <- clean(fake_whisper("hi"), dir, w2 = fake_whisper("hey"))
  expect_equal(as.vector(tapply(out$end, out$channel, max)), c(5, 5))
})

test_that("no zero-length intervals when speech starts at 0 s or runs to the end", {
  dir <- withr::local_tempdir()
  write_silences(dir, 1, c("sounding", "silence", "sounding"), c(0, 2, 3, 5))
  out <- clean(fake_whisper(c("first", "last")), dir)

  expect_true(all(out$end > out$start))
  expect_equal(out$text, c("first", "n", "last"))
  expect_equal(c(min(out$start), max(out$end)), c(0, 5))

  make_textgrid(out, file.path(dir, "rec.wav"))
  tg <- readtextgrid::read_textgrid(file.path(dir, "rec_output.TextGrid"))
  expect_true(all(tg$xmax > tg$xmin))
})

test_that("double quotes in text are escaped and read back intact", {
  dir <- withr::local_tempdir()
  data <- data.frame(start = c(0, 1), end = c(1, 3),
                     text = c("n", 'and she said "no way" to me'), channel = 1)
  make_textgrid(data, file.path(dir, "rec.wav"))

  out <- file.path(dir, "rec_output.TextGrid")
  expect_true(any(grepl('text = "and she said ""no way"" to me"', readLines(out), fixed = TRUE)))
  tg <- readtextgrid::read_textgrid(out)
  expect_equal(tg$text[2], 'and she said "no way" to me')
})

test_that("missing text is written as an empty label, not NA", {
  dir <- withr::local_tempdir()
  data <- data.frame(start = c(0, 1), end = c(1, 3), text = c("n", NA), channel = 1)
  make_textgrid(data, file.path(dir, "rec.wav"))
  lines <- readLines(file.path(dir, "rec_output.TextGrid"))
  expect_false(any(grepl('text = "NA"', lines, fixed = TRUE)))
})
