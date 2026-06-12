

test_that("full package call", {
  # probably check individual functions, auto_textgrid is too long
})


# helper: write a short synthetic wav (sine tones) into a fresh temp dir so
# tmp/ folders and channel files don't collide between tests
synth_wav <- function(channels = 2){
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  wav_file <- file.path(dir, "synth.wav")
  t <- seq(0, 1, length.out = 16000)
  left <- round(sin(2 * pi * 440 * t) * 16000)
  if (channels == 2){
    right <- round(sin(2 * pi * 220 * t) * 16000)
    wav <- tuneR::Wave(left = left, right = right, samp.rate = 16000, bit = 16)
  } else {
    wav <- tuneR::Wave(left = left, samp.rate = 16000, bit = 16)
  }
  tuneR::writeWave(wav, wav_file)
  wav_file
}

test_that("split_channels splits a stereo file into two channels", {
  wav <- synth_wav(channels = 2)
  chs <- split_channels(wav)
  expect_length(chs, 2)
  expect_true(all(fs::file_exists(chs)))
  expect_equal(tuneR::nchannel(tuneR::readWave(chs[1])), 1)
  expect_equal(tuneR::nchannel(tuneR::readWave(chs[2])), 1)
})

test_that("split_channels auto-detects a mono file", {
  wav <- synth_wav(channels = 1)
  chs <- split_channels(wav)
  expect_length(chs, 1)
  expect_true(fs::file_exists(chs))
  expect_equal(tuneR::nchannel(tuneR::readWave(chs)), 1)
})

test_that("split_channels mixes a stereo file down when channels = 'mono'", {
  wav <- synth_wav(channels = 2)
  chs <- split_channels(wav, channels = "mono")
  expect_length(chs, 1)
  expect_equal(tuneR::nchannel(tuneR::readWave(chs)), 1)
})

test_that("split_channels errors when stereo is required but file is mono", {
  wav <- synth_wav(channels = 1)
  expect_error(split_channels(wav, channels = "stereo"), "2 channels")
})

test_that("make_textgrid writes a valid two tier textgrid", {
  dir <- withr::local_tempdir()
  wav <- file.path(dir, "fake.wav")
  data <- data.frame(
    start = c(0, 1.5, 0, 2.5),
    end = c(1.5, 3.0, 2.5, 3.0),
    text = c("n", "hello there", "n", "hi"),
    channel = c(1, 1, 2, 2)
  )
  make_textgrid(data, wav)

  out <- file.path(dir, "fake_output.TextGrid")
  expect_true(fs::file_exists(out))
  lines <- readLines(out)
  expect_true(any(grepl("^size = 2$", lines)))

  tg <- readtextgrid::read_textgrid(out)
  expect_equal(sort(unique(tg$tier_name)), c("one", "two"))
})

test_that("make_textgrid writes a valid single tier textgrid", {
  dir <- withr::local_tempdir()
  wav <- file.path(dir, "fake.wav")
  data <- data.frame(
    start = c(0, 1.5),
    end = c(1.5, 3.0),
    text = c("n", "hello there"),
    channel = c(1, 1)
  )
  make_textgrid(data, wav)

  out <- file.path(dir, "fake_output.TextGrid")
  expect_true(fs::file_exists(out))
  lines <- readLines(out)
  expect_true(any(grepl("^size = 1$", lines)))

  tg <- readtextgrid::read_textgrid(out)
  expect_equal(unique(tg$tier_name), "one")
  expect_equal(unique(tg$tier_num), 1)
})
