# Praat calls should fail fast with clear messages instead of failing later
# (or hanging) when Praat is missing, errors, stalls, or writes nothing.

# a folder like split_channels() makes: tmp/ with one channel of tone bursts
channel_folder <- function(env = parent.frame()){
  dir <- file.path(withr::local_tempdir(.local_envir = env), "my data", "tmp")
  dir.create(dir, recursive = TRUE)
  sr <- 16000
  t <- seq(0, 6, length.out = 6 * sr)
  x <- round(sin(2 * pi * 200 * t) * 12000 * ((t > 0.5 & t < 1.5) | (t > 3 & t < 4)))
  tuneR::writeWave(tuneR::Wave(x, samp.rate = sr, bit = 16), file.path(dir, "rec_ch1.wav"))
  paste0(dir, "/")
}

# a stand-in Praat executable (a shell script) for simulating failures
fake_praat <- function(body, env = parent.frame()){
  path <- file.path(withr::local_tempdir(.local_envir = env), "praat")
  writeLines(c("#!/bin/sh", body), path)
  Sys.chmod(path, "755")
  path
}

run_boundaries <- function(folder){
  get_boundaries(folder, min_pitch = 100, time_step = 0, threshold = -25,
                 min_silent_int = 0.3, min_sound_int = 0.1)
}

test_that("a missing Praat executable stops with an actionable message", {
  withr::local_options(wav2textgrid.praat.path = "/no/such/praat")
  expect_error(run_boundaries(channel_folder()), "set_praat_path")
})

test_that("a Praat error stops with Praat's own message", {
  skip_on_os("windows")
  withr::local_options(wav2textgrid.praat.path = fake_praat(c("echo 'Error: File not recognized.' >&2", "exit 1")))
  expect_error(run_boundaries(channel_folder()), "exit status 1.*File not recognized")
})

test_that("a stalled Praat is stopped after the timeout", {
  skip_on_os("windows")
  withr::local_options(
    wav2textgrid.praat.path = fake_praat("sleep 30"),
    wav2textgrid.praat.timeout = 1
  )
  elapsed <- system.time(expect_error(run_boundaries(channel_folder()), "did not finish within 1 seconds"))
  expect_lt(elapsed[["elapsed"]], 10)
})

test_that("Praat exiting cleanly without output is caught", {
  skip_on_os("windows")
  withr::local_options(wav2textgrid.praat.path = fake_praat("exit 0"))
  expect_error(run_boundaries(channel_folder()), "did not write the silences TextGrid for: rec_ch1.wav")
})

test_that("set_praat_path accepts a macOS app bundle and warns on a bad path", {
  withr::local_options(wav2textgrid.praat.path = NULL)
  app <- file.path(withr::local_tempdir(), "Praat.app")
  dir.create(file.path(app, "Contents", "MacOS"), recursive = TRUE)
  file.create(file.path(app, "Contents", "MacOS", "Praat"))

  expect_equal(set_praat_path(app), file.path(app, "Contents", "MacOS", "Praat"))
  expect_equal(getOption("wav2textgrid.praat.path"), file.path(app, "Contents", "MacOS", "Praat"))
  expect_message(set_praat_path("/no/such/praat"), "No Praat executable found")
})

test_that("default_praat_path always returns a single path", {
  path <- default_praat_path()
  expect_type(path, "character")
  expect_length(path, 1)
})

test_that("get_boundaries works with real Praat in a folder with spaces", {
  praat <- default_praat_path()
  skip_if_not(file.exists(praat), "Praat not installed")
  withr::local_options(wav2textgrid.praat.path = praat)
  folder <- channel_folder()
  expect_equal(run_boundaries(folder), 1)
  sil <- read_silences(folder, 1)
  expect_equal(sum(sil$text == "sounding"), 2)
})
