# Whisper step: language is passed through, a failing segment does not end the
# run, and prompt echoes are caught. Uses a fake model so no python is needed.

default_prompt <- "I was like, was like, I'm like, um, ah, huh, and so, so um, uh, and um, mm-hmm, like um, so like, like it's, it's like, i mean, yeah, uh-huh, hmm, right, ok so, uh so, so uh, yeah so, you know, it's uh, uh and, and uh"

# folder with a 6 s channel file and a silences TextGrid with three sounding intervals
whisper_folder <- function(env = parent.frame()){
  dir <- withr::local_tempdir(.local_envir = env)
  tuneR::writeWave(tuneR::Wave(rep(0L, 6 * 16000), samp.rate = 16000, bit = 16),
                   file.path(dir, "rec_ch1.wav"))
  write_silences(dir, 1, c("silence", "sounding", "silence", "sounding", "silence", "sounding", "silence"),
                 c(0, 0.5, 1.5, 2, 3, 3.5, 4.5, 6))
  dir
}

# fake whisper model: returns texts[i] on the i-th call; "ERROR" raises an error.
# Records the arguments of every call in calls$args.
fake_model <- function(texts){
  calls <- new.env()
  calls$args <- list()
  model <- list(transcribe = function(audio, ...){
    calls$args[[length(calls$args) + 1]] <- list(...)
    text <- texts[[length(calls$args)]]
    if (identical(text, "ERROR")) stop("CUDA out of memory (simulated)")
    list(text = text, segments = list(list(text = text)))
  })
  list(model = model, calls = calls)
}

run_channel <- function(dir, fake, ...){
  whisper_channel(file.path(dir, "rec_ch1.wav"), 1, dir, fake$model, prompt = default_prompt, ...)
}

test_that("language defaults to English and is passed to whisper", {
  local_mocked_bindings(wave_to_whisper = function(wave, sample_freq) wave)
  dir <- whisper_folder()

  fake <- fake_model(c("one", "two", "three"))
  run_channel(dir, fake)
  expect_true(all(vapply(fake$calls$args, function(a) identical(a$language, "en"), logical(1))))

  fake <- fake_model(c("one", "two", "three"))
  run_channel(dir, fake, language = NULL)
  expect_true(all(vapply(fake$calls$args, function(a) "language" %in% names(a) && is.null(a$language), logical(1))))
})

test_that("a failing segment is skipped with a warning naming it; the rest are kept", {
  local_mocked_bindings(wave_to_whisper = function(wave, sample_freq) wave)
  dir <- whisper_folder()
  fake <- fake_model(c("hello", "ERROR", "goodbye"))

  expect_warning(out <- run_channel(dir, fake), "failed on 1 of 3 segments.*#2 \\(2.0-3.0s\\).*CUDA out of memory")
  expect_length(out, 3)
  expect_equal(out[[1]]$text, "hello")
  expect_length(out[[2]]$segments, 0)
  expect_equal(out[[3]]$text, "goodbye")

  # downstream, the failed segment becomes non-speech
  cleaned <- clean_up(out, folder = dir, remove_partial = FALSE, hyphen = "keep",
                      remove_apostrophe = FALSE, remove_punct = FALSE, lowercase = FALSE, nonspeech = "n")
  expect_equal(cleaned$text[cleaned$start == 2], "n")
  expect_true(all(c("hello", "goodbye") %in% cleaned$text))
})

test_that("if every segment fails the run stops with whisper's error", {
  local_mocked_bindings(wave_to_whisper = function(wave, sample_freq) wave)
  dir <- whisper_folder()
  expect_error(run_channel(dir, fake_model(rep("ERROR", 3))),
               "failed on every segment of channel 1.*CUDA out of memory")
})

test_that("prompt echoes are dropped with a warning, or kept on request", {
  local_mocked_bindings(wave_to_whisper = function(wave, sample_freq) wave)
  dir <- whisper_folder()
  echo <- "like um, so like, like it's, it's like, I mean, yeah, uh-huh, hmm, right."

  expect_warning(out <- run_channel(dir, fake_model(c("hi there", echo, "bye"))),
                 "repeated the prompt.*#2 \\(2.0-3.0s\\)")
  expect_length(out[[2]]$segments, 0)
  expect_equal(out[[1]]$text, "hi there")

  expect_no_warning(out <- run_channel(dir, fake_model(c("hi there", echo, "bye")), drop_prompt_echo = FALSE))
  expect_equal(out[[2]]$text, echo)
})

test_that("is_prompt_echo flags verbatim prompt runs but not ordinary speech", {
  # echoes
  expect_true(is_prompt_echo(default_prompt, default_prompt))
  expect_true(is_prompt_echo("Uh so, so uh, yeah so, you know, it's uh, uh and.", default_prompt))
  # filler-heavy but real speech, and short utterances made of prompt words
  expect_false(is_prompt_echo("um yeah so", default_prompt))
  expect_false(is_prompt_echo("you know it's uh", default_prompt))
  expect_false(is_prompt_echo("so like I was like going to the store and um it was closed", default_prompt))
  # one prompt-like run inside a long real utterance
  expect_false(is_prompt_echo(paste("then I have another towel that's coming off the dog is pulling it off",
                                    "and so so um uh and um and then a wheelbarrow with a pig"), default_prompt))
  # missing inputs
  expect_false(is_prompt_echo(NULL, default_prompt))
  expect_false(is_prompt_echo("anything at all here really now", NULL))
})

test_that("segment wav files are no longer written to the working folder", {
  local_mocked_bindings(wave_to_whisper = function(wave, sample_freq) wave)
  dir <- whisper_folder()
  run_channel(dir, fake_model(c("a", "b", "c")))
  expect_length(fs::dir_ls(dir, regexp = "segment"), 0)
})
