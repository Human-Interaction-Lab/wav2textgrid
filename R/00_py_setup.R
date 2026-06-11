#' @title Python Set Up
#'
#' @description Optional helper that creates a persistent conda environment
#' containing the python dependencies (the OpenAI Whisper model).
#'
#' Manual setup is no longer required: if no python environment has been set up,
#' the package automatically provisions one the first time the Whisper model is
#' used by calling `reticulate::py_require("openai-whisper", python_version = "3.11")`.
#' Use `py_setup()` only if you prefer a persistent, named conda environment,
#' e.g. on machines where the automatic provisioning is not possible.
#'
#' @param path The name of (or path to) the conda environment
#' @param install Whether to create the conda environment if it does not already exist (default = TRUE)
#'
#' @import reticulate
#' @importFrom cli cli_process_start
#' @importFrom cli cli_process_done
#'
#' @export
py_setup <- function(path, install = TRUE){
  cli::cli_process_start(msg = "Begin python environment set up")
  if (install && !reticulate::condaenv_exists(path)) {
    reticulate::conda_create(envname = path, python_version = "3.11")
  }
  reticulate::use_condaenv(path, required = TRUE)
  # openai-whisper pulls in everything it needs (torch, numpy, numba, tiktoken)
  reticulate::py_install("openai-whisper", envname = path, pip = TRUE, pip_options = "-U")
  cli::cli_process_done(msg_done = "Completed python environment set up")
}


# declare python requirements so reticulate provisions an environment
# automatically (via uv) when the user has not bound one themselves;
# a user-specified environment (use_condaenv(), py_setup(), etc.) takes precedence
ensure_whisper <- function(){
  if (!reticulate::py_available()){
    reticulate::py_require("openai-whisper", python_version = "3.11")
  }
  invisible(NULL)
}
