% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_clean_up.R
\name{clean_up}
\alias{clean_up}
\title{Clean Up Whisper Output}
\usage{
clean_up(
  whispered1,
  whispered2,
  folder,
  remove_partial,
  hyphen,
  remove_apostrophe,
  remove_punct,
  lowercase,
  nonspeech
)
}
\arguments{
\item{whispered1}{channel one whisper data output}

\item{whispered2}{channel two whisper data output}

\item{folder}{the folder where the files are located}

\item{remove_partial}{Should the model keep words that are incomplete at the end of the sentence? Default is FALSE.}

\item{hyphen}{Should hyphens be retained or replaced? Options are "space" (hyphens are replaced with a space), "keep" (the hyphens are retained), "remove" the hyphens are removed with no white space added.}

\item{remove_apostrophe}{Should all apostraphes be removed? Default is FALSE.}

\item{remove_punct}{Should all punctuation be removed (other than hyphens and apostrophes)? Default is FALSE.}

\item{lowercase}{Should all text be lowercase? Default is FALSE.}

\item{nonspeech}{What symbol should be used for non-speech? Default = "n" but can be any string.}
}
\description{
Cleans up the data to get it ready for making
the TextGrid file.
}
