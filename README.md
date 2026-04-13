
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wav2textgrid

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Human-Interaction-Lab/wav2textgrid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Human-Interaction-Lab/wav2textgrid/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `wav2textgrid` is to take a two channel `.wav` file of a
conversation and turn it into a transcribed TextGrid ready for further
analysis in R or Praat. To use the package, you’ll need `R`, `Praat`,
and `python` installed.

## Installation

You can install the development version of wav2textgrid from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("Human-Interaction-Lab/wav2textgrid")
```

## Example

### Python Environment Set Up

``` r
library(wav2textgrid)
py_setup(path)
# this runs the following
# wav2textgrid relies on the python (the OpenAI whisper model)
# reticulate::use_miniconda("path/r-miniconda-arm64")
# if the following python packages are not installed, use:
# packages = c("ffmpeg-python", "numpy", "scipy", "setuptools-rust", "pydub", "llvmlite", "librosa", "numba",
#              "Cmake", "wheel", "setuptools-rust", "pytorch", "torchvision")
# reticulate::conda_install(envname = "~/Desktop/Automation/r-py-automate", packages = packages)
# reticulate::py_install("openai-whisper", pip = TRUE, pip_options = "-U")
# reticulate::py_install("light-the-torch", pip = TRUE, pip_options = "-U")
```

### Run `auto_textgrid()`

``` r
wav_file = "path/example.wav"
auto_textgrid(wav_file)
```

    #> → Default prompt:
    #> 'I was like, was like, I'm like, um, ah, huh, and so, so um, uh, and um, mm-hmm, like um, so like, like it's, it's like, i mean, yeah, uh-huh, hmm, right, ok so, uh so, so uh, yeah so, you know, it's uh, uh and, and uh'
    #> ✔ Step 1 of 5 [2.6s]
    #> ✔ Step 2 of 5 [1.8s]
    #> ✔ Step 3 of 5 [5m 11.3s]                                                
    #> ✔ Step 4 of 5 [262ms]
    #> ✔ Step 5 of 5 [68ms]
    #> ℹ Written to path/example_output.TextGrid

The output is a textgrid that can be loaded directly into Praat or can
be read into R via `readtextgrid` package. For the wav file in the
example above (found on GitHub in the `inst` folder), running the code
above gives us the following TextGrid (as shown by reading it in using
`readtextgrid`).

    #> # A tibble: 254 × 6
    #>    tier_num tier_xmin tier_xmax  xmin  xmax text                                
    #>       <int>     <dbl>     <dbl> <dbl> <dbl> <chr>                               
    #>  1        1         0      281.  0     1.49 n                                   
    #>  2        1         0      281.  1.49  4.43 i am participant one on channel one 
    #>  3        1         0      281.  4.43  8.31 n                                   
    #>  4        1         0      281.  8.31  8.96 ok                                  
    #>  5        1         0      281.  8.96  9.48 n                                   
    #>  6        1         0      281.  9.48 11.7  um you want to start in the top lef…
    #>  7        1         0      281. 11.7  15.4  n                                   
    #>  8        1         0      281. 15.4  15.7  yes                                 
    #>  9        1         0      281. 15.7  17.8  n                                   
    #> 10        1         0      281. 17.8  19.4  mine says peggy sue                 
    #> # ℹ 244 more rows

Importantly, we can control a number of features of the transcription to
make it more accurate. In this case, we used a noise reduction program
and set the threshold of silences to -30 dB. We also use the larger
“small” model from Whisper.

``` r
auto_textgrid(wav_file, noise_reduction = TRUE, threshold = -30, model_type = "small")
```

We do recommend the “small” model or larger for more accurate
transcription, especially when using the default prompt.
