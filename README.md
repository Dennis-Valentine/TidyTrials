<!-- badges: start -->
[![Travis build status](https://travis-ci.com/Dennis-Valentine/TidyTrials.svg?branch=master)](https://travis-ci.com/Dennis-Valentine/TidyTrials)
<!-- badges: end -->

# TidyTrials

This is the TidyTrials package. This package is essentially an XML wrapper to extract and tidy trials from ClinicalTrials.Gov. TidyTrials exists because clinical trials have a broad audience but the data is currently inaccessible to researchers who might be unfamiliar with parsing XML files (i.e. statisticians, geneticists and maybe some computational biologists). I have used this codebase/package as part of my PhD so I know the functions work. However, this is my first R package so I haven't abided to best practices - there is a stronger focus on scientific goals than operational goals. I intend to maintain the codebase and improve it as a side project, but the functions might occasionally throw warning messages.

# Install 
Install the package and browse the vignettes with:

```
devtools::install_github(repo = "https://github.com/Dennis-Valentine/TidyTrials", 
 build_vignettes = TRUE, force = TRUE) 

library(TidyTrials) 
browseVignettes(package = "TidyTrials")

```
