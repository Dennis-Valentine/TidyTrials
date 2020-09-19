## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(TidyTrials)

## ---- reading-files-----------------------------------------------------------
# This reads in one of the example files
example_file <- system.file("extdat", "NCT00160147.xml", package = "TidyTrials")


## ---- Eligibility-------------------------------------------------------------
Eligibility <- extract_eligibility(trial_path = example_file)
knitr::kable(Eligibility)


## ---- Outcomes----------------------------------------------------------------
Outcomes <- extract_outcome(trial_path = example_file)
knitr::kable(Outcomes)


## ---- Conditions--------------------------------------------------------------
Conditions <- extract_conditions(trial_path = example_file)
knitr::kable(Conditions)


