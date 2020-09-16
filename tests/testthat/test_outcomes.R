context("Extracting outcomes")
library(TidyTrials)

test_that("extract_outcomes extracts the correct outcomes", {
  example_file <- system.file("extdat", "NCT00160147.xml", package = "TidyTrials")

  expect_equal(extract_outcome(trial_path = example_file,
                               primary_outcome = TRUE,
                               secondary_outcome = FALSE)$measure,
               "Brief Psychiatric Rating Scale (BPRS) Total Score")

  expect_equal(extract_outcome(trial_path = example_file, primary_outcome = FALSE,
                               secondary_outcome = TRUE)$measure,
               "Adverse events")


})

test_that("extract_outcomes formats data into data.frame", {
  example_file <- system.file("extdat", "NCT00160147.xml", package = "TidyTrials")

  expect_equal(class(extract_outcome(trial_path = example_file)), class(data.frame()))

})
