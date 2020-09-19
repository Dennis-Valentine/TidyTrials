library(TidyTrials)
library(testthat)


# Testing housekeeping ----------------------------------------------------
context("Testing Housekeeping")

test_that(desc = "internal_houskeeping checks the files are XML", code = {
  non_xml <- "some_file.txt"
  invalid_XML <- "some_xml.xml"
  correct_file <- system.file("extdat", "NCT00160147.xml", package = "TidyTrials")

  expect_error(Housekeeping(non_xml), "Path is not a ClinicalTrials.Gov XML file.\nFiles should start with 'NCT' and end with '.XML'")

  expect_error(Housekeeping(invalid_XML), "Path is not a ClinicalTrials.Gov XML file.\nFiles should start with 'NCT' and end with '.XML'")

  expect_equal(class(Housekeeping(correct_file)), expected = "data.frame")

})


test_that("internal_housekeeping extracts the NCT, trial status, phase and brief title as a data frame", {
  example_file <- system.file("extdat", "NCT00160147.xml", package = "TidyTrials")
  expect_identical(Housekeeping(example_file), object = TidyTrials::test_NCT00160147_xml)

})






context("Extracting outcomes")

test_that("extract_outcomes extracts the correct outcomes", {
  example_file <- system.file("extdat", "NCT00160147.xml", package = "TidyTrials")

  expect_equal(extract_outcome(trial_path = example_file, outcome = "primary")$measure,
               "Brief Psychiatric Rating Scale (BPRS) Total Score")

  expect_equal(extract_outcome(trial_path = example_file, outcome = "secondary")$measure,
               "Adverse events")


})

test_that("extract_outcomes formats data into data.frame", {
  example_file <- system.file("extdat", "NCT00160147.xml", package = "TidyTrials")

  expect_equal(class(extract_outcome(trial_path = example_file)), class(data.frame()))

})
