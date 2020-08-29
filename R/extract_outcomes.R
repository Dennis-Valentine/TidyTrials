#' Extract Outcomes
#'
#' Function to extract the primary and secondary outcomes.
#'
#' @param trial_path An absolute or relative path to the XML file
#' @param primary_outcome Logical. If TRUE the primary outcome is extracted.
#' @param secondary_outcome Logical. If TRUE the secondary outcome is extracted.
#'
#' @return
#'
#' Returns a tidy dataframe with the basic trial metadata (a trial's NCT number,
#' trial status, phase and title) with the primary and/or secondary outcome.
#'
#' @export
#'
#' @examples
#'
#' ct_files <- list.files(path = "Benchmark_data/", pattern = "\\.xml", recursive = TRUE, full.names = TRUE)
#' k <- lapply(X = ct_files, FUN = extract_outcome)
#' k <- dplyr::bind_rows(k)
#' View(k)
#'
extract_outcome <- function(trial_path, primary_outcome = TRUE, secondary_outcome = TRUE){

  # Check to see if the input is a XML file
  if(!grepl(pattern = "NCT.*xml$", x = trial_path)){
    stop("Path is not an XML file. File should start with 'NCT' and end with '.XML'")
  }

  data <- xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  data_root <- xmlRoot(data)

  # Get housekeeping data
  #source(file = "R/Internal_housekeeping.R")
  Housekeeping <- TidyTrials::Housekeeping
  house_keeping <- Housekeeping(trial_path)


  # Scope
  primary_outcome_df <- data.frame()
  secondary_outcome_df <- data.frame()

  # Working with the primary outcome
  if(primary_outcome == TRUE){
    tryCatch(expr = {
                primary_outcome_present <- xpathSApply(doc = data_root, path = "//primary_outcome")
                primary_outcome_df <- xmlToDataFrame(doc = primary_outcome_present, nodes = primary_outcome_present)
                primary_outcome_df$Type = "Primary"
      },
             error = function(e){
               message("Primary Outcome is missing from XML file. They might not have provided it.")
               primary_outcome_df <<- data.frame("Type" = "Primary")
             })
    }


  # Working with the secondary outcome
  if(secondary_outcome == TRUE){
    tryCatch(expr = {
      secondary_outcome_present <- xpathSApply(doc = data_root, path = "//secondary_outcome")
      secondary_outcome_df <- xmlToDataFrame(doc = secondary_outcome_present, nodes = secondary_outcome_present)
      secondary_outcome_df$Type = "Secondary"
    },
             error = function(e){
               message("Secondary Outcome is missing from XML file. They might not have provided it.")
               secondary_outcome_df <<- data.frame("Type" = "Secondary")

             })
  }


  all_outcomes <- dplyr::bind_rows(primary_outcome_df, secondary_outcome_df)
  all_outcomes <- cbind(house_keeping, all_outcomes)
  all_outcomes
}
