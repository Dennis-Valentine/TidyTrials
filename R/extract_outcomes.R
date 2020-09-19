#' Extract Outcomes
#'
#' Function to extract the primary and secondary outcomes.
#'
#' @param trial_path An absolute or relative path to the XML file
#' @param outcome A character string with the outcome type (primary or secondary) that should be extracted. Default extracts both.
#'
#' @return
#'
#' Returns a tidy dataframe with the basic trial metadata (a trial's NCT number,
#' trial status, phase and title) with the primary and/or secondary outcome.
#'
#' @export
#'

extract_outcome <- function(trial_path, outcome = c("primary", "secondary")){

  data <- XML::xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  data_root <- XML::xmlRoot(data)

  # Get housekeeping data
  house_keeping <- TidyTrials::Housekeeping(trial_path)


  # Scope
  primary_outcome_df <- data.frame()
  secondary_outcome_df <- data.frame()

  # Working with the primary outcome
  if("primary" %in% outcome){
    tryCatch(expr = {
                primary_outcome_present <- XML::xpathSApply(doc = data_root,
                                                            path = "//primary_outcome")
                primary_outcome_df <- XML::xmlToDataFrame(doc = primary_outcome_present,
                                                          nodes = primary_outcome_present)
                primary_outcome_df$Type = "Primary"
      },
             error = function(e){
               message(crayon::cyan("Primary Outcome is missing from XML file. They might not have provided it."))
               primary_outcome_df <<- data.frame("Type" = "Primary")
             })
    }


  # Working with the secondary outcome
  if("secondary" %in% outcome){
    tryCatch(expr = {
      secondary_outcome_present <- XML::xpathSApply(doc = data_root, path = "//secondary_outcome")
      secondary_outcome_df <- XML::xmlToDataFrame(doc = secondary_outcome_present, nodes = secondary_outcome_present)
      secondary_outcome_df$Type = "Secondary"
    },
             error = function(e){
               message(crayon::cyan("Secondary Outcome is missing from XML file. They might not have provided it."))
               secondary_outcome_df <<- data.frame("Type" = "Secondary")

             })
  }


  all_outcomes <- dplyr::bind_rows(primary_outcome_df, secondary_outcome_df)
  all_outcomes <- cbind(house_keeping, all_outcomes)
  all_outcomes
}

