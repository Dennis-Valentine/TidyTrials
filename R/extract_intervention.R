#' Extract Interventions
#'
#' Function to extract the interventions used in the trials.
#'
#' @param trial_path An absolute or relative path to the XML file
#'
#' @return Returns a tidy dataframe with the basic trial metadata (a trial's NCT
#'   number, trial status, phase and title) and the intervention type; the
#'   intervention name, the group the intervention is given to, a description of
#'   the intervention and synonyms for the intervention.
#'
#' @export
#'
#' @examples
#' ct_files <- list.files(path = "Benchmark_data/", pattern = "\\.xml", recursive = TRUE, full.names = TRUE)
#' k <- lapply(X = ct_files, FUN = extract_interventions)
#' k <- dplyr::bind_rows(k)
#' View(k)
#'
extract_interventions <- function(trial_path){

  # Check to see if the input is a XML file
  if(!grepl(pattern = "NCT.*xml$", x = trial_path)){
    stop("Path is not an XML file. File should start with 'NCT' and end with '.XML'")
  }
  # We need to check if the XML is malformed - i.e. check against the DTD.


  # This is boiler plate code because Housekeeping() does this as well
  data <- xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  data_root <- xmlRoot(data)

  # Get housekeeping data
  #source(file = "R/Internal_housekeeping.R")
  Housekeeping <- TidyTrials::Housekeeping
  house_keeping <- Housekeeping(trial_path)

  # Intervention
  tryCatch(
    expr = {
      intervention <- xpathApply(doc = data_root, path = ".//intervention", xmlToList)
      intervention <- bind_rows(intervention)
    },
    error = function(e){ }
  )

  results <- cbind(house_keeping, intervention)
  return(results)
}
