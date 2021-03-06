#' Extract Conditions
#'
#' Extract the MESH condtions assigned to the trials. CAUTION:  The MeSH terms
#' are assigned with an imperfect algorithm.
#'
#' @param trial_path An absolute or relative path to the XML file
#'
#' @return Returns a tidy dataframe with the basic trial metadata (a trial's NCT
#'   number, trial status, phase and title) with the MESH terms associated to
#'   the trial.
#' @export

extract_conditions <- function(trial_path){

  # Check to see if the input is a XML file
  if(!grepl(pattern = "NCT.*xml$", x = trial_path)){
    stop("Path is not an XML file. File should start with 'NCT' and end with '.XML'")
  }
  # We need to check if the XML is malformed - i.e. check against the DTD.


  # This is boiler plate code because Housekeeping() does this as well
  data <- XML::xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  data_root <- XML::xmlRoot(data)

  # Get housekeeping data
  #source(file = "R/Internal_housekeeping.R")
  Housekeeping <- TidyTrials::Housekeeping
  house_keeping <- Housekeeping(trial_path)

  # Scope
  conditions_df <- data.frame("Ontology" = "MeSH", "Conditions" = NA)

  # Conditions
  tryCatch(expr = {
    conditions <- XML::xpathSApply(doc = data_root,
                                   path = ".//condition_browse/mesh_term",
                                   xmlValue)
    conditions_df <- data.frame("Ontology" = "MeSH", "Conditions" = conditions)},
          error = function(e){  }
    )

  results <- cbind(house_keeping, conditions_df)
  return(results)
}

