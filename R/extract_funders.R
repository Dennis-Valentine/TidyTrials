#' Extract Conditions
#'
#' Extract the MESH condtions assigned to the trials. CAUTION:  The MeSH terms
#' are assigned with an imperfect algorithm.
#'
#' @param trial_path An absolute or relative path to the XML file
#'
#' @return Returns a tidy dataframe with the basic trial metadata (a trial's NCT
#'   number, trial status, phase and title) with funders behind the trial.

#' @export

extract_funders <- function(trial_path){

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
  funders_df <- data.frame(agency = "Missing")

  # Conditions
  tryCatch(expr = {
    lead_sponsors <- XML::xpathApply(doc = data_root,
                                     path = ".//sponsors/lead_sponsor") %>%
      xmlToDataFrame() %>%
      mutate(type = "Lead Sponsor")


    collaborator <- XML::xpathApply(doc = data_root,
                                    path = ".//sponsors/collaborator") %>%
      xmlToDataFrame() %>%
      mutate(type = "collaborator")

    funders_df <- bind_rows(lead_sponsors, collaborator)  },
          error = function(e){  }
    )

  results <- cbind(house_keeping, funders_df)
  return(results)
}

