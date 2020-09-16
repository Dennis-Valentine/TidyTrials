#' Housekeeping
#'
#' Collect basic metadata from a trial such as
#' \itemize{
#'   \item NCT number
#'   \item Trial Status
#'   \item Phase
#'   \item Brief Title
#' }
#'
#' @param trial_path An absolute or relative path to the XML file
#'
#' @return Returns a dataframe with a trial's NCT number, trial status, phase
#'   and title. 1 row per trial
#'
#' @export
#'
#' @import XML
#' @import dplyr
#'
#' @examples
#' ct_files <- example_file <- system.file("extdat", "NCT00160147.xml", package = "TidyTrials")
#' Housekeeping(ct_files)


Housekeeping <- function(trial_path){

  temp_xml_tree <- XML::xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  temp_xml_tree_rooted <- XML::xmlRoot(temp_xml_tree)

  # Scope
  results <- data.frame("NCT" = NA ,
                        "Trial Status" = NA ,
                        "Phase" = NA,
                        "Brief Title" = NA  )


  tryCatch(expr = {results[[1]] <- as.character(XML::xpathApply(doc = temp_xml_tree, path = "//nct_id", xmlValue))},
           error = function(e){ })

  tryCatch(expr = {results[[2]] <- as.character(XML::xpathApply(doc = temp_xml_tree, path = "//overall_status", xmlValue))},
           error = function(e){ })

  tryCatch(expr = {results[[3]] <- as.character(XML::xpathSApply(doc = temp_xml_tree, path = "//phase", xmlValue))},
           error = function(e){ })

  tryCatch(expr = {results[[4]] <- XML::xmlValue(temp_xml_tree_rooted[["brief_title"]])},
           error = function(e){ })


  # trial_id_temp <- as.data.frame(xpathApply(doc = temp_xml_tree, path = "//nct_id", xmlValue))
  # trial_status <- as.data.frame(xpathApply(doc = temp_xml_tree, path = "//overall_status", xmlValue))
  # phase <- as.data.frame(xpathSApply(doc = temp_xml_tree, path = "//phase", xmlValue))
  # brief_title <- xmlValue(temp_xml_tree_rooted[["brief_title"]])

  #colnames(results) <- c("NCT", "Trial_Status", "Phase", "Brief_Title")


  return(results)

}



