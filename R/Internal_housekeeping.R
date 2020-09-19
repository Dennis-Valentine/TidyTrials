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
#' @importFrom XML xmlTreeParse xmlRoot xpathApply xmlValue
#'
#' @examples
#' ct_files <- example_file <- system.file("extdat", "NCT00160147.xml", package = "TidyTrials")
#' Housekeeping(ct_files)


Housekeeping <- function(trial_path){

  # Check to see if the input is a XML file
  if(!grepl(pattern = "(NCT).*xml$", x = trial_path)){
    stop(crayon::cyan("Path is not a ClinicalTrials.Gov XML file.\nFiles should start with 'NCT' and end with '.XML'"))
  }

  # If the file is correct
  temp_xml_tree <- XML::xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  temp_xml_tree_rooted <<- XML::xmlRoot(temp_xml_tree)

  # Save the results
  results <- data.frame("NCT" = NA ,
                        "Trial_Status" = NA ,
                        "Phase" = NA,
                        "Brief_Title" = NA  )

  # Nodes to extract
  vars <- paste0("//", c("nct_id", "overall_status", "phase", "brief_title"))

  # One generic function to rule them all.
  housekeeping_func <- function(x){
    tryCatch(expr =
               {results[[x]] <- as.character(
                 XML::xpathApply(doc = temp_xml_tree,
                                 path = vars[x],
                                 XML::xmlValue)
                 )},
             error = function(e){ })
    }

  results[[1]] <- housekeeping_func(1)
  results[[2]] <- housekeeping_func(2)
  results[[3]] <- housekeeping_func(3)
  results[[4]] <- housekeeping_func(4)

  return(results)

}



