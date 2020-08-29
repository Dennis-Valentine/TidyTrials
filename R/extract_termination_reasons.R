#' Extract Termination Reason
#'
#' Extract the resons why a trial was terminated, if provided.
#'
#' @param trial_path An absolute or relative path to the XML file
#'
#' @return
#'
#' Returns a tidy dataframe with the basic trial metadata (a trial's NCT number,
#' trial status, phase and title) with the reasons why the trial stopped.
#'
#' @export
#'
#' @examples
#'
#' ct_files <- list.files(path = "Benchmark_data/", pattern = "\\.xml", recursive = TRUE, full.names = TRUE)
#' k <- lapply(X = ct_files, FUN = extract_termination_reason)
#' k <- dplyr::bind_rows(k)
#' View(k)
#'
extract_termination_reason <- function(trial_path){

  # Check to see if the input is a XML file
  if(!grepl(pattern = "NCT.*xml$", x = trial_path)){
    stop("Path is not an XML file. File should start with 'NCT' and end with '.XML'")
  }

  data <- xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  data_root <- xmlRoot(data)

  # Get housekeeping data
  #source(file = "R/Internal_housekeeping.R")
  Housekeeping <- CTGov::Housekeeping
  house_keeping <- Housekeeping(trial_path)


  # Scope
  why_stopped_df <- data.frame("why_stopped" = "Not Terminated")

  # Check to see if the trial is terminated
  if(house_keeping$Trial.Status == "Terminated"){ # If the trial is terminated

    tryCatch(expr = {
      why_stopped_present <- xpathSApply(doc = data_root, path = "//why_stopped")
      why_stopped_df <- xmlToDataFrame(doc = why_stopped_present, nodes = why_stopped_present)
      names(why_stopped_df) <- "why_stopped"

    }, error = function(e){
      message("No termination reason given")
      why_stopped_df <<- data.frame("why_stopped" = "No reason given")
      }
    )

    termination <- cbind(house_keeping, why_stopped_df)
    return(termination)

    } else { # If the trial is not terminated
    termination <- cbind(house_keeping, why_stopped_df)
    return(termination)
  }

}
