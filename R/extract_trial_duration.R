#' Extract Date
#'
#' Function to extract the start and end date of the trial.
#'
#' @param trial_path An absolute or relative path to the XML file
#' @return
#'
#' Returns a tidy dataframe with the basic trial metadata (a trial's NCT number,
#' trial status, phase and title) with the start and end date of the trial.
#'
#' @export
#' @importFrom lubridate floor_date ceiling_date mdy


extract_date <- function(trial_path){

  # Check to see if the input is a XML file
  if(!grepl(pattern = "NCT.*xml$", x = trial_path)){
    stop("Path is not an XML file. File should start with 'NCT' and end with '.XML'")
  }

  data <- XML::xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  data_root <- XML::xmlRoot(data)

  # Get housekeeping data
  #source(file = "R/Internal_housekeeping.R")
  Housekeeping <- TidyTrials::Housekeeping
  house_keeping <- Housekeeping(trial_path)


  # Scope
  trial_start_df <- data.frame()
  trial_end_df <- data.frame()


  # Extracting the trial start date
    tryCatch(expr = {
      trial_duration <- XML::xpathSApply(doc = data_root, path = "//start_date")
      trial_start_df <- XML::xmlToDataFrame(doc = trial_duration,
                                                  nodes = trial_duration)
      names(trial_start_df) <- "start_date"
      },
        error = function(e){
           message("The trial start date is missing from XML file. They might not have provided it.")
          trial_start_df <<- data.frame("start_date" = "Missing")
             })

  # Extracting the trial end date
  tryCatch(expr = {
    trial_duration <- XML::xpathSApply(doc = data_root, path = "//completion_date")
    trial_end_df <- XML::xmlToDataFrame(doc = trial_duration,
                                     nodes = trial_duration)
    names(trial_end_df) <- "completion_date"
  },
  error = function(e){
    message("The trial completion date is missing from XML file. They might not have provided it.")
    trial_end_df <<- data.frame("completion_date" = "Missing")
  })


  # Cleaning the date - all start dates will be floored and all end dates will
  # be ceiling'd

  trial_start_df$start_date_clean <- lubridate::floor_date(
    lubridate::mdy(trial_start_df$start_date), unit = "month")

  trial_end_df$completion_date_clean <- lubridate::ceiling_date(
    lubridate::mdy(trial_end_df$completion_date), unit = "month")

  trial_duration <- cbind(house_keeping, trial_start_df, trial_end_df)
  trial_duration$trial_duration <- trial_duration$completion_date_clean - trial_duration$start_date_clean

  trial_duration
}
