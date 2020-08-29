#Building a function to extract the locations of the trials
#' Extract locations
#'
#' Extract the locations of the trials
#'
#' @param trial_path
#'
#' @return
#' @export
#'
#' @examples
#'
#' ct_files <- list.files(path = "Benchmark_data/", pattern = "\\.xml", recursive = TRUE, full.names = TRUE)
#' k <- lapply(X = ct_files, FUN = extract_locations)
#' k <- dplyr::bind_rows(k)
#' View(k)
#'
extract_locations <- function(trial_path){
  library(XML)
  library(dplyr)

  # Check to see if the input is a XML file
  if(!grepl(pattern = "NCT.*xml$", x = trial_path)){
    stop("Path is not an XML file. File should start with 'NCT' and end with '.XML'")
  }

  data <- xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  data_root <- xmlRoot(data)

  # Collect housekeeping data on the trial
  Housekeeping <- TidyTrials::Housekeeping
  house_keeping <- Housekeeping(trial_path)

  # For some reason, location data ends up in 2 formats: a list or a matrix. I don't know why this happens
  location_data <- xpathSApply(doc = data_root, path = "//facility", xmlToDataFrame)

  if(class(location_data) == "list"){   # If the result is a list...

    # Given a list of df, coalesce them
    tryCatch(expr = location_data <- lapply(location_data, FUN = function(x){coalesce(x[1,], x[2,])}),
             error = function(e){   message(trial_path) }
             ) # coalesce TryCatch

    location_data <- bind_rows(location_data)

    }else{ #If the result is a matrix

    location_name <- xpathSApply(doc = data_root, path = "//facility/name")
    location_name <- xmlToDataFrame(location_name, nodes = list("name"))
    if(ncol(location_name) == 0){
      location_name = NA
    }

    location_address <- xpathSApply(doc = data_root, path = "//facility/address")
    location_address <- xmlToDataFrame(location_address, nodes = list("address"))
    if(ncol(location_address) == 0){
      location_address = NA
    }

    location_data <- cbind(location_name, location_address)

    }

  # If no location data is provided
  # if(nrow(location_data) == 0){
  #   location_data <- data.frame("NCT" = xpathSApply(doc = data_root, path = "//nct_id", fun = xmlValue),
  #                               "Phase" = xpathSApply(doc = data_root, path = "//phase", fun = xmlValue))
  # } else {
  #   location_data$NCT <- xpathSApply(doc = data_root, path = "//nct_id", fun = xmlValue)
  #   location_data$Phase <- xpathSApply(doc = data_root, path = "//phase", fun = xmlValue)
  # }

  results <- cbind(house_keeping, location_data)
  results
}


# ## Testing
# test <- list.files(path = "Benchmark_data/Dementia/", recursive = TRUE, full.names = TRUE)
# trial_path = test[17]
# get_locations(trial_path = trial_path)

