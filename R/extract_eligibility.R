#' Extract Eligibility
#'
#' Function extraccts the eligibility criteria from the trials.
#' @param trial_path An absolute or relative path to the XML file
#'
#' @return Returns a tidy dataframe with the basic trial metadata (a trial's NCT
#'   number, trial status, phase and title) with the inclusion and exclusion
#'   criteria.
#'
#' @export

extract_eligibility <- function(trial_path){

  # Check to see if the input is a XML file
  if(!grepl(pattern = "NCT.*xml$", x = trial_path)){
    stop("Path is not an XML file. File should start with 'NCT' and end with '.XML'")
  }

  data <- XML::xmlTreeParse(file = trial_path, useInternalNodes = TRUE)
  data_root <- XML::xmlRoot(data)

  # Collecting housekeeping data
  Housekeeping <- TidyTrials::Housekeeping
  house_keeping <- Housekeeping(trial_path)

  # Check to make sure the text block is present in the XML file.
  tryCatch(expr = {
    eligibility_textblock <- XML::xpathSApply(doc = data_root, path = "//criteria/textblock", fun = xmlValue)
  },
  error = function(e){ next() }
  )


  # Splitting the text block --
  eligibility_textblock <- XML::xpathSApply(doc = data_root, path = "//criteria/textblock", fun = xmlValue)
  eligibility_textblock <- gsub(pattern = "-  ", replacement = "", x = eligibility_textblock)
  eligibility_textblock <- strsplit(x = eligibility_textblock, split = "Inclusion Criteria:|Exclusion Criteria:")
  eligibility_textblock <- eligibility_textblock[[1]][2:3]

  # Working with the inclusion criteria --
  eligibility_inc <- strsplit(x = gsub(pattern = "\n\n", replacement = " new_line ", x = eligibility_textblock[1]), split = "new_line")[[1]]
  eligibility_inc <- base::trimws(gsub(pattern = "\\s+", replacement = " ", x = eligibility_inc))
  tryCatch(expr = {  eligibility_df_inc <- data.frame(x = eligibility_inc[eligibility_inc != ""], "Type" = "Inclusion")},
           error = function(e){
             eligibility_df_inc <<- data.frame("x" = "None Given", "Type" = "Inclusion")
             eligibility_df_inc
           } #  Error handling code
  ) # tryCatch function

  # Working with the exclusion criteria --
  eligibility_exc <- strsplit(x = gsub(pattern = "\n\n", replacement = " new_line ", x = eligibility_textblock[2]), split = "new_line")[[1]]
  eligibility_exc <- base::trimws(gsub(pattern = "\\s+", replacement = " ", x = eligibility_exc))
  tryCatch(expr = {  eligibility_df_exc <- data.frame(x = eligibility_exc[eligibility_exc != ""], "Type" = "Exclusion")},
           error = function(e) {
             eligibility_df_exc <<- data.frame("x" = "None Given", "Type" = "Exclusion")
             eligibility_df_exc
             } #  Error handling code
           ) # tryCatch function

  # Putting the inclusion and exclsion criteria together --
  eligibility_df_all <- dplyr::bind_rows(eligibility_df_inc, eligibility_df_exc)



  # Working with the eligibility criteria (gender, age and accepts healthy volunteers) --
  eligibility_criteria <- data.frame("x" =
    c(
    gender <- paste("Gender:", XML::xpathSApply(doc = data_root, path = "//eligibility/gender", fun = xmlValue)),
    minimum_age <- paste("Minimum_Age:", XML::xpathSApply(doc = data_root, path = "//eligibility/minimum_age", fun = xmlValue)),
    maximum_age <- paste("Maximum_Age:", XML::xpathSApply(doc = data_root, path = "//eligibility/maximum_age", fun = xmlValue)),
    healthy_volunteers <- paste("Healthy_Volunteers:", XML::xpathSApply(doc = data_root, path = "//eligibility/healthy_volunteers", fun = xmlValue))
    ), "Type" = "Eligibility"
  )

  eligibility_df_all <- dplyr::bind_rows(eligibility_df_all, eligibility_criteria)
  eligibility_df_all <- cbind(house_keeping, eligibility_df_all)

  eligibility_df_all
}
