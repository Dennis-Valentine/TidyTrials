% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_intervention.R
\name{extract_interventions}
\alias{extract_interventions}
\title{Extract Interventions}
\usage{
extract_interventions(trial_path)
}
\arguments{
\item{trial_path}{An absolute or relative path to the XML file}
}
\value{
Returns a tidy dataframe with the basic trial metadata (a trial's NCT
  number, trial status, phase and title) and the intervention type; the
  intervention name, the group the intervention is given to, a description of
  the intervention and synonyms for the intervention.
}
\description{
Function to extract the interventions used in the trials.
}
