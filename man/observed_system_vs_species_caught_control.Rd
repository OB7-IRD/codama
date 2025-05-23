% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/observed_system_vs_species_caught_control.R
\name{observed_system_vs_species_caught_control}
\alias{observed_system_vs_species_caught_control}
\title{Observed system vs species caught control}
\usage{
observed_system_vs_species_caught_control(
  data_connection,
  start_year,
  end_year,
  program,
  ocean,
  country_code,
  path_file = NULL
)
}
\arguments{
\item{data_connection}{{\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fate_control.}

\item{start_year}{{\link[base]{integer}} expected. Starting year for the control.}

\item{end_year}{{\link[base]{integer}} expected. Ending year for the control.}

\item{program}{{\link[base]{character}} expected. Programs to be controlled.}

\item{ocean}{{\link[base]{character}} expected. Ocean to be controlled.}

\item{country_code}{{\link[base]{character}} expected. Countries for which control will be made.}

\item{path_file}{{\link[base]{character}} expected. Path to save the final xlsx with all the informations for the correction.}
}
\value{
The function returns one xlsx table.
}
\description{
Identifies in the Observer data any inconsistencies between observed systems and species caught.
}
\note{
Version 1.0
}
\author{
Chloé Tellier, Philippe S. Sabarros
}
