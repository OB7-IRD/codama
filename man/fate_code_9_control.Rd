% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fate_code_9_control.R
\name{fate_code_9_control}
\alias{fate_code_9_control}
\title{Fate code 9 control}
\usage{
fate_code_9_control(
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
\item{data_connection}{{\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fate_code_9_control}

\item{start_year}{{\link[base]{integer}} expected. Starting year for the control.}

\item{end_year}{{\link[base]{integer}} expected. Ending year for the control.}

\item{program}{{\link[base]{character}} expected. Programs to be controlled.}

\item{ocean}{{\link[base]{character}} expected. Ocean to be controlled.}

\item{country_code}{{\link[base]{character}} expected. Countries for which control will be made.}

\item{path_file}{{\link[base]{character}} expected. Path to save the final xlsx with all the informations for the correction.}
}
\value{
The function return a xlsx table.
}
\description{
Identifies in the Observer data all the fate code 9 and the comment associated
}
