% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/round_size_correction.R
\name{round_size_correction}
\alias{round_size_correction}
\title{Round size correction}
\usage{
round_size_correction(
  data_connection,
  start_year,
  end_year,
  program,
  ocean,
  country_code,
  corrector,
  action,
  path_file = NULL
)
}
\arguments{
\item{data_connection}{{\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the size_control.}

\item{start_year}{{\link[base]{integer}} expected. Starting year for the control.}

\item{end_year}{{\link[base]{integer}} expected. Ending year for the control.}

\item{program}{{\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234".}

\item{ocean}{{\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.}

\item{country_code}{{\link[base]{character}} expected. Countries on which control will be made. Examples: 'FRA', 'MUS'...etc.}

\item{corrector}{{\link[base]{character}} expected. First letter of the corrector's first name and last name. Examples: 'JMartin' for Jeanne Martin.}

\item{action}{{\link[base]{character}} expected. Type of action required when update queries are launched. COMMIT is used to definitively validate modifications and make them permanent in the database. ROLLBACK is used to undo changes made.}

\item{path_file}{{\link[base]{character}} expected. By default NULL. Path to save the final xlsx.}
}
\value{
The function corrects size samples directly in the database using update queries and returns a xlsx file with the corrected samples.
}
\description{
This function corrects unrounded size samples to the inferior centimeter (or half centimeter for PD1).
}
\note{
Version 1.0
}
\author{
Chloé Tellier, Philippe S. Sabarros
}
