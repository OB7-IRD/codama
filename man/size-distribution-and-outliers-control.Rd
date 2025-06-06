% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/size_distribution_and_outliers_control.R
\name{size distribution and outliers control}
\alias{size distribution and outliers control}
\alias{size_distribution_and_outliers_control}
\title{Size distribution and outliers control}
\usage{
size_distribution_and_outliers_control(
  data_connection,
  start_year,
  end_year,
  start_year_stat,
  end_year_stat,
  start_year_catch_vs_sample,
  end_year_catch_vs_sample,
  program,
  ocean,
  country_code,
  type,
  path_file = NULL
)
}
\arguments{
\item{data_connection}{{\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the size_distribution_and_outliers_control.}

\item{start_year}{{\link[base]{integer}} expected. Starting year for the samples to control.}

\item{end_year}{{\link[base]{integer}} expected. Ending year for the samples to control.}

\item{start_year_stat}{{\link[base]{integer}} expected. Starting year to calculate the mean median and quantiles.}

\item{end_year_stat}{{\link[base]{integer}} expected. Ending year to calculate the mean median and quantiles.}

\item{start_year_catch_vs_sample}{{\link[base]{integer}} expected. Starting year to compare sample to catch size distribution.}

\item{end_year_catch_vs_sample}{{\link[base]{integer}} expected. Ending year to compare sample to catch size distribution.}

\item{program}{{\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234".}

\item{ocean}{{\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.}

\item{country_code}{{\link[base]{character}} expected. Countries on wich control will be made. Examples: 'FRA', 'MUS'...etc.}

\item{type}{{\link[base]{character}} expected. type of graph you want: count or density.}

\item{path_file}{{\link[base]{character}} expected. Path to save the final xlsx.}
}
\value{
The function returns histograms and xlsx tables.
}
\description{
'This script aims to visualise length distribtuion by species, year and ocean. It also gives for each species outliers lengths'.
}
\note{
Version 1.0
}
\author{
Esther Mollier, Flore Moussy, Philippe S. Sabarros
}
