#' @name obs_sample_no_fate_control
#' @title Sample with no fate control
#' @author Chloé Tellier, Esther Mollier, Philippe S. Sabarros
#' @note Version 1.0
#' @description This function aims to find all the samples in the observer data that don't have a fate.
#' @param sample {\link[base]{data.frame}} expected. All samples during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe sample:
#'  \item{\code{  samplemeasure_id}}
#'  \item{\code{  speciesfate_code}}
#' }
#' @doctest
#' #Sample 1 is ok: the fate code is 6 (retained on board)
#' #Sample 2 is ok: the fate code is 4 (discarded alive)
#' #Sample 3 is not ok: the fate code is NA
#' sample <- data.frame(samplemeasure_id = c("1", "2", "3"),
#'                      speciesfate_code = c("6", "4", NA))
#' @expect equal(., structure(list(samplemeasure_id = c("1", "2", "3"), speciesfate_code = c("6", "4", NA), logical = c(TRUE, TRUE, FALSE)), class = "data.frame", row.names = c(NA, -3L)))
#' obs_sample_no_fate_control(sample)
#' @export
obs_sample_no_fate_control <- function(sample) {
  # 0 - Global variables assignment ----
  speciesfate_code <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = sample,
    type = "data.frame",
    column_name = c("samplemeasure_id", "speciesfate_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = sample,
      type = "data.frame",
      column_name = c("samplemeasure_id", "speciesfate_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    sample <- sample[, c("samplemeasure_id", "speciesfate_code")]
  }
  # 2 - Data manipulation ----
  ## Filter sample to find all the samplemeasure without no fate
  sample_with_no_fate <- sample %>%
    dplyr::mutate(logical = !(is.na(speciesfate_code)))
  # 3 - Return ----
  return(sample_with_no_fate)
}
