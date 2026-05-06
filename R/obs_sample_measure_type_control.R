#' @name obs_sample_measure_type_control
#' @title Measure type control
#' @author Chloé Tellier, Esther Mollier, Philippe S. Sabarros
#' @note Version 1.0
#' @description This function allows to check the coherence of a sampled species measure type according to its default measure type. Major tunas (YFT, BET, SKJ and ALB) could also be measured in PD1.
#' @param sample {\link[base]{data.frame}} expected. All samples during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe sample:
#'  \item{\code{  samplemeasure_id}}
#'  \item{\code{  species_faocode}}
#'  \item{\code{  sizetype_code}}
#'  \item{\code{  defaultsizetype_code}}
#' }
#' @doctest
#' #Sample 1 is ok: the sample is a CNT measured in TL
#' #Sample 2 is ok: the sample is a FAL measured in TL
#' #Sample 3 is ok: the sample is a LKV measured in SCL
#' #Sample 4 is ok: the sample is a YFT measured in PD1
#' #Sample 5 is not ok: the sample is a FAL measured in FL
#' sample <- data.frame(samplemeasure_id = c("1", "2", "3", "4", "5"),
#'                      species_faocode = c("CNT", "FAL", "LKV", "YFT", "FAL"),
#'                      sizetype_code = c("TL", "TL", "SCL", "PD1", "FL"),
#'                      defaultsizetype_code = c("TL", "TL", "SCL", "FL", "TL"))
#' @expect equal(., structure(list(samplemeasure_id = c("1", "2", "3", "4", "5"), species_faocode = c("CNT", "FAL", "LKV", "YFT", "FAL"), sizetype_code = c("TL", "TL", "SCL", "PD1", "FL"), defaultsizetype_code = c("TL", "TL", "SCL", "FL", "TL"), logical = c(TRUE, TRUE, TRUE, TRUE, FALSE)), class = "data.frame", row.names = c(NA, -5L)))
#' obs_sample_measure_type_control(sample)
#' @export
obs_sample_measure_type_control <- function(sample) {
  # 0 - Global variables assignment ----
  species_faocode <- NULL
  sizetype_code <- NULL
  defaultsizetype_code <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = sample,
    type = "data.frame",
    column_name = c("samplemeasure_id", "species_faocode", "sizetype_code", "defaultsizetype_code"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = sample,
      type = "data.frame",
      column_name = c("samplemeasure_id", "species_faocode", "sizetype_code", "defaultsizetype_code"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    sample <- sample[, c("samplemeasure_id", "species_faocode", "sizetype_code", "defaultsizetype_code")]
  }
  # 2 - Data manipulation ----
  sample_measure_type <- sample %>%
    dplyr::mutate(logical =
                    (sizetype_code == defaultsizetype_code |
                       sizetype_code == "PD1" & species_faocode %in% c("YFT", "BET", "SKJ", "ALB")))
  # 3 - Return ----
  return(sample_measure_type)
}
