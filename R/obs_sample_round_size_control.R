#' @name obs_sample_round_size_control
#' @title Sample round size control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description This function controls the accuracy (decimals) of size samples. Returns samples with decimal values that are not rounded to the inferior centimeter (or to the nearest half centimeter for PD1 measurements).
#' @param sample {\link[base]{data.frame}} expected. All samples during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe sample:
#'  \item{\code{  samplemeasure_id}}
#'  \item{\code{  sample_length}}
#'  \item{\code{  sizetype_code}}
#' }
#' @doctest
#' #Sample 1 is ok: the size type is TL and the length is rounded to .0
#' #Sample 2 is ok: the size type is PD1 and the length is rounded to.5
#' #Sample 3 is not ok: the size type is TL and the length is not rounded to .0
#' #Sample 4 is not ok: the size type is PD1 and the length is not rounded to .5
#' sample <- data.frame(samplemeasure_id = c("1", "2", "3", "4"),
#'                      sample_length = c("112", "35.5", "119.3", "43.7"),
#'                      sizetype_code = c("TL", "PD1", "TL", "PD1"))
#' @expect equal(., structure(list(samplemeasure_id = c("1", "2", "3", "4"), sample_length = c("112", "35.5", "119.3", "43.7"), sizetype_code = c("TL", "PD1", "TL", "PD1"), logical = c(TRUE, TRUE, FALSE, FALSE)), class = "data.frame", row.names = c(NA, -4L)))
#' obs_sample_round_size_control(sample)
#' @export
obs_sample_round_size_control <- function(sample) {
  # 0 - Global variables assignment ----
  sizetype_code <- NULL
  sample_length <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = sample,
    type = "data.frame",
    column_name = c("samplemeasure_id", "sample_length", "sizetype_code"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = sample,
      type = "data.frame",
      column_name = c("samplemeasure_id", "sample_length", "sizetype_code"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    sample <- sample[, c("samplemeasure_id", "sample_length", "sizetype_code")]
  }
  # 2 - Data manipulation ----
  # sizetype_code :
  # CCL CCW CDML CFL CLFL CLJFL CTL DL DML DW EFL FL IDL LJFL PAL PD1 PFL SCL SCW SL TL UNK
  # Wrong samples:
  # sizetype_code not PD1 and length not rounded to .0
  # sizetype_code PD1 and length not rounded to .5
  sample_length_error <- sample %>%
    dplyr::mutate(logical = !(
      (sizetype_code != "PD1" & as.numeric(sample_length) != floor(as.numeric(sample_length))) | (sizetype_code == "PD1" & as.numeric(sample_length) != floor(as.numeric(sample_length) * 2) / 2)
    ))
  # 3 - Return ----
  return(sample_length_error)
}
