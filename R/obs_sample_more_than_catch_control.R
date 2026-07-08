#' @name obs_sample_more_than_catch_control
#' @title Sample more than catch control
#' @author Chloé Tellier, Esther Mollier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the observer data all the observations with a number of samples superior to the number of catches.
#' @param catch {\link[base]{data.frame}} expected. All catches during the time range selected.
#' @param sample {\link[base]{data.frame}} expected. All samples during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframes must contain all these columns for the function to work :
#' \itemize{
#' Dataframe catch:
#'  \item{\code{  catch_id}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  species_faocode}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  catch_count}}
#' }
#' \itemize{
#' Dataframe sample:
#'  \item{\code{  samplemeasure_id}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  species_faocode}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  sample_count}}
#' }
#' @doctest
#' #Activity 1 is ok: there is 2 catches and 1 samples,
#' #Activity 2 is ok: there is 2 catches and 2 samples,
#' #Activity 3 is not ok: there is 1 catch but 2 samples.
#' catch <- data.frame(catch_id = c("1", "2", "3", "4"),
#'                     activity_id = c("1", "2", "2", "3"),
#'                     species_faocode = c("BLT", "FAL", "FAL", "CNT"),
#'                     speciesfate_code = c("15", "4", "4", "5"),
#'                     catch_count = c("2", "1", "1", "1"))
#' sample <- data.frame(samplemeasure_id = c("1", "2", "3", "4"),
#'                      activity_id = c("1", "2", "2", "3"),
#'                      species_faocode = c("BLT", "FAL", "FAL", "CNT"),
#'                      speciesfate_code = c("15", "4", "4", "5"),
#'                      sample_count = c("1", "1", "1", "2"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3"), species_faocode = c("BLT", "FAL", "CNT"), speciesfate_code = c("15", "4", "5"), nb_catch = c(2, 2, 1), nb_measure = c(1, 2, 2), logical = c(TRUE, TRUE, FALSE)), row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame")))
#' obs_sample_more_than_catch_control(catch, sample)
#' @export
obs_sample_more_than_catch_control <- function(catch, sample) {
  # 0 - Global variables assignment ----
  activity_id <- NULL
  species_faocode <- NULL
  speciesfate_code <- NULL
  catch_count <- NULL
  sample_count <- NULL
  nb_catch <- NULL
  nb_measure <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = catch,
    type = "data.frame",
    column_name = c("catch_id", "activity_id", "species_faocode", "speciesfate_code", "catch_count"),
    column_type = c("character", "character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = catch,
      type = "data.frame",
      column_name = c("catch_id", "activity_id", "species_faocode", "speciesfate_code", "catch_count"),
      column_type = c("character", "character", "character", "character", "character"),
      output = "error"
    )
  } else {
    catch <- catch[, c("catch_id", "activity_id", "species_faocode", "speciesfate_code", "catch_count")]
  }
  if (!codama::r_table_checking(
    r_table = sample,
    type = "data.frame",
    column_name = c("samplemeasure_id", "activity_id", "species_faocode", "speciesfate_code", "sample_count"),
    column_type = c("character", "character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = sample,
      type = "data.frame",
      column_name = c("samplemeasure_id", "activity_id", "species_faocode", "speciesfate_code", "sample_count"),
      column_type = c("character", "character", "character", "character", "character"),
      output = "error"
    )
  } else {
    sample <- sample[, c("samplemeasure_id", "activity_id", "species_faocode", "speciesfate_code", "sample_count")]
  }
  # 2 - Data manipulation ----
  # Summarise catch and sample by activity_id, fao code and fate code
  summarise_catch <- catch %>%
    dplyr::group_by(activity_id,
                    species_faocode,
                    speciesfate_code) %>%
    dplyr::summarise(nb_catch = sum(as.numeric(catch_count)),
                     .groups = "drop")
  summarise_sample <- sample %>%
    dplyr::group_by(activity_id,
                    species_faocode,
                    speciesfate_code) %>%
    dplyr::summarise(nb_measure = sum(as.numeric(sample_count)),
                     .groups = "drop")
  # Merge of the two tables
  summarise_sample_catch <- summarise_catch %>%
    dplyr::full_join(summarise_sample,
                     by = c("activity_id",
                            "species_faocode",
                            "speciesfate_code"))
  # Add zero if catch is NA after merging (meaning there is no catch observation for this sampled species and fate)
  # Add zero if sample is NA after merging (meaning there is no samples for this caught species and fate)
  summarise_sample_catch <- summarise_sample_catch %>%
    dplyr::mutate(nb_catch = ifelse(is.na(nb_catch), 0, nb_catch)) %>%
    dplyr::mutate(nb_measure = ifelse(is.na(nb_measure), 0, nb_measure))
  # Filter if sample > catch
  sample_more_than_catch <- summarise_sample_catch %>%
    dplyr::mutate(logical = nb_catch >= nb_measure)
  # 4 - Export ----
  return(sample_more_than_catch)
}
