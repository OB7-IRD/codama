#' @name obs_sample_sex_control
#' @title Sex control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description This function aims to check the coherence of the sex of a sample according to the possibility of sexing this species.
#' @param sample {\link[base]{data.frame}} expected. All samples during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe sample:
#'  \item{\code{  samplemeasure_id}}
#'  \item{\code{  sex_label}}
#'  \item{\code{  species_faocode}}
#'  \item{\code{  speciesgroup_label}}
#' }
#' @doctest
#' #Sample 1 is ok: the sample is not sexed
#' #Sample 2 is ok: the sample is sexed and is a DOL
#' #Sample 3 is ok: the sample is sexed and is a shark
#' #Sample 4 is not ok: the sample is sexed and is a CNT
#' sample <- data.frame(samplemeasure_id = c("1", "2", "3", "4"),
#'                      sex_label = c(NA, "Female", "Female", "Male"),
#'                      species_faocode = c("CNT", "DOL", "FAL", "CNT"),
#'                      speciesgroup_label = c("Other bony fishes",
#'                                             "Other bony fishes",
#'                                             "Sharks",
#'                                             "Other bony fishes"))
#' @expect equal(., structure(list(samplemeasure_id = c("1", "2", "3", "4"), sex_label = c(NA, "Female", "Female", "Male"), species_faocode = c("CNT", "DOL", "FAL", "CNT"), speciesgroup_label = c("Other bony fishes", "Other bony fishes", "Sharks", "Other bony fishes"), logical = c(TRUE, TRUE, TRUE, FALSE)), class = "data.frame", row.names = c(NA, -4L)))
#' obs_sample_sex_control(sample)
#' @export
obs_sample_sex_control <- function(sample) {
  # 0 - Global variables assignment ----
  sex_label <- NULL
  species_faocode <- NULL
  speciesgroup_label <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = sample,
    type = "data.frame",
    column_name = c("samplemeasure_id", "sex_label", "species_faocode", "speciesgroup_label"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = sample,
      type = "data.frame",
      column_name = c("samplemeasure_id", "sex_label", "species_faocode", "speciesgroup_label"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    sample <- sample[, c("samplemeasure_id", "sex_label", "species_faocode", "speciesgroup_label")]
  }
  # 2 - Data manipulation ----
  ## Filter sample to find all the sample with sex and which is not a sexable specie
  sample_sex_error <- sample %>%
    dplyr::mutate(logical =
                    !(sex_label %in% c("Female", "Male")) |
                    (sex_label %in% c("Female", "Male") & species_faocode %in% c("DOL", "CFW", "DOX")) |
                    (sex_label %in% c("Female", "Male") & speciesgroup_label %in% c("Rays", "Sharks", "Turtles", "Cetaceans", "Whale shark")))
  # 3 - Return ----
  return(sample_sex_error)
}
