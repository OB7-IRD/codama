#' @name obs_catch_or_sample_outliers_control
#' @title Catch or sample outliers control
#' @author Chloé Tellier, Esther Mollier, Flore Moussy, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies for each species in the observer data the individuals with outliers lengths.
#' @param data {\link[base]{data.frame}} expected. All catches or samples during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframes must contain all these columns for the function to work :
#' \itemize{
#' Dataframe catch:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_meanlength}}
#'  \item{\code{  catch_meanlengthcomputedsource}}
#'  \item{\code{  species_minlength}}
#'  \item{\code{  species_maxlength}}
#' }
#' Or
#' \itemize{
#' Dataframe sample:
#'  \item{\code{  samplemeasure_id}}
#'  \item{\code{  sample_length}}
#'  \item{\code{  sample_islengthcomputed}}
#'  \item{\code{  species_minlength}}
#'  \item{\code{  species_maxlength}}
#' }
#' @doctest
#' #Catch 1 is ok: the catch is 30 cm, its computed source is NA, its mean length is 10 and its max length is 60,
#' #Catch 2 is not ok: the catch is 12 cm, its computed source is NA, its mean length is 10 and its max length is 60,
#' #Catch 3 is not ok: the catch is 59 cm, its computed source is NA, its mean length is 10 and its max length is 60,
#' #Catch 4 is not ok: the catch is 30 cm, its computed source is NA, its mean length is NA and its max length is NA.
#' catch <- data.frame(catch_id = c("1", "2", "3", "4"),
#'                     catch_meanlength = c(30, 12, 59, 30),
#'                     catch_meanlengthcomputedsource = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_),
#'                     species_minlength = c(10, 10, 10, NA),
#'                     species_maxlength = c(60, 60, 60, NA))
#' @expect equal(., structure(list(catch_id = c("1", "2", "3", "4"), length = c(30, 12, 59, 30), species_minlength = c(10, 10, 10, NA), species_maxlength = c(60, 60, 60, NA), logical = c(TRUE, FALSE, FALSE, FALSE)), class = "data.frame", row.names = c(NA, -4L)))
#' obs_catch_or_sample_outliers_control(catch)
#' @export
obs_catch_or_sample_outliers_control <- function(data) {
  # 0 - Global variables assignment ----
  species_minlength <- NULL
  species_maxlength <- NULL
  species_minlength <- NULL
  species_maxlength <- NULL
  suspicioussizemin <- NULL
  suspicioussizemax <- NULL
  lengthcomputed <- NULL
  # 1 - Arguments verification ----
  if (!(codama::r_table_checking(r_table = data,
                                 type = "data.frame",
                                 column_name = c("catch_id", "catch_meanlength", "catch_meanlengthcomputedsource", "species_minlength", "species_maxlength"),
                                 column_type = c("character", "numeric", "integer", "numeric", "numeric"),
                                 output = "logical") ||
        codama::r_table_checking(r_table = data,
                                 type = "data.frame",
                                 column_name = c("samplemeasure_id", "sample_length", "sample_islengthcomputed", "species_minlength", "species_maxlength"),
                                 column_type = c("character", "numeric", "logical", "numeric", "numeric"),
                                 output = "logical"))) {
    # Pas possible d'utiliser le message automatique de Codama, j'en créé un perso
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - The data is incorrect; it must be in one of the following two formats : ",
      "\n  'catch_id' (character), 'catch_meanlength' (numeric), 'catch_meanlengthcomputedsource' (integer), 'species_minlength' (numeric), 'species_maxlength' (numeric)",
      "\n  or",
      "\n  'samplemeasure_id' (character), 'sample_length' (numeric), 'sample_islengthcomputed' (logical), 'species_minlength' (numeric), 'species_maxlength' (numeric)",
      ,
      sep = ""
    )
  } else {
    if ("catch_id" %in% colnames(data)) {
      data <- data[, c("catch_id", "catch_meanlength", "catch_meanlengthcomputedsource", "species_minlength", "species_maxlength")]
    }
    if ("samplemeasure_id" %in% colnames(data)) {
      data <- data[, c("samplemeasure_id", "sample_length", "sample_islengthcomputed", "species_minlength", "species_maxlength")]
    }
  }
  # 2 - Data manipulation ----
  data <- data %>%
    dplyr::rename(length = dplyr::any_of(c("catch_meanlength", "sample_length"))) %>%
    dplyr::rename(lengthcomputed = dplyr::any_of(c("catch_meanlengthcomputedsource", "sample_islengthcomputed")))
  # Delete lengths that are not true length measure (not observed)
  data_size <- data %>%
    dplyr::filter(!is.na(length)) %>%
    dplyr::filter(lengthcomputed == "FALSE" | is.na(lengthcomputed))
  # Catches or samples in the 5% smaller or taller in the size range of this species
  threshold <- 0.05
  data_size <- data_size %>%
    dplyr::mutate(suspicioussizemin = species_minlength + threshold * (species_maxlength - species_minlength)) %>%
    dplyr::mutate(suspicioussizemax = species_maxlength - threshold * (species_maxlength - species_minlength)) %>%
    dplyr::mutate(logical = !(length <= suspicioussizemin | length >= suspicioussizemax | is.na(species_minlength) | is.na(species_maxlength))) %>%
    dplyr::select(-c(lengthcomputed, suspicioussizemin, suspicioussizemax))
  # 3 - Export ----
  return(data_size)
}
