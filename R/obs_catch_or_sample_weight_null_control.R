#' @name obs_catch_or_sample_weight_null_control
#' @title Catch or sample weight unknown control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the observer data catches or samples all individual weights = 0 or NA.
#' @param data {\link[base]{data.frame}} expected. All catches or samples during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframes must contain all these columns for the function to work :
#' \itemize{
#' Dataframe catch:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#' }
#' Or
#' \itemize{
#' Dataframe sample:
#'  \item{\code{  samplemeasure_id}}
#'  \item{\code{  sample_weight}}
#' }
#' @doctest
#' #Catch 1 is ok: the catch's weight is > 0,
#' #Catch 2 is not ok: the catch's weight is = 0,
#' #Catch 3 is not ok: the catch's weight is NA.
#' catch <- data.frame(catch_id = c("1", "2", "3"),
#'                     catch_weight = c("5", "0", NA))
#' @expect equal(., structure(list(catch_id = c("1", "2", "3"), weight = c("5", "0", NA), logical = c(TRUE, FALSE, FALSE)), class = "data.frame", row.names = c(NA, -3L)))
#' obs_catch_or_sample_weight_null_control(catch)
#' @export
obs_catch_or_sample_weight_null_control <- function(data) {
  # 0 - Global variables assignment ----
  weight <- NULL
  # 1 - Arguments verification ----
  if (!(codama::r_table_checking(r_table = data,
                                 type = "data.frame",
                                 column_name = c("catch_id", "catch_weight"),
                                 column_type = c("character", "character"),
                                 output = "logical") ||
        codama::r_table_checking(r_table = data,
                                 type = "data.frame",
                                 column_name = c("samplemeasure_id", "sample_weight"),
                                 column_type = c("character", "character"),
                                 output = "logical"))) {
    # Pas possible d'utiliser le message automatique de Codama, j'en créé un perso
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - The data is incorrect; it must be in one of the following two formats : ",
      "\n  'catch_id' (character), 'catch_weight' (character)",
      "\n  or",
      "\n  'samplemeasure_id' (character), 'sample_weight' (character)",
      ,
      sep = ""
    )
  } else {
    if ("catch_id" %in% colnames(data)) {
      data <- data[, c("catch_id", "catch_weight")]
    }
    if ("samplemeasure_id" %in% colnames(data)) {
      data <- data[, c("samplemeasure_id", "sample_weight")]
    }
  }
  # 2 - Data manipulation ----
  data_weight_null <- data %>%
    dplyr::rename(weight = dplyr::any_of(c("catch_weight", "sample_weight"))) %>%
    dplyr::mutate(logical = ifelse(weight == "0" | is.na(weight), FALSE, TRUE))
  # 3 - Return ----
  return(data_weight_null)
}
