#' @name function_catch_fate_unknown_control
#' @title Catch fate unknown control
#' @author Chloé Tellier, Esther Mollier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the observer data catches where fate code 9 (other) or 11 (discarded unknown) were used.
#' @param catch {\link[base]{data.frame}} expected. All catches during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframes must contain all these columns for the function to work :
#' \itemize{
#' Dataframe catch:
#'  \item{\code{  catch_id}}
#'  \item{\code{  speciesfate_code}}
#' }
#' @doctest
#' #Catch 1 is ok: the catch is discarded alive,
#' #Catch 2 is ok: the catch is retained on board,
#' #Catch 3 is not ok: the catch has an unknown fate,
#' #Catch 3 is not ok: the catch is discarded but with an unknown status.
#' catch <- data.frame(catch_id = c("1", "2", "3", "4"),
#'                     speciesfate_code = c("4", "15", "9", "11"))
#' @expect equal(., structure(list(catch_id = c("1", "2", "3", "4"), speciesfate_code = c("4", "15", "9", "11"), logical = c(TRUE, TRUE, FALSE, FALSE)), class = "data.frame", row.names = c(NA, -4L)))
#' function_catch_fate_unknown_control(catch)
#' @export
function_catch_fate_unknown_control <- function(catch) {
  # 0 - Global variables assignment ----
  speciesfate_code <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = catch,
    type = "data.frame",
    column_name = c("catch_id", "speciesfate_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = catch,
      type = "data.frame",
      column_name = c("catch_id", "speciesfate_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    catch <- catch[, c("catch_id", "speciesfate_code")]
  }
  # 2 - Data manipulation ----
  catch_fate_unknown <- catch %>%
    dplyr::mutate(logical = ifelse(speciesfate_code %in% c("9", "11"), FALSE, TRUE))
  # 3 - Return ----
  return(catch_fate_unknown)
}
