#' @name function_reason_for_discard_unknown_control
#' @title Reason for discard unknown control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the observer data cases where the reason for discard is 99 or NA.
#' @param catch {\link[base]{data.frame}} expected. All catches during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe catch:
#'  \item{\code{  catch_id}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  reasonfordiscard_code}}
#' }
#' @doctest
#' #Catch 1 is ok: the catch is not discarded,
#' #Catch 2 is ok: the catch is discarded and has a reason for discard,
#' #Catch 3 is not ok: the catch is discarded and has a reason for discard = NA,
#' #Catch 3 is not ok: the catch is discarded and does not have a reason for discard.
#' catch <- data.frame(catch_id = c("1", "2", "3", "4"),
#'                     speciesfate_code = c("6", "4", "5", "14"),
#'                     reasonfordiscard_code = c(NA, "1", NA, "99"))
#' @expect equal(., structure(list(catch_id = c("1", "2", "3", "4"), speciesfate_code = c("6", "4", "5", "14"), reasonfordiscard_code = c(NA, "1", NA, "99"), logical = c(TRUE, TRUE, FALSE, FALSE)), class = "data.frame", row.names = c(NA, -4L)))
#' function_reason_for_discard_unknown_control(catch)
#' @export
function_reason_for_discard_unknown_control <- function(catch) {
  # 0 - Global variables assignment ----
  speciesfate_code <- NULL
  reasonfordiscard_code <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = catch,
    type = "data.frame",
    column_name = c("catch_id", "speciesfate_code", "reasonfordiscard_code"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = catch,
      type = "data.frame",
      column_name = c("catch_id", "speciesfate_code", "reasonfordiscard_code"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    catch <- catch[, c("catch_id", "speciesfate_code", "reasonfordiscard_code")]
  }
  # 2 - Data manipulation ----
  catch_reason_discard_unknown <- catch %>%
    dplyr::mutate(logical = ifelse(
      speciesfate_code %in% c("1", "2", "3", "4", "5", "10", "11", "13", "14") & (is.na(reasonfordiscard_code) | reasonfordiscard_code == "99"),
      FALSE, TRUE
    ))
  # 3 - Return ----
  return(catch_reason_discard_unknown)
}
