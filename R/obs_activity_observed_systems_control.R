#' @name obs_activity_observed_systems_control
#' @title Activity observed systems control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the observer data any inconsistencies between observed systems and species caught.
#' @param catch {\link[base]{data.frame}} expected. All catches during the time range selected.
#' @param observedsystem {\link[base]{data.frame}} expected. All observed systems for activities during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframes must contain all these columns for the function to work :
#' \itemize{
#' Dataframe catch:
#'  \item{\code{  activity_id}}
#'  \item{\code{  catch_id}}
#'  \item{\code{  species_faocode}}
#' }
#' \itemize{
#' Dataframe observedsystem:
#'  \item{\code{  activity_id}}
#'  \item{\code{  observedsystem_id}}
#'  \item{\code{  observedsystem_code}}
#' }
#' @doctest
#' #Catch 1 is ok: there is a whale shark in the catches, and a whale shark in the observed systems.
#' #Catch 2 is ok: there is a whale in the catches, and a whale in the observed systems.
#' #Catch 3 is ok: there is no whale or whale shark in the catches, and no whale or whale shark in the observed systems.
#' #Catch 4 is not ok: there is a whale in the catches, but not in the observed systems.
#' #Catch 5 is not ok: there is a whale shark in the observed systems, but not in the catches.
#' catch <- data.frame(activity_id = c("1", "2", "3", "4", "5"),
#'                     catch_id = c("1", "2", "3", "4", "5"),
#'                     species_faocode = c("RHN, SKJ, BLT", "MYS, YFT, BET", "SKJ, YFT", "SKJ, MYS, BUM", "YFT"))
#' observedsystem <- data.frame(activity_id = c("1", "2", "3", "4", "5"),
#'                              observedsystem_id = c("1", "2", "3", "4", "5"),
#'                              observedsystem_code = c("22", "112", "0", "0", "21"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3", "4", "5"), species_faocode = c("RHN, SKJ, BLT", "MYS, YFT, BET", "SKJ, YFT", "SKJ, MYS, BUM", "YFT"), observedsystem_code = c("22", "112", "0", "0", "21"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE)), row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame")))
#' obs_activity_observed_systems_control(catch, observedsystem)
#' @export
obs_activity_observed_systems_control <- function(catch,
                                                  observedsystem) {
  # 0 - Global variables assignment ----
  activity_id <- NULL
  observedsystem_code <- NULL
  species_faocode <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = catch,
    type = "data.frame",
    column_name = c("activity_id", "catch_id", "species_faocode"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = catch,
      type = "data.frame",
      column_name = c("activity_id", "catch_id", "species_faocode"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    catch <- catch[, c("activity_id", "catch_id", "species_faocode")]
  }
  if (!codama::r_table_checking(
    r_table = observedsystem,
    type = "data.frame",
    column_name = c("activity_id", "observedsystem_id", "observedsystem_code"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = observedsystem,
      type = "data.frame",
      column_name = c("activity_id", "observedsystem_id", "observedsystem_code"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    observedsystem <- observedsystem[, c("activity_id", "observedsystem_id", "observedsystem_code")]
  }
  # 2 - Data manipulation ----
  observedsystem_summary <- observedsystem %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(observedsystem_code = paste(observedsystem_code, collapse = ", "),
                     .groups = "drop")
  catch_summary <- catch %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(species_faocode = paste(species_faocode, collapse = ", "),
                     .groups = "drop")
  catch_result <- catch_summary %>%
    dplyr::left_join(observedsystem_summary,
                     by = "activity_id")
  # observed_system_code : 9 / 10 / 11 / 111 / 112 pour MYS
  # observed_system_code : 12 / 21 / 22 pour RHN
  # fao_code : MYS / RHN
  catch_result <- catch_result %>%
    dplyr::mutate(
      logical = dplyr::case_when(
        # Si RHN dans systèmes observés et pas RHN dans capture
        grepl("(^|, )(12|21|22)(, |$)", observedsystem_code) & !grepl("RHN", species_faocode) ~ FALSE,
        # Si RHN en capture et pas RHN dans systèmes observés
        grepl("RHN", species_faocode) & !grepl("(^|, )(12|21|22)(, |$)", observedsystem_code) ~ FALSE,
        # Si baleine en capture et pas dans systèmes observés
        grepl("MYS|BLW|FIW|HUW|MIW|SIW", species_faocode) & !grepl("(^|, )(9|10|11|111|112)(, |$)", observedsystem_code) ~ FALSE,
        TRUE ~ TRUE
      )
    )
  # 3 - Return ----
  return(catch_result)
}
