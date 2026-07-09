#' @name obs_catch_ocean_control
#' @title Catch ocean control
#' @author Chloé Tellier, Esther Mollier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Checks the coherence of the species caught according to their distribution area.
#' @param catch {\link[base]{data.frame}} expected. All catches during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe catch:
#'  \item{\code{  catch_id}}
#'  \item{\code{  species_faocode}}
#'  \item{\code{  ocean_label}}
#'  \item{\code{  speciesocean_label}}
#' }
#' @doctest
#' #Catch 1 is ok: a blue marlin found in the Indian ocean.
#' #Catch 2 is ok: a black marlin found in the Indian ocean.
#' #Catch 3 is not ok: a black marlin found in the Atlantic ocean.
#' catch <- data.frame(catch_id = c("1", "2", "3"),
#'                     species_faocode = c("BUM", "BLM", "BLM"),
#'                     ocean_label = c("Indian", "Indian", "Atlantic"),
#'                     speciesocean_label = c("Indian", "Indian", NA))
#' @expect equal(., structure(list(catch_id = c("1", "2", "3"), species_faocode = c("BUM", "BLM", "BLM"), ocean_label = c("Indian", "Indian", "Atlantic"), logical = c(TRUE, TRUE, FALSE)), class = "data.frame", row.names = c(NA, -3L)))
#' obs_catch_ocean_control(catch)
#' @export
obs_catch_ocean_control <- function(catch) {
  # 0 - Global variables assignment ----
  speciesocean_label <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = catch,
    type = "data.frame",
    column_name = c("catch_id", "species_faocode", "ocean_label", "speciesocean_label"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = catch,
      type = "data.frame",
      column_name = c("catch_id", "species_faocode", "ocean_label", "speciesocean_label"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    catch <- catch[, c("catch_id", "species_faocode", "ocean_label", "speciesocean_label")]
  }
  # 2 - Data manipulation ----
  catch_species <- catch %>%
    dplyr::mutate(logical = !is.na(speciesocean_label)) %>%
    dplyr::select(-speciesocean_label)
  # 3 - Return ----
  return(catch_species)
}
