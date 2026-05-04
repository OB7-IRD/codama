#' @name obs_catch_or_sample_fate_by_species_group_control
#' @title Catch or sample fate by species group control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the observer data all the species with an inconsistent fate according to their species group.
#' @param data {\link[base]{data.frame}} expected. All catches or samples during the time range selected.
#' @return The function returns one {\link[base]{data.frame}}.
#' @details
#' The input dataframes must contain all these columns for the function to work :
#' \itemize{
#' Dataframe catch:
#'  \item{\code{  catch_id}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  species_faocode}}
#'  \item{\code{  speciesgroup_label}}
#' }
#' Or
#' \itemize{
#' Dataframe sample:
#'  \item{\code{  samplemeasure_id}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  species_faocode}}
#'  \item{\code{  speciesgroup_label}}
#' }
#' @doctest
#' #Catch 1 is ok: the catch is a whale and its fate is code 1,
#' #Catch 2 is not ok: the catch is a bycatch and its fate is code 2,
#' #Catch 3 is not ok: the catch is a whale and its fate is code 4,
#' #Catch 4 is ok: the catch is a major tuna and its fate is code 6,
#' #Catch 5 is not ok: the catch is a bycatch and its fate is code 6,
#' #Catch 6 is ok: the catch is a minor tuna and its fate is code 15,
#' #Catch 7 is not ok: the catch is a major tuna and is fate is code 15,
#' #Catch 8 is not ok: the catch is a bycatch and its fate is code 10.
#' catch <- data.frame(catch_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                     speciesfate_code = c("1", "2", "4", "6", "6", "15", "15", "10"),
#'                     species_faocode = c("MYS", "ALM", "MYS", "YFT", "CNT", "LTA", "BET", "MOP"),
#'                     speciesgroup_label = c("Cetaceans", "Other bony fishes",
#'                                              "Cetaceans", "Tunas nei", "Other bony fishes",
#'                                             "Tunas nei", "Tunas nei", "Other bony fishes"))
#' @expect equal(., structure(list(catch_id = c("1", "2", "3", "4", "5", "6", "7", "8"), speciesfate_code = c("1", "2", "4", "6", "6", "15", "15", "10"), species_faocode = c("MYS", "ALM", "MYS", "YFT", "CNT", "LTA", "BET", "MOP"), speciesgroup_label = c("Cetaceans", "Other bony fishes", "Cetaceans", "Tunas nei", "Other bony fishes", "Tunas nei", "Tunas nei", "Other bony fishes"), logical = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)), class = "data.frame", row.names = c(NA, -8L)))
#' obs_catch_or_sample_fate_by_species_group_control(catch)
#' @export
obs_catch_or_sample_fate_by_species_group_control <- function(data) {
  # 0 - Global variables assignment ----
  catch_id <- NULL
  samplemeasure_id <- NULL
  speciesfate_code <- NULL
  species_faocode <- NULL
  speciesgroup_label <- NULL
  # 1 - Arguments verification ----
  if (!(codama::r_table_checking(r_table = data,
                                 type = "data.frame",
                                 column_name = c("catch_id", "speciesfate_code", "species_faocode", "speciesgroup_label"),
                                 column_type = c("character", "character", "character", "character"),
                                 output = "logical") ||
        codama::r_table_checking(r_table = data,
                                 type = "data.frame",
                                 column_name = c("samplemeasure_id", "speciesfate_code", "species_faocode", "speciesgroup_label"),
                                 column_type = c("character", "character", "character", "character"),
                                 output = "logical"))) {
    # Pas possible d'utiliser le message automatique de Codama, j'en créé un perso
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - The data is incorrect; it must be in one of the following two formats : ",
      "\n  'catch_id' (character), 'speciesfate_code' (character), 'species_faocode' (character), 'speciesgroup_label' (character)",
      "\n  or",
      "\n  'samplemeasure_id' (character), 'speciesfate_code' (character), 'species_faocode' (character), 'speciesgroup_label' (character)",
      ,
      sep = ""
    )
  } else {
    if ("catch_id" %in% colnames(data)) {
      data <- data[, c("catch_id", "speciesfate_code", "species_faocode", "speciesgroup_label")]
    }
    if ("samplemeasure_id" %in% colnames(data)) {
      data <- data[, c("samplemeasure_id", "speciesfate_code", "species_faocode", "speciesgroup_label")]
    }
  }
  # 2 - Data manipulation ----
  # Minor tuna : species_group = "Tunas nei" & fao_code %in% c("BLT", "FRI", "FRZ", "KAW", "LTA")
  # Major tuna : species_group = "Tunas nei" & fao_code %in% c("ALB", "BET", "SKJ", "YFT", "TUS")
  # Vulnerable species : species_group = "Sharks" or "Turtles" or "Rays"
  # Bycatch : species_group = "Other bony fishes"
  data_fate_by_species_group_pb <- data %>%
    dplyr::filter(
      # Codes 1 ("échappé du filet") / 2 ("sorti vivant du filet") / 3 ("sorti mort du filet") are only for whales and whale sharks
      (speciesfate_code %in% c(1, 2, 3) & !(speciesgroup_label %in% c("Cetaceans", "Whale shark"))) |
        # Whale sharks and whales can only have codes 1, 2 or 3
        (speciesgroup_label %in% c("Cetaceans", "Whale shark") & !(speciesfate_code %in% c(1, 2, 3))) |
        # Code 6 ("conservé à destination de la conserverie") is only for SKJ / YFT / BET / ALB
        (speciesfate_code == 6 & !(species_faocode %in% c("YFT", "BET", "SKJ", "ALB"))) |
        # Code 15 ("marché local") is only for bycatches or minor tunas
        (speciesfate_code == 15 & (!(speciesgroup_label %in% c("Billfishes", "Other bony fishes") | (speciesgroup_label == "Tunas nei" & !(species_faocode %in% c("YFT", "BET", "SKJ", "ALB")))))) |
        # Code 10 ("ailerons seulement") is only for sharks
        (speciesfate_code == 10 & speciesgroup_label != "Sharks")
    )
  if ("catch_id" %in% colnames(data)) {
    data_fate_by_species_group <- data %>%
      dplyr::mutate(logical = !(catch_id %in% data_fate_by_species_group_pb$catch_id))
  } else if ("samplemeasure_id" %in% colnames(data)) {
    data_fate_by_species_group <- data %>%
      dplyr::mutate(logical = !(samplemeasure_id %in% data_fate_by_species_group_pb$samplemeasure_id))
  }
  # 4 - Export ----
  return(data_fate_by_species_group)
}
