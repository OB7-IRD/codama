#' @name logbook_weigh_catch_well_control
#' @title Gives the inconsistencies between the catch weight and weight in well
#' @description The purpose of the logbook_weigh_catch_well_control function is to provide a table of data that contains an inconsistency between the catch weight and weight in well
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weigh_catch_well_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weigh_catch_well_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("YFT", "SKJ", "BET", "ALB"). list of of species (FAO code) listed separately in the well plan.
#' @param species_fate {\link[base]{character}} expected. Default values: c("6", "15"). Vector of inventory of fate retained on board.
#' @param species_grouped_well {\link[base]{character}} expected. Default values: "XXX*". Code for accessory species grouped together within the well plan
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  wellactivityspecies_id}}
#'  \item{\code{  wellactivityspecies_weight}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Activity 1 are ok,
#' #Activity 2 has different weight,
#' #Activity 3 has well missing,
#' #Activity 4 has catch missing,
#' dataframe1 <- data.frame(catch_id = c("1", "2", "3", "4", "5", "6"),
#'                          catch_weight = c(3, 2, 4, 1, 15, 6),
#'                          speciesfate_code = c("6", "6", "6", "11", "6", "6"),
#'                          species_fao_code = c("YFT", "SKJ", "FRI", "JOS", "ALB", "FRI"),
#'                          activity_id = c("1", "1", "1", "1", "2", "3"))
#' dataframe2 <- data.frame(wellactivityspecies_id = c("1", "2", "3", "4", "5"),
#'                          wellactivityspecies_weight = c(3, 2, 4, 13, 4),
#'                          species_fao_code = c("YFT", "SKJ", "XXX*", "ALB", "BET"),
#'                          activity_id = c("1", "1", "1", "2", "4"))
#' @expect equal(., structure(list(activity_id = c("1", "3", "2", "4"), logical = c(TRUE, FALSE, FALSE, FALSE), `ALB catch` = c(NA, NA, 15, NA), `ALB well` = c(NA, NA, 13, NA), `BET catch` = c(NA, NA, NA, NA), `BET well` = c(NA, NA, NA, 4), `SKJ catch` = c(2, NA, NA, NA), `SKJ well` = c(2, NA, NA, NA), `YFT catch` = c(3, NA, NA, NA), `YFT well` = c(3, NA, NA, NA), `XXX* catch` = c(4, 6, NA, NA), `XXX* well` = c(4, NA, NA, NA)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame")))
#' logbook_weigh_catch_well_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_weigh_catch_well_control <- function(dataframe1,
                                             dataframe2,
                                             output,
                                             species = c("YFT", "SKJ", "BET", "ALB"),
                                             species_fate = c("6", "15"),
                                             species_grouped_well = ("XXX*")) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  species_fao_code <- NULL
  wellactivityspecies_weight <- NULL
  catch_weight <- NULL
  speciesfate_code <- NULL
  `:=` <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id"),
    column_type = c("character", "numeric", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id"),
      column_type = c("character", "numeric", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("wellactivityspecies_id", "wellactivityspecies_weight", "species_fao_code", "activity_id"),
    column_type = c("character", "numeric", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("wellactivityspecies_id", "wellactivityspecies_weight", "species_fao_code", "activity_id"),
      column_type = c("character", "numeric", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("wellactivityspecies_id", "wellactivityspecies_weight", "species_fao_code", "activity_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species_fate,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_fate,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species_grouped_well,
    type = "character",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_grouped_well,
      type = "character",
      length = 1L,
      output = "error"
    ))
  }
  if (species_grouped_well %in% species) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      "Warning! The code in species_grouped_well argument (", species_grouped_well, ") must not appear in species argument (", species, ")..\n",
      sep = ""
    )
  }
  select <- unique(dataframe1$activity_id)
  nrow_first <- length(select)
  # 2 - Data design ----
  # Remove weight if fate not species_fate
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(catch_weight = replace(catch_weight, !(speciesfate_code %in% species_fate), 0))
  # Filter species list separately or grouped in well plan
  dataframe1_separately <- dataframe1 %>%
    dplyr::filter(species_fao_code %in% species)
  dataframe1_grouped <- dataframe1 %>%
    dplyr::filter(!(species_fao_code %in% species))
  # Calculation sum weight catch grouped species (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_sum_weight <- dataframe1_grouped %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(!!paste("sum_weight", species_grouped_well, "catch_bis") := ifelse(all(is.na(catch_weight)), 0, sum(catch_weight, na.rm = TRUE)),
                     !!paste("sum_weight", species_grouped_well, "catch") := ifelse(all(is.na(catch_weight)), NA, sum(catch_weight, na.rm = TRUE)), .groups = "drop")
  # Calculation sum weight catch separately species (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  list_sum_weight_catch_separately <- lapply(species, function(c) {
    dataframe1_separately %>%
      dplyr::filter(species_fao_code %in% c) %>%
      dplyr::group_by(activity_id) %>%
      dplyr::summarise(!!paste("sum_weight", c, "catch_bis") := ifelse(all(is.na(catch_weight)), 0, sum(catch_weight, na.rm = TRUE)),
                       !!paste("sum_weight", c, "catch") := ifelse(all(is.na(catch_weight)), NA, sum(catch_weight, na.rm = TRUE)), .groups = "drop")
  })
  # Merge list with sum weight catch separately species and dataframe with sum weight catch grouped species
  for (i in seq_along(list_sum_weight_catch_separately)){
    dataframe_sum_weight <- dplyr::full_join(dataframe_sum_weight, list_sum_weight_catch_separately[[i]], by = dplyr::join_by(activity_id))
  }
  # Filter species for weight well
  dataframe2 <- dataframe2 %>%
    dplyr::filter(species_fao_code %in% c(species, species_grouped_well))
  # Calculation sum weight well (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  list_sum_weight_well <- lapply(c(species, species_grouped_well), function(c) {
    dataframe2 %>%
      dplyr::filter(species_fao_code %in% c) %>%
      dplyr::group_by(activity_id) %>%
      dplyr::summarise(!!paste("sum_weight", c, "well_bis") := ifelse(all(is.na(wellactivityspecies_weight)), 0, sum(wellactivityspecies_weight, na.rm = TRUE)),
                       !!paste("sum_weight", c, "well") := ifelse(all(is.na(wellactivityspecies_weight)), NA, sum(wellactivityspecies_weight, na.rm = TRUE)), .groups = "drop")
  })
  # Merge list with sum weight well and dataframe with sum weight catch
  for (i in seq_along(list_sum_weight_well)){
    dataframe_sum_weight <- dplyr::full_join(dataframe_sum_weight, list_sum_weight_well[[i]], by = dplyr::join_by(activity_id))
  }
  # Replace NA by 0
  for (i in c(species, species_grouped_well)){
    dataframe_sum_weight[is.na(dataframe_sum_weight[, paste("sum_weight", i, "catch_bis")]), paste("sum_weight", i, "catch_bis")] <- 0
    dataframe_sum_weight[is.na(dataframe_sum_weight[, paste("sum_weight", i, "well_bis")]), paste("sum_weight", i, "well_bis")] <- 0
  }
  # weight grouped species comparison
  comparison <- codama::vector_comparison(
    first_vector = dataframe_sum_weight[, paste("sum_weight", species_grouped_well, "catch_bis"), drop = TRUE],
    second_vector = dataframe_sum_weight[, paste("sum_weight", species_grouped_well, "well_bis"), drop = TRUE],
    comparison_type = "equal",
    output = "report"
  )
  dataframe_sum_weight$logical <- comparison$logical
  # weight separately species comparison
  for (i in species){
    comparison <- codama::vector_comparison(
      first_vector = dataframe_sum_weight[, paste("sum_weight", i, "catch_bis"), drop = TRUE],
      second_vector = dataframe_sum_weight[, paste("sum_weight", i, "well_bis"), drop = TRUE],
      comparison_type = "equal",
      output = "report"
    )
    dataframe_sum_weight$logical <- dataframe_sum_weight$logical & comparison$logical
  }
  # Rename, reorder and remove column
  for (i in  c(species_grouped_well, species)){
    dataframe_sum_weight <- dataframe_sum_weight %>%
      dplyr::rename(!!paste(i, "catch") := paste("sum_weight", i, "catch"),
                    !!paste(i, "well") := paste("sum_weight", i, "well")) %>%
      dplyr::relocate(!!paste(i, "catch"), !!paste(i, "well"), .after = logical) %>%
      dplyr::select(!c(!!paste("sum_weight", i, "catch_bis"), !!paste("sum_weight", i, "well_bis")))
  }
  # Modify the table for display purposes: add, remove and order column
  if ((sum(dataframe_sum_weight$logical, na.rm = TRUE) + sum(!dataframe_sum_weight$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe_sum_weight$logical))) {
    all <- c(select, dataframe_sum_weight$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing catch or well ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe_sum_weight$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe_sum_weight$logical)), "):", paste0(dataframe_sum_weight$activity_id[is.na(dataframe_sum_weight$logical)], collapse = ", "))
    }
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      text,
      sep = ""
    )
  }
  # 3 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe_sum_weight$logical), " activity inconsistency weight catch with weight in well", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe_sum_weight)
  }
  if (output == "logical") {
    if (sum(!dataframe_sum_weight$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
