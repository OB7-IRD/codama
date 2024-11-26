#' @name check_raising_factor_inspector
#' @title Gives the inconsistencies between RF1 and threshold values
#' @description The purpose of the check_raising_factor_inspector function is to provide a table of data that contains an inconsistency with the RF1 and the valid threshold (Default : 0.9 ; 1.1)
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_raising_factor_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_raising_factor_inspector () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_raising_factor_inspector () function.
#' @param dataframe4 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_raising_factor_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param country_species {\link[base]{character}} expected. Default values: list("1" = c("TUN", "ALB", "YFT", "BET", "SKJ"), "4" = c("LOT", "TUN", "ALB", "YFT", "BET", "SKJ", "LTA", "FRI", "BLF", "RAV*", "KAW", "FRZ", "BLT")). list of the inventory of species (FAO code) used to calculate catch weight in RF1 by country (country code).
#' @param speciesfate {\link[base]{character}} expected. Default values: "6". Vector of inventory of fate used to calculate catch weight in RF1.
#' @param vesselactivity {\link[base]{character}} expected. Default values: c("23", "25", "27", "29"). Vector of inventory of vessel activity NOT used to calculate catch weight in RF1.
#' @param threshold {\link[base]{numeric}} expected. Default values: 0.9 and 1.1. Vector containing the lower and upper acceptable threshold for RF1.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#' Dataframe 2:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  trip_id}}
#' Dataframe 3:
#'  \item{\code{  landing_id}}
#'  \item{\code{  landing_weight}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  trip_id}}
#' Dataframe 4:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_end_full_trip_id}}
#'  \item{\code{  vessel_id}}
#'  \item{\code{  country_fleetcountry}}
#' }
#' @export
check_raising_factor_inspector <- function(dataframe1,
                                           dataframe2,
                                           dataframe3,
                                           dataframe4,
                                           output,
                                           country_species = list("1" = c("TUN", "ALB", "YFT", "BET", "SKJ"), "4" = c("LOT", "TUN", "ALB", "YFT", "BET", "SKJ", "LTA", "FRI", "BLF", "RAV*", "KAW", "FRZ", "BLT")),
                                           speciesfate = "6",
                                           vesselactivity = c("23", "25", "27", "29"),
                                           threshold = c(0.9, 1.1)) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  catch_weight <- NULL
  sum_catch_weight <- NULL
  rf1 <- NULL
  full_trip_sum_landing_weight <- NULL
  full_trip_sum_catch_weight <- NULL
  lower_threshold <- NULL
  upper_threshold <- NULL
  speciesfate_code <- NULL
  vesselactivity_code <- NULL
  country_fleetcountry <- NULL
  landing_weight <- NULL
  sum_landing_weight <- NULL
  trip_end_full_trip_id <- NULL
  vessel_id <- NULL
  logical_full_trip <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id"),
      column_type = c("character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "vesselactivity_code", "trip_id"),
    column_type = c("character", "numeric", "character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "vesselactivity_code", "trip_id"),
      column_type = c("character", "numeric", "character", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "vesselactivity_code", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("landing_id", "landing_weight", "species_fao_code", "trip_id"),
    column_type = c("character", "numeric", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("landing_id", "landing_weight", "species_fao_code", "trip_id"),
      column_type = c("character", "numeric", "character", "character"),
      output = "message"
    )
  } else {
    dataframe3 <- dataframe3[, c("landing_id", "landing_weight", "species_fao_code", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("trip_id", "trip_end_full_trip_id", "vessel_id", "country_fleetcountry"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("trip_id", "trip_end_full_trip_id", "vessel_id", "country_fleetcountry"),
      column_type = c("character", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe4 <- dataframe4[, c("trip_id", "trip_end_full_trip_id", "vessel_id", "country_fleetcountry")]
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
      output = "message"
    ))
  }
  # Checks the type of country_species
  if (!codama::r_type_checking(
    r_object = country_species,
    type = "list",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = country_species,
      type = "list",
      output = "message"
    ))
  }
  # Checks the type of speciesfate
  if (!codama::r_type_checking(
    r_object = speciesfate,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = speciesfate,
      type = "character",
      output = "message"
    ))
  }
  # Checks the type of vesselactivity
  if (!codama::r_type_checking(
    r_object = vesselactivity,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vesselactivity,
      type = "character",
      output = "message"
    ))
  }
  # Checks the type of threshold
  if (!codama::r_type_checking(
    r_object = threshold,
    type = "numeric",
    length = 2L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold,
      type = "numeric",
      length = 2L,
      output = "message"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Add country_fleetcountry for catch
  dataframe2 <- merge(dataframe2, unique(dataframe4[, c("trip_id", "country_fleetcountry")]), by = "trip_id", all.x = TRUE)
  # Catch filtration for RF1
  ## Selection species when the list is available for the country and selection species
  condition <- as.list(as.data.frame(t(data.frame(country = names(country_species), species = I(unname(country_species))))))
  dataframe2_select_species <- purrr::map(condition, ~ dataframe2 %>% dplyr::filter((country_fleetcountry %in% .x[[1]] & species_fao_code %in% .x[[2]])))
  dataframe2_select_species <- do.call(rbind.data.frame, dataframe2_select_species)
  ## Selection all species when the list is not available for the country
  dataframe2 <- rbind(dataframe2_select_species, dataframe2 %>% dplyr::filter(!(country_fleetcountry %in% names(country_species))))
  ## Selection species fate
  dataframe2 <- dataframe2 %>%
    dplyr::filter((speciesfate_code %in% speciesfate))
  ## Selection vessel activity
  dataframe2 <- dataframe2 %>%
    dplyr::filter(!(vesselactivity_code %in% vesselactivity))
  # Calculation of the sum of weights caught per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_catch_weight = ifelse(all(is.na(catch_weight)), catch_weight[NA_integer_], sum(catch_weight, na.rm = TRUE)))
  # Merge data
  dataframe4 <- merge(dataframe4, dataframe2, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Add country_fleetcountry for landing
  dataframe3 <- merge(dataframe3, unique(dataframe4[, c("trip_id", "country_fleetcountry")]), by = "trip_id", all.x = TRUE)
  # Landing filtration for RF1
  ## Selection species when the list is available for the country and selection species fate
  condition <- as.list(as.data.frame(t(data.frame(country = names(country_species), species = I(unname(country_species))))))
  dataframe3_select_species <- purrr::map(condition, ~ dataframe3 %>% dplyr::filter((country_fleetcountry %in% .x[[1]] & species_fao_code %in% .x[[2]])))
  dataframe3_select_species <- do.call(rbind.data.frame, dataframe3_select_species)
  ## Selection all species when the list is not available for the country
  dataframe3 <- rbind(dataframe3_select_species, dataframe3 %>% dplyr::filter(!(country_fleetcountry %in% names(country_species))))
  # Calculation of the sum of weights caught per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe3 <- dataframe3 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_landing_weight = ifelse(all(is.na(landing_weight)), landing_weight[NA_integer_], sum(landing_weight, na.rm = TRUE)))
  # Merge data
  dataframe4 <- merge(dataframe4, dataframe3, by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Add of a logic that indicates whether the full trip is finished or not
  dataframe4$logical_full_trip <- !is.na(dataframe4$trip_end_full_trip_id)
  # For unfinished full trips (no end-of-full-trip id) indicates the vessel id for the end-of-full-trip id (for each ship, allows you to group together all the trips of the non-finished full trip)
  dataframe4[is.na(dataframe4$trip_end_full_trip_id), "trip_end_full_trip_id"] <- paste0("vessel_id_", dataframe4[is.na(dataframe4$trip_end_full_trip_id), "vessel_id", drop = TRUE])
  # RF1 calculation
  full_trip_id_data_rf1 <- dataframe4 %>%
    dplyr::group_by(trip_end_full_trip_id) %>%
    dplyr::summarise(rf1 = ifelse(all(is.na(sum_landing_weight)), sum_landing_weight[NA_integer_], sum(sum_landing_weight, na.rm = TRUE)) / ifelse(all(is.na(sum_catch_weight)), sum_catch_weight[NA_integer_], sum(sum_catch_weight, na.rm = TRUE)), full_trip_sum_landing_weight = ifelse(all(is.na(sum_landing_weight)), sum_landing_weight[NA_integer_], sum(sum_landing_weight, na.rm = TRUE)), full_trip_sum_catch_weight = ifelse(all(is.na(sum_catch_weight)), sum_catch_weight[NA_integer_], sum(sum_catch_weight, na.rm = TRUE)))
  dataframe4$lower_threshold <- threshold[1]
  dataframe4$upper_threshold <- threshold[2]
  # Selection of user-supplied trips
  dataframe4 <- merge(data.frame(trip_id = dataframe1$trip_id), unique(dataframe4), by.x = "trip_id", by.y = "trip_id", all.x = TRUE)
  # Merge data
  dataframe4 <- merge(dataframe4, full_trip_id_data_rf1, by.x = "trip_end_full_trip_id", by.y = "trip_end_full_trip_id", all.x = TRUE)
  # Compare RF1 to valid threshold
  comparison_less <- codama::vector_comparison(
    first_vector = dataframe4$rf1,
    second_vector = dataframe4$upper_threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  comparison_greater <- codama::vector_comparison(
    first_vector = dataframe4$rf1,
    second_vector = dataframe4$lower_threshold,
    comparison_type = "greater_equal",
    output = "report"
  )
  dataframe4$logical <- comparison_less$logical & comparison_greater$logical
  # Corrects missing RF1s when nothing has been landed and there is no capture
  dataframe4[(is.na(dataframe4$full_trip_sum_landing_weight) | dataframe4$full_trip_sum_landing_weight == 0) & is.na(dataframe4$full_trip_sum_catch_weight), "logical"] <- TRUE
  dataframe4 <- dplyr::relocate(.data = dataframe4, rf1, .after = logical)
  dataframe4 <- subset(dataframe4, select = -c(trip_end_full_trip_id, logical_full_trip, sum_catch_weight, sum_landing_weight, full_trip_sum_landing_weight, full_trip_sum_catch_weight, lower_threshold, upper_threshold, vessel_id, country_fleetcountry))
  if ((sum(dataframe4$logical, na.rm = TRUE) + sum(!dataframe4$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe4$logical)) > 0) {
    all <- c(select, dataframe4$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe4$logical)) > 0) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe4$logical)), "):", paste0(dataframe4$trip_id[is.na(dataframe4$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe4$logical), " trips with RF1 outside defined thresholds or missing")))
  }
  if (output == "report") {
    return(dataframe4)
  }
  if (output == "logical") {
    if (sum(!dataframe4$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
