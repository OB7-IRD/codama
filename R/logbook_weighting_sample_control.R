#' @name logbook_weighting_sample_control
#' @title Gives the inconsistencies between the sample weighting and catch weight
#' @description The purpose of the logbook_weighting_sample_control function is to provide a table of data that contains an inconsistency between the sample weighting and catch weight for activity
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weighting_sample_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weighting_sample_control () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weighting_sample_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"). list of the inventory of species (FAO code) used to compare to sample weighting.
#' @param species_fate {\link[base]{character}} expected. Default values: "6". Vector of inventory of fate used to compare to sample weighting.
#' @param epsilon {\link[base]{numeric}} expected, default : 0.01. Gives the threshold at which the difference is considered too large.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  speciesfate_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  sampleactivity_id}}
#'  \item{\code{  sampleactivity_weightedweight}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Activity 1, 3 and 5 are ok,
#' #Activity 2 has different weight,
#' #Activity 4 has weighted weight
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5"))
#' dataframe2 <- data.frame(catch_id = c("1", "2", "3", "4", "5", "6"),
#'                          catch_weight = c(4, 2, 5, 10, 3, 8),
#'                          speciesfate_code = c("6", "6", "6", "6", "6", "6"),
#'                          species_fao_code = c("YFT", "JOS", "ALB", "YFT", "YFT", "FRI"),
#'                          activity_id = c("1", "1", "1", "2", "2", "5"))
#' dataframe3 <- data.frame(sampleactivity_id = c("1", "2", "3", "4", "5"),
#'                          sampleactivity_weightedweight = c(3, 6, 12.5, NA, 26),
#'                          activity_id = c("1", "1", "2", "3", "4"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, FALSE, TRUE, FALSE, TRUE), weight = c(9, 13, NA, NA, 8), weightedweight = c(9, 12.5, 0, 26, NA)), row.names = c(NA, 5L), class = "data.frame"))
#' logbook_weighting_sample_control(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
logbook_weighting_sample_control <- function(dataframe1,
                                             dataframe2,
                                             dataframe3,
                                             output,
                                             species = c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"),
                                             species_fate = "6",
                                             epsilon = 0.01) {
  # 0 - Global variables assignement ----
  sampleactivity_weightedweight <- NULL
  weightedweight <- NULL
  weight <- NULL
  activity_id <- NULL
  species_fao_code <- NULL
  speciesfate_code <- NULL
  catch_weight <- NULL
  difference <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id"),
      column_type = c("character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id"),
    column_type = c("character", "numeric", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id"),
      column_type = c("character", "numeric", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("catch_id", "catch_weight", "speciesfate_code", "species_fao_code", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("sampleactivity_id", "sampleactivity_weightedweight", "activity_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("sampleactivity_id", "sampleactivity_weightedweight", "activity_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("sampleactivity_id", "sampleactivity_weightedweight", "activity_id")]
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
    r_object = epsilon,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = epsilon,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_weight_calculation <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::filter(species_fao_code %in% species & speciesfate_code %in% species_fate) %>%
    dplyr::summarise(weight = ifelse(all(is.na(catch_weight)), 0, sum(catch_weight, na.rm = TRUE)))
  # Calculation weightedweight for sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_weightedweight_calculation <- dataframe3 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(weightedweight = ifelse(all(is.na(sampleactivity_weightedweight)), 0, sum(sampleactivity_weightedweight, na.rm = TRUE)))
  # Merge
  dataframe2 <- dataframe2 %>%
    dplyr::select(activity_id) %>%
    dplyr::distinct()
  dataframe1 <- dplyr::left_join(dataframe1, dataframe_weight_calculation, by = dplyr::join_by(activity_id))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe_weightedweight_calculation, by = dplyr::join_by(activity_id))
  # Calcul difference
  dataframe1$difference <- ifelse(is.na(dataframe1$weight), 0, dataframe1$weight) - ifelse(is.na(dataframe1$weightedweight), 0, dataframe1$weightedweight)
  dataframe1$difference <- abs(dataframe1$difference)
  if (nrow(dataframe1) > 0) {
    dataframe1$epsilon <- epsilon
  } else {
    dataframe1$epsilon <- numeric()
  }
  # Compare sum difference with epsilon
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$difference,
    second_vector = dataframe1$epsilon,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Management of missing sample
  dataframe1[is.na(dataframe1$weightedweight) | (!is.na(dataframe1$weightedweight) & dataframe1$weightedweight == 0), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(epsilon, difference))
  dataframe1 <- dplyr::relocate(.data = dataframe1, weight, weightedweight, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples inconsistency with weighted weight", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
