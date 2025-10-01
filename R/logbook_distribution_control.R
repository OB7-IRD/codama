#' @name logbook_distribution_control
#' @title Gives the inconsistencies between the weights of small and big sample fish and the sum of the small and big weights in the associated well
#' @description The purpose of the logbook_distribution_control  function is to provide a table of data that contains an inconsistency between the small and large sample weights and the sum of the small and big weights of the associated well
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_distribution_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_distribution_control () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_distribution_control () function.
#' @param dataframe4 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_distribution_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species_category_small_big {\link[base]{character}} expected. Default values: c("ALB", "YFT", "BET", "SKJ"). List of the inventory of species (FAO code) used to calculate weight category small and big in well
#' @param species_category_unknown {\link[base]{character}} expected. Default values: c("SKJ"). Vector of species categorized as small if weight category information is missing
#' @param weight_category_small {\link[base]{character}} expected. Default values: c("W-1"). Vector of small weight category codes
#' @param weight_category_big {\link[base]{character}} expected. Default values: c("W-2"). Vector of big weight category codes
#' @param weight_category_unknown {\link[base]{character}} expected. Default values: c("W-9"). Vector of unknown weight category codes
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_well}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  well_id}}
#'  \item{\code{  well_label}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  wellactivity_id}}
#'  \item{\code{  well_id}}
#' }
#' \itemize{
#' Dataframe 4:
#'  \item{\code{  wellactivityspecies_id}}
#'  \item{\code{  wellactivity_id}}
#'  \item{\code{  weightcategory_code}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  wellactivityspecies_weight}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1 and 2 are ok,
#' #Sample 3 has not small weight in well,
#' #Sample 4 has not bigs weight in sample,
#' #Sample 5 has different bigs weight,
#' #Sample 6 and 7 has different small weight
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          sample_well = c("well_1", "well_2", "well_3", "well_4", "well_5",
#'                                          "well_6","well_7"),
#'                          trip_id = c("1", "1", "1", "1", "1", "1", "1"),
#'                          sample_smallsweight = c(6, 25, 14, 0, NA, 10, 8),
#'                          sample_bigsweight = c(12, 0, 9, NA, 6, 0, 0))
#' dataframe2 <- data.frame(well_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          well_label = c("well_1", "well_2", "well_3", "well_4", "well_5", "well_6",
#'                                         "well_7"),
#'                          trip_id = c("1", "1", "1", "1", "1", "1", "1"))
#' dataframe3 <- data.frame(wellactivity_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                          well_id = c("1", "1", "2", "3", "4", "5", "6", "7"))
#' dataframe4 <- data.frame(wellactivityspecies_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
#'                                                     "10", "11", "12", "13", "14"),
#'                          wellactivity_id = c("1", "1", "1", "2", "3", "4", "4", "5", "6", "7",
#'                                              "7", "7", "8", "8"),
#'                          weightcategory_code = c("W-1", "W-9", "W-2", "W-2", "W-1", "W-2", "W-9",
#'                                                  "W-2", "W-2", "W-1", "W-9", "W-9", "W-1", "W-1"),
#'                          species_fao_code = c("BET", "SKJ", "SKJ", "ALB", "SKJ", "BET", "BET",
#'                                               "ALB", "SKJ", "SKJ", "SKJ", "BET", "ALB", "JOS"),
#'                          wellactivityspecies_weight = c(4, 2, 7, 5, 25, 9, 14, 5, 17, 10, 5, 2, 7,
#'                                                         1))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), sample_smallsweight = c(6, 25, 14, 0, NA, 10, 8), sample_bigsweight = c(12, 0, 9, NA, 6, 0, 0), sample_well = c("well_1", "well_2", "well_3", "well_4", "well_5", "well_6", "well_7"), weight_sum_small_filter = c(6, 25, NaN, NaN, NaN, 15, 7), weight_sum_big_filter = c(12, NaN, 9, 5, 17, NaN, NaN), weight_sum_small = c(6, 25, 14, NaN, NaN, 17, 8), weight_sum_big = c(12, NaN, 9, 5, 17, NaN, NaN)), row.names = c(NA, -7L), class = "data.frame"))
#' logbook_distribution_control(dataframe1, dataframe2, dataframe3, dataframe4, output = "report")
#' @export
logbook_distribution_control <- function(dataframe1,
                                         dataframe2,
                                         dataframe3,
                                         dataframe4,
                                         output,
                                         species_category_small_big = c("ALB", "YFT", "BET", "SKJ"),
                                         species_category_unknown = c("SKJ"),
                                         weight_category_small = c("W-1"),
                                         weight_category_big = c("W-2"),
                                         weight_category_unknown = c("W-9")) {
  # 0 - Global variables assignement ----
  well_id <- NULL
  trip_id <- NULL
  well_label <- NULL
  weightcategory_code <- NULL
  wellactivityspecies_weight <- NULL
  species_fao_code <- NULL
  weight_small <- NULL
  weight_small_unknown <- NULL
  sample_smallsweight <- NULL
  sample_bigsweight <- NULL
  weight_big_filter <- NULL
  weight_small_filter <- NULL
  weight_big_filter_species <- NULL
  weight_small_filter_species <- NULL
  weight_sum_small <- NULL
  weight_sum_big <- NULL
  weight_sum_small_filter <- NULL
  weight_sum_big_filter <- NULL
  sample_smallsweight_bis <- NULL
  sample_bigsweight_bis <- NULL
  weight_small_total_bis <- NULL
  weight_big_bis <- NULL
  sample_well <- NULL
  wellactivity_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_well", "trip_id", "sample_smallsweight", "sample_bigsweight"),
    column_type = c("character", "character", "character", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_well", "trip_id", "sample_smallsweight", "sample_bigsweight"),
      column_type = c("character", "character", "character", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_well", "trip_id", "sample_smallsweight", "sample_bigsweight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("well_id", "well_label", "trip_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("well_id", "well_label", "trip_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("well_id", "well_label", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("wellactivity_id", "well_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("wellactivity_id", "well_id"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("wellactivity_id", "well_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("wellactivityspecies_id", "wellactivity_id", "weightcategory_code", "species_fao_code", "wellactivityspecies_weight"),
    column_type = c("character", "character", "character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("wellactivityspecies_id", "wellactivity_id", "weightcategory_code", "species_fao_code", "wellactivityspecies_weight"),
      column_type = c("character", "character", "character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe4 <- dataframe4[, c("wellactivityspecies_id", "wellactivity_id", "weightcategory_code", "species_fao_code", "wellactivityspecies_weight")]
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
    r_object = species_category_small_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_category_small_big,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = species_category_unknown,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_category_unknown,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weight_category_small,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weight_category_small,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weight_category_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weight_category_big,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = weight_category_unknown,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weight_category_unknown,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge
  dataframe2 <- dplyr::left_join(dataframe2, dataframe3, by = dplyr::join_by(well_id))
  dataframe2 <- dplyr::left_join(dataframe2, dataframe4, by = dplyr::join_by(wellactivity_id))
  # Calculation small weight and big weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  # weight_sum_small and weight_sum_big are calculated to give information to the user but are not used in the control
  dataframe2_sum_weight <- dataframe2 %>%
    dplyr::group_by(well_id, trip_id, well_label) %>%
    dplyr::mutate(weight_small_filter = ifelse((weightcategory_code %in% weight_category_small) | (weightcategory_code %in% weight_category_unknown), wellactivityspecies_weight, NA),
                  weight_small_filter_species = ifelse((weightcategory_code %in% weight_category_small & species_fao_code %in% species_category_small_big) | (weightcategory_code %in% weight_category_unknown & species_fao_code %in% species_category_unknown), wellactivityspecies_weight, NA),
                  weight_big_filter = ifelse(weightcategory_code %in% weight_category_big, wellactivityspecies_weight, NA),
                  weight_big_filter_species = ifelse(weightcategory_code %in% weight_category_big & species_fao_code %in% species_category_small_big, wellactivityspecies_weight, NA)) %>%
    dplyr::summarise(weight_sum_small_filter = ifelse(all(is.na(weight_small_filter_species)), NaN, sum(weight_small_filter_species, na.rm = TRUE)),
                     weight_sum_small = ifelse(all(is.na(weight_small_filter)), NaN, sum(weight_small_filter, na.rm = TRUE)),
                     weight_sum_big_filter = ifelse(all(is.na(weight_big_filter_species)), NaN, sum(weight_big_filter_species, na.rm = TRUE)),
                     weight_sum_big = ifelse(all(is.na(weight_big_filter)), NaN, sum(weight_big_filter, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::select(-well_id)
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2_sum_weight, by = dplyr::join_by(trip_id == trip_id, sample_well == well_label))
  # Calculation small weight total (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  # Case of NA
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      sample_smallsweight_bis = dplyr::coalesce(sample_smallsweight, 0),
      sample_bigsweight_bis = dplyr::coalesce(sample_bigsweight, 0),
      weight_big_bis = dplyr::coalesce(weight_sum_big_filter, 0),
      weight_small_total_bis = dplyr::coalesce(weight_sum_small_filter, 0)
    )
  # Check small weight
  comparison_smallsweight <- codama::vector_comparison(
    first_vector = dataframe1$weight_small_total_bis,
    second_vector = dataframe1$sample_smallsweight_bis,
    comparison_type = "equal",
    output = "report"
  )
  # Check smalls weight
  comparison_bigsweight <- codama::vector_comparison(
    first_vector = dataframe1$weight_big_bis,
    second_vector = dataframe1$sample_bigsweight_bis,
    comparison_type = "equal",
    output = "report"
  )
  # Check
  dataframe1$logical <- comparison_smallsweight$logical & comparison_bigsweight$logical
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, weight_small_unknown, weight_small, sample_smallsweight_bis, sample_bigsweight_bis, weight_small_total_bis, weight_big_bis))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_smallsweight, sample_bigsweight, sample_well, weight_sum_small_filter, weight_sum_big_filter, weight_sum_small, weight_sum_big, .after = logical) %>%
    data.frame()
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " sample with inconsistencies in distribution with the well", collapse = ", ")))
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
