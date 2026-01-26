#' @name logbook_weighting_sample_control
#' @title Gives the inconsistencies between the sample weighting and weight in well
#' @description The purpose of the logbook_weighting_sample_control function is to provide a table of data that contains an inconsistency between the sample weighting and weight in well
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weighting_sample_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weighting_sample_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"). list of the inventory of species (FAO code) used to compare to sample weighting.
#' @param epsilon {\link[base]{numeric}} expected, default : 0.01. Gives the threshold at which the difference is considered too large.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sampleactivity_id}}
#'  \item{\code{  sampleactivity_weightedweight}}
#'  \item{\code{  sample_well}}
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  wellactivityspecies_id}}
#'  \item{\code{  wellactivityspecies_weight}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  well_label}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample activity 1, 2, 3, 4 and 5 are ok,
#' #Sample Activity 6 has weights missing from the well,
#' #Sample Activity 7 does not take into account all species
#' #Sample Activity 8 does not take into account all well
#' #Sample Activity 9 has weighted weight missing
#' dataframe1 <- data.frame(sampleactivity_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
#'                          sampleactivity_weightedweight = c(3, 6, 20, 6.25, 13.75, 2, 26, 13, NA),
#'                          sample_well = c("well_1", "well_2", "well_1", "well_1", "well_2",
#'                                          "well_1", "well_1", "well_1", "well_1"),
#'                          activity_id = c("1", "1", "2", "3", "3", "4", "5", "6", "7"))
#' dataframe2 <- data.frame(wellactivityspecies_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
#'                                                     "10", "11", "12", "13"),
#'                          wellactivityspecies_weight = c(3, 2, 4, 1, 15, 5, 5, 11, 4, 26, 2, 13, 6),
#'                          species_fao_code = c("YFT", "SKJ", "ALB", "JOS", "YFT", "FRI", "YFT",
#'                                               "SKJ", "BET", "ALB", "YFT", "BET", "ALB"),
#'                          well_label = c("well_1", "well_2", "well_2", "well_2", "well_1", "well_2",
#'                                         "well_1", "well_2", "well_3", "well_1", "well_1", "well_1",
#'                                         "well_2"),
#'                          activity_id = c("1", "1", "1", "1", "2", "2", "3", "3", "3", "5", "5", "6",
#'                                          "6"))
#' @expect equal(., structure(list(sampleactivity_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), sampleactivity_weightedweight = c(3, 6, 20, 6.25, 13.75, 2, 26, 13, NA), logical = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE), weightedweight_well = c(3, 6, 20, 6.25, 13.75, NA, 28, 19, NA)), row.names = c(NA, 9L), class = "data.frame"))
#' logbook_weighting_sample_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_weighting_sample_control <- function(dataframe1,
                                             dataframe2,
                                             output,
                                             species = c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"),
                                             epsilon = 0.01) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  species_fao_code <- NULL
  wellactivityspecies_weight <- NULL
  well_label <- NULL
  sample_well <- NULL
  weight_all <- NULL
  weight_well <- NULL
  weight_all_sampled <- NULL
  weightedweight_well <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sampleactivity_id", "sampleactivity_weightedweight", "sample_well", "activity_id"),
    column_type = c("character", "numeric", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sampleactivity_id", "sampleactivity_weightedweight", "sample_well", "activity_id"),
      column_type = c("character", "numeric", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sampleactivity_id", "sampleactivity_weightedweight", "sample_well", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("wellactivityspecies_id", "wellactivityspecies_weight", "species_fao_code", "well_label", "activity_id"),
    column_type = c("character", "numeric", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("wellactivityspecies_id", "wellactivityspecies_weight", "species_fao_code", "well_label", "activity_id"),
      column_type = c("character", "numeric", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("wellactivityspecies_id", "wellactivityspecies_weight", "species_fao_code", "well_label", "activity_id")]
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
  select <- dataframe1$sampleactivity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight all well (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_weight_calculation <- dataframe2 %>%
    dplyr::filter(species_fao_code %in% species) %>%
    dplyr::group_by(activity_id) %>%
    dplyr::mutate(weight_all = ifelse(all(is.na(wellactivityspecies_weight)), 0, sum(wellactivityspecies_weight, na.rm = TRUE))) %>%
    dplyr::ungroup()
  # Filter the sampled wells
  dataframe_weight_filter_sampled <- dplyr::inner_join(dataframe_weight_calculation, dataframe1, by = dplyr::join_by(activity_id, well_label == sample_well))
  # Calculation weight all sampled well (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_weight_calculation <- dataframe_weight_filter_sampled %>%
    dplyr::group_by(activity_id, weight_all) %>%
    dplyr::summarise(weight_all_sampled = ifelse(all(is.na(wellactivityspecies_weight)), 0, sum(wellactivityspecies_weight, na.rm = TRUE)), .groups = "drop")
  # Calculation weight sampled well (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_weight_well <- dataframe_weight_filter_sampled %>%
    dplyr::group_by(activity_id, well_label) %>%
    dplyr::summarise(weight_well = ifelse(all(is.na(wellactivityspecies_weight)), 0, sum(wellactivityspecies_weight, na.rm = TRUE)), .groups = "drop")
  # Merge
  dataframe_weight_calculation <- dplyr::left_join(dataframe_weight_calculation, dataframe_weight_well, by = dplyr::join_by(activity_id))
  # Calculation weightedweight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe_weight_calculation <- dataframe_weight_calculation %>%
    dplyr::mutate(weightedweight_well = (weight_well / weight_all_sampled) * weight_all)
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe_weight_calculation, by = dplyr::join_by(activity_id, sample_well == well_label))
  # Weighted comparison
  comparison <- codama::vector_comparison(
    first_vector = dataframe1[, "weightedweight_well", drop = TRUE],
    second_vector = dataframe1[, "sampleactivity_weightedweight", drop = TRUE],
    comparison_type = "equal",
    epsilon = epsilon,
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Management of missing sample
  dataframe1[is.na(dataframe1$sampleactivity_weightedweight), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, weightedweight_well, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(weight_all_sampled, weight_all, weight_well, sample_well, activity_id))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sampleactivity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sampleactivity_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples activity inconsistency with weighted weight", collapse = ", ")))
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
