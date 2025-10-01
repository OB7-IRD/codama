#' @name logbook_weighting_control
#' @title Gives the inconsistencies between the sample weighting and sample weight or landed weight
#' @description The purpose of the logbook_weighting_control  function is to provide a table of data that contains an inconsistency between the sample weighting
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weighting_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weighting_control () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weighting_control () function.
#' @param dataframe4 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weighting_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_type {\link[base]{character}} expected. Default values: c("6", "2"). List of two elements, the first is the seine vessel type code, and the second is the baitboat type code.
#' @param threshold_weight {\link[base]{numeric}} expected. Default values: 100. Seiner threshold weight
#' @param threshold_ratio {\link[base]{numeric}} expected. Default values: 0.95. Percentage threshold between weight and weighted weight for seiners
#' @param sample_type_code_landing_baitboat {\link[base]{character}} expected. Default values: c("11"). List of sample type codes for baitboat fresh landings
#' @param landing_type_baitboat {\link[base]{character}} expected. Default values: c("L-YFT-10", "L-BET-10", "L-TUN-10"). List of codes for fresh baitboat landings
#' @param threshold_baitboat {\link[base]{numeric}} expected. Default values: 1. Threshold for baitboats with respect to the difference between the weighted weight and the landed fresh weight and the difference between the weight and the weighted weight
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#'  \item{\code{  sample_totalweight}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  sampletype_code}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  sampleactivity_id}}
#'  \item{\code{  sampleactivity_weightedweight}}
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  trip_id}}
#'  \item{\code{  vesseltype_code}}
#'  \item{\code{  vesseltype_label1}}
#' }
#' \itemize{
#' Dataframe 4:
#'  \item{\code{  landing_id}}
#'  \item{\code{  landing_weight}}
#'  \item{\code{  weightcategory_code}}
#'  \item{\code{  trip_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1, 3, 4, 7, 9, 12, 17 and 18 are ok,
#' #Sample 2 has a weighted weight ratio over the sum of the weights of small and big individuals below
#' #         the threshold (threshold_ratio),
#' #Sample 5 has a difference between fresh landing and weighted weight above the
#' #         threshold (threshold_baitboat),
#' #Sample 6 has no vessel type,
#' #Sample 8 has no sample type,
#' #Sample 10 has no sample activity,
#' #Sample 11 has a difference between total weight and weighted weight above the
#' #          threshold (threshold_baitboat),
#' #Sample 13 has no sample activity,
#' #Sample 14 has a difference between sum of the weights of small and big individuals and weighted
#' #          weight above the threshold (threshold_baitboat),
#' #Sample 15 has the sum of the weights of small and big individuals above the
#' #          threshold (threshold_weight),
#' #Sample 16 has the sum of the total weight above the threshold (threshold_weight),
#' #Sample 19 has a weighted weight ratio over the total weight below the threshold (threshold_ratio)
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
#'                                        "12", "13", "14", "15", "16", "17", "18", "19"),
#'                          sample_smallsweight = c(10, 32, 2.5, 30, 12, 7, NA, 6, NA, 4, 8, 3, 7, 13,
#'                                                  54, 3, 8, 2, 16),
#'                          sample_bigsweight = c(50, 2, 9, 3, 6, 13, 0, 3, 7, 2, 0, 2, 8, 3, 62, 8,
#'                                                15, 6, 1),
#'                          sample_totalweight = c(NA, NA, NA, 33, 8, 9, 142, 2, 14, 10, 3, 0, NA, 0,
#'                                                 0, 104, 24, 36, 12),
#'                          trip_id =  c("1", "1", "2", "3", "4", "5", "6", "7", "8", "8", "8", "8",
#'                                       "8", "8", "1", "1", "1", "1", "1"),
#'                          sampletype_code = c("1", "1", "1", "11", "11", "1", "1", NA, "1", "1", "1",
#'                                              "1", "1", "1", "1", NA, NA, NA, NA))
#' dataframe2 <- data.frame(sampleactivity_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
#'                                               "11", "12", "13", "14", "15", "16", "17"),
#'                          sampleactivity_weightedweight = c(70, 5, 18, 12, 33, 5, 9, 4, 13, 7, 4, 15,
#'                                                            116, 104, 24, 35, 11),
#'                          sample_id = c("1", "1", "2", "3", "4", "5", "6", "8", "9", "11", "12",
#'                                        "14", "15", "16", "17", "18", "19"))
#' dataframe3 <- data.frame(trip_id = c("1", "2", "3", "4", "6", "7", "8"),
#'                          vesseltype_code = c("6", "6", "2", "2", "3", "2", "2"),
#'                          vesseltype_label = c("vessel_type_1", "vessel_type_1", "vessel_type_2",
#'                                               "vessel_type_2", "vessel_type_3", "vessel_type_2",
#'                                               "vessel_type_2"))
#' dataframe4 <- data.frame(landing_id = c("1", "2", "3", "4", "5", "6"),
#'                          landing_weight = c(85, 26, 30, 2.6, 20, 3),
#'                          weightcategory_code = c("W-1", "W-1", "L-YFT-10", "L-YFT-10", "L-YFT-10",
#'                                                  "L-BET-10"),
#'                          trip_id = c("1", "2", "3", "3", "4", "7"))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"), logical = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE), sample_smallsweight = c(10, 32, 2.5, 30, 12, 7, NA, 6, NA, 4, 8, 3, 7, 13, 54, 3, 8, 2, 16), sample_bigsweight = c(50, 2, 9, 3, 6, 13, 0, 3, 7, 2, 0, 2, 8, 3, 62, 8, 15, 6, 1), sample_totalweight = c(NA, NA, NA, 33, 8, 9, 142, 2, 14, 10, 3, 0, NA, 0, 0, 104, 24, 36, 12), sampletype_code = c("1", "1", "1", "11", "11", "1", "1", NA, "1", "1", "1", "1", "1", "1", "1", NA, NA, NA, NA), weightedweight = c(75, 18, 12, 33, 5, 9, NA, 4, 13, NA, 7, 4, NA, 15, 116, 104, 24, 35, 11), vesseltype_label = c("vessel_type_1", "vessel_type_1", "vessel_type_1", "vessel_type_2", "vessel_type_2", NA, "vessel_type_3", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_2", "vessel_type_1", "vessel_type_1", "vessel_type_1", "vessel_type_1", "vessel_type_1"), sum_landing_weight_baitboat = c(NA, NA, NA, 32.6, 20, NA, NA, 3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), row.names = c(NA, -19L), class = "data.frame"))
#' logbook_weighting_control(dataframe1, dataframe2, dataframe3, dataframe4, output = "report")
#' @export
logbook_weighting_control <- function(dataframe1,
                                      dataframe2,
                                      dataframe3,
                                      dataframe4,
                                      output,
                                      vessel_type = c("6", "2"),
                                      threshold_weight = 100,
                                      threshold_ratio = 0.95,
                                      sample_type_code_landing_baitboat = c("11"),
                                      landing_type_baitboat = c("L-YFT-10", "L-BET-10", "L-TUN-10"),
                                      threshold_baitboat = 1) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  sample_smallsweight <- NULL
  sample_bigsweight <- NULL
  sample_totalweight <- NULL
  weight_calculation <- NULL
  sampleactivity_weightedweight <- NULL
  trip_id <- NULL
  weightcategory_code <- NULL
  landing_weight <- NULL
  weightedweight <- NULL
  sum_landing_weight_baitboat <- NULL
  weight <- NULL
  vesseltype_code <- NULL
  weightedweight_bis <- NULL
  sum_landing_weight_baitboat_bis <- NULL
  sampletype_code <- NULL
  vesseltype_label <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code"),
    column_type = c("character", "numeric", "numeric", "numeric", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code"),
      column_type = c("character", "numeric", "numeric", "numeric", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight", "trip_id", "sampletype_code")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sampleactivity_id", "sample_id", "sampleactivity_weightedweight"),
    column_type = c("character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sampleactivity_id", "sample_id", "sampleactivity_weightedweight"),
      column_type = c("character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("sampleactivity_id", "sample_id", "sampleactivity_weightedweight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("trip_id", "vesseltype_code", "vesseltype_label"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("trip_id", "vesseltype_code", "vesseltype_label"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("trip_id", "vesseltype_code", "vesseltype_label")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe4,
    type = "data.frame",
    column_name = c("landing_id", "trip_id", "landing_weight", "weightcategory_code"),
    column_type = c("character", "character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe4,
      type = "data.frame",
      column_name = c("landing_id", "trip_id", "landing_weight", "weightcategory_code"),
      column_type = c("character", "character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe4 <- dataframe4[, c("landing_id", "trip_id", "landing_weight", "weightcategory_code")]
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
    r_object = vessel_type,
    type = "character",
    length = 2L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_type,
      type = "character",
      length = 2L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_weight,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_weight,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_ratio,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_ratio,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = landing_type_baitboat,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = landing_type_baitboat,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = sample_type_code_landing_baitboat,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = sample_type_code_landing_baitboat,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_baitboat,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_baitboat,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight_calculation = ifelse(all(is.na(c(sample_smallsweight, sample_bigsweight))), 0, sum(c(sample_smallsweight, sample_bigsweight), na.rm = TRUE))) %>%
    dplyr::ungroup()
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight = ifelse(is.na(sample_totalweight) | sample_totalweight == 0, weight_calculation, sample_totalweight)) %>%
    dplyr::ungroup()
  # Calculation weightedweight for sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(weightedweight = ifelse(all(is.na(sampleactivity_weightedweight)), 0, sum(sampleactivity_weightedweight, na.rm = TRUE)), .groups = "drop")
  # Calculation fresh landing baitboat (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe4 <- dataframe4 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::filter(weightcategory_code %in% landing_type_baitboat) %>%
    dplyr::summarise(sum_landing_weight_baitboat = ifelse(all(is.na(landing_weight)), 0, sum(landing_weight, na.rm = TRUE)), .groups = "drop")
  # Merge
  dataframe1$logical <- TRUE
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(sample_id))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3, by = dplyr::join_by(trip_id))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe4, by = dplyr::join_by(trip_id))
  # Case of NA weightedweight or sum_landing_weight_baitboat
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(weightedweight_bis = dplyr::coalesce(weightedweight, 0),
                  sum_landing_weight_baitboat_bis = dplyr::coalesce(sum_landing_weight_baitboat, 0))
  # Check
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[1] & dataframe1$weight > threshold_weight, "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[1] & dataframe1$weightedweight_bis < dataframe1$weight & !((dataframe1$weightedweight_bis / dataframe1$weight) >= threshold_ratio), "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & !is.na(dataframe1$sampletype_code) & dataframe1$sampletype_code %in% sample_type_code_landing_baitboat & abs(dataframe1$weightedweight_bis - dataframe1$sum_landing_weight_baitboat_bis) > threshold_baitboat, "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & !is.na(dataframe1$sampletype_code) & !(dataframe1$sampletype_code %in% sample_type_code_landing_baitboat) & abs(dataframe1$weightedweight_bis - dataframe1$weight) > threshold_baitboat, "logical"] <- FALSE
  # Case NA vesseltype_code sampletype_code
  dataframe1[is.na(dataframe1$vesseltype_code), "logical"] <- FALSE
  dataframe1[!is.na(dataframe1$vesseltype_code) & dataframe1$vesseltype_code == vessel_type[2] & is.na(dataframe1$sampletype_code), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, weight_calculation, weight, vesseltype_code, weightedweight_bis, sum_landing_weight_baitboat_bis))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_smallsweight, sample_bigsweight, sample_totalweight, sampletype_code, weightedweight, vesseltype_label, sum_landing_weight_baitboat, .after = logical) %>%
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
