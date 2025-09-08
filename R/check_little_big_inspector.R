#' @name check_little_big_inspector
#' @title Gives the inconsistencies between the percentage of little and big fish sampled
#' @description The purpose of the check_little_big_inspector function is to provide a table of data that contains an inconsistency between the percentage of little and big fish sampled
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_little_big_inspector() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_little_big_inspector() function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_little_big_inspector() function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species_big {\link[base]{character}} expected. Default values:  c("YFT", "YFT", "BET", "BET", "ALB", "ALB"). Vector of the species. First criterion for identifying big fish (other values are small fish)
#' @param size_measure_type_big {\link[base]{character}} expected. Default values: c("PD1", "FL", "PD1", "FL", "PD1", "FL"). Vector of the size measure type. Second criterion for identifying big fish (other values are small fish)
#' @param threshold_size_big {\link[base]{numeric}} expected. Default values: c(24, 80, 25, 77, 23.5, 78). Vector for defining the lower or equal for threshold size measurement. Third criterion for identifying big fish (other values are small fish)
#' @param size_measure_type {\link[base]{character}} expected. Default values: c("FL", "PD1"). Vector with the preferred type of size measurement for small fish and then for big fish
#' @param threshold {\link[base]{numeric}} expected. Default values: 0.9. Threshold for percentage of small or big fish
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#'  \item{\code{  sample_totalweight}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  sizemeasuretype_code}}
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  samplespeciesmeasure_sizeclass}}
#'  \item{\code{  samplespeciesmeasure_count}}
#'  \item{\code{  samplespecies_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1, 2, 4, 5, 7, 8 and 9 are ok,
#' #Sample 3 has a percentage of big below the threshold,
#' #Sample 6 has a percentage of little below the threshold,
#' #Sample 10 has a percentage of little below the threshold,
#' #Sample 11 has a percentage of little below the threshold,
#' #Sample 12 has a percentage of big below the threshold
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
#'                                        "12"),
#'                          sample_smallsweight = c(10, 20, 1, 30, 3, 7, 4, 12, 0, 0, 3, 0),
#'                          sample_bigsweight = c(NA, 2, 9, 3, 5, 4, 13, 5, 0, NA, 0, 4),
#'                          sample_totalweight = c(NA, NA, NA, 33, NA, NA, 7, 2, 0, 5, 0, 0))
#' dataframe2 <- data.frame(samplespecies_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
#'                                               "11", "12"),
#'                          species_fao_code = c("YFT", "BET", "BET", "YFT", "ALB", "YFT", "YFT",
#'                                               "BET", "YFT", "ALB", "YFT", "YFT"),
#'                          sizemeasuretype_code = c("PD1", "PD1", "FL","PD1", "FL", "FL", "PD1",
#'                                                   "FL", "PD1", "PD1", "PD1", "PD1"),
#'                          sample_id = c("1", "2", "2", "3", "4", "5", "5", "6", "6", "8", "9", "10"))
#' dataframe3 <- data.frame(samplespeciesmeasure_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9",
#'                                                      "10", "11", "12"),
#'                          samplespeciesmeasure_sizeclass = c(20, 26, 70, 20, 10, 45, 36, 30, 32, 24,
#'                                                             13, 13),
#'                          samplespeciesmeasure_count = c(5, 1, 9, 8, 10, 25, 2, 16, 4, 3, 6, 6),
#'                          samplespecies_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
#'                                               "11", "12"))
#' @expect equal(., structure(list(sample_id = c("1", "10", "11", "12", "2", "3", "4", "5", "6", "7", "8", "9"), logical = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE), sample_smallsweight = c(10, 0, 3, 0, 20, 1, 30, 3, 7, 4, 12, 0), sample_bigsweight = c(NA, NA, 0, 4, 2, 9, 3, 5, 4, 13, 5, 0), sample_totalweight = c(NA, 5, 0, 0, NA, NA, 33, NA, NA, 7, 2, 0), little_percentage = c(1, 1, 0, 0, 0.9, 1, 1, 0.925925925925926, 0.8, 0, 0, 1), big_percentage = c(0, 0, 0, 0, 0.1, 0, 0, 0.0740740740740741, 0.2, 0, 1, 0), measure1_percentage = c(0, 0, 0, 0, 0.9, 0, 1, 0.925925925925926, 0.8, 0, 0, 0), measure2_percentage = c(1, 1, 0, 0, 0.1, 1, 0, 0.0740740740740741, 0.2, 0, 1, 1)), row.names = c(NA, -12L), class = "data.frame"))
#' check_little_big_inspector(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
check_little_big_inspector <- function(dataframe1,
                                       dataframe2,
                                       dataframe3,
                                       output,
                                       species_big = c("YFT", "YFT", "BET", "BET", "ALB", "ALB"),
                                       size_measure_type_big = c("PD1", "FL", "PD1", "FL", "PD1", "FL"),
                                       threshold_size_big = c(24, 80, 25, 77, 23.5, 78),
                                       size_measure_type = c("FL", "PD1"),
                                       threshold = 0.9) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  sample_smallsweight <- NULL
  sample_bigsweight <- NULL
  sample_totalweight <- NULL
  species_fao_code <- NULL
  samplespeciesmeasure_count <- NULL
  sizemeasuretype_code <- NULL
  sample_smallsweight_bis <- NULL
  sample_bigsweight_bis <- NULL
  sample_totalweight_bis <- NULL
  little_percentage <- NULL
  big_percentage <- NULL
  measure1_percentage <- NULL
  measure2_percentage <- NULL
  samplespecies_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
      column_type = c("character", "numeric", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_sizeclass", "samplespeciesmeasure_count", "samplespecies_id"),
    column_type = c("character", "numeric", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_sizeclass", "samplespeciesmeasure_count", "samplespecies_id"),
      column_type = c("character", "numeric", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("samplespeciesmeasure_id", "samplespeciesmeasure_sizeclass", "samplespeciesmeasure_count", "samplespecies_id")]
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
    r_object = species_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = species_big,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = size_measure_type_big,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type_big,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_size_big,
    type = "numeric",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_size_big,
      type = "numeric",
      output = "error"
    ))
  }
  if (length(species_big) != length(size_measure_type_big) || length(species_big) != length(threshold_size_big)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, the following arguments must be of the same size : \"species_big\", \"size_measure_type_big\" and \"threshold_size_big\"\n"
    )
  }
  if (!codama::r_type_checking(
    r_object = size_measure_type,
    type = "character",
    length = 2L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type,
      type = "character",
      length = 2L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(sample_id))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3, by = dplyr::join_by(samplespecies_id))
  # Calculate the number of the measure per sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  total_count <- dataframe1 %>%
    dplyr::group_by(sample_id, sample_smallsweight, sample_bigsweight, sample_totalweight) %>%
    dplyr::summarise(total_count = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)), .groups = "drop")
  # Small and large category conditions
  condition <- as.list(as.data.frame(t(data.frame(species_big, size_measure_type_big, threshold_size_big))))
  # Measurement selection of small individuals
  little <- purrr::map(condition, ~ dataframe1 %>%
                         dplyr::filter(species_fao_code == .x[1] & sizemeasuretype_code == .x[2] & samplespeciesmeasure_sizeclass < as.numeric(.x[3])))
  little <- dplyr::bind_rows(little)
  little <- dataframe1 %>%
    dplyr::filter(!(species_fao_code %in% species_big)) %>%
    dplyr::bind_rows(little)
  # Calculation of the number of measurements of small individuals (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  little <- little %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(little = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Measurement selection of big individuals
  big <- purrr::map(condition, ~ dataframe1 %>%
                      dplyr::filter(species_fao_code == .x[1] & sizemeasuretype_code == .x[2] & samplespeciesmeasure_sizeclass >= as.numeric(.x[3])))
  big <- dplyr::bind_rows(big)
  # Calculation of the number of measurements of big individuals (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  big <- big %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(big = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Calculation of the number of measurements of type measurements 1 (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  measure1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::filter(sizemeasuretype_code == size_measure_type[1]) %>%
    dplyr::summarise(measure1 = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Calculation of the number of measurements of type measurements 2 (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  measure2 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::filter(sizemeasuretype_code == size_measure_type[2]) %>%
    dplyr::summarise(measure2 = ifelse(all(is.na(samplespeciesmeasure_count)), 0, sum(samplespeciesmeasure_count, na.rm = TRUE)))
  # Merge
  total_count <- dplyr::left_join(total_count, little, by = dplyr::join_by(sample_id))
  total_count <- dplyr::left_join(total_count, big, by = dplyr::join_by(sample_id))
  total_count <- dplyr::left_join(total_count, measure1, by = dplyr::join_by(sample_id))
  total_count <- dplyr::left_join(total_count, measure2, by = dplyr::join_by(sample_id))
  # Calculates percentages
  total_count <- total_count %>%
    dplyr::mutate(little_percentage = little / total_count, big_percentage = big / total_count, measure1_percentage = measure1 / total_count, measure2_percentage = measure2 / total_count)
  # Case of NA sample_smallsweight, sample_bigsweight or sample_totalweight
  total_count <- total_count %>%
    dplyr::mutate(sample_smallsweight_bis = dplyr::coalesce(sample_smallsweight, 0),
                  sample_bigsweight_bis = dplyr::coalesce(sample_bigsweight, 0),
                  sample_totalweight_bis = dplyr::coalesce(sample_totalweight, 0))
  # Case of NA little_percentage, big_percentage, measure1_percentage, measure2_percentage
  total_count[is.na(total_count$little_percentage), "little_percentage"] <- 0
  total_count[is.na(total_count$big_percentage), "big_percentage"] <- 0
  total_count[is.na(total_count$measure1_percentage), "measure1_percentage"] <- 0
  total_count[is.na(total_count$measure2_percentage), "measure2_percentage"] <- 0
  # Check
  total_count$logical <- !((total_count$sample_smallsweight_bis > 0 & total_count$sample_bigsweight_bis == 0 & total_count$little_percentage < threshold) |
                             ((total_count$sample_totalweight_bis != 0 | (total_count$sample_smallsweight_bis > 0 & total_count$sample_bigsweight_bis > 0)) & total_count$measure1_percentage > total_count$measure2_percentage & total_count$little_percentage < threshold) |
                             (total_count$sample_smallsweight_bis == 0 & total_count$sample_bigsweight_bis > 0 & total_count$big_percentage < threshold) |
                             ((total_count$sample_totalweight_bis != 0 | (total_count$sample_smallsweight_bis > 0 & total_count$sample_bigsweight_bis > 0)) & total_count$measure1_percentage < total_count$measure2_percentage & total_count$big_percentage < threshold))
  # Case of NA
  total_count[is.na(total_count$logical), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  total_count <- subset(total_count, select = -c(total_count, little, big, measure1, measure2, sample_smallsweight_bis, sample_bigsweight_bis, sample_totalweight_bis))
  total_count <- dplyr::relocate(.data = total_count, sample_smallsweight, sample_bigsweight, sample_totalweight, little_percentage, big_percentage, measure1_percentage, measure2_percentage, .after = logical) %>%
    data.frame()
  if ((sum(total_count$logical, na.rm = TRUE) + sum(!total_count$logical, na.rm = TRUE)) != nrow_first || any(is.na(total_count$logical))) {
    all <- c(select, total_count$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(total_count$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(total_count$logical)), "):", paste0(total_count$sample_id[is.na(total_count$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!total_count$logical), " samples inconsistency with percentage of little and big fish", collapse = ", ")))
  }
  if (output == "report") {
    return(total_count)
  }
  if (output == "logical") {
    if (sum(!total_count$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
