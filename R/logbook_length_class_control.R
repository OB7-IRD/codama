#' @name logbook_length_class_control
#' @title Gives the inconsistencies between size class of the samples depending on the species and measurement type and the valid threshold
#' @description The purpose of the logbook_length_class_control function is to provide a table of data that contains an inconsistency between the size class of the samples depending on the species and measurement type and the valid threshold (Default : 80)
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_length_class_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param size_measure_type {\link[base]{character}} expected. Default values: c("FL", "PD1"). Vector of the size measure type controlled.
#' @param threshold {\link[base]{numeric}} expected. Default values: 10 and 80. Vector containing the lower and upper acceptable threshold for the size measured
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  sizemeasuretype_code}}
#'  \item{\code{  samplespeciesmeasure_sizeclass}}
#' }
#' @doctest
#' #Activity 1 and 2 are ok,
#' #Activity 3 has size class is less than threshold,
#' #Activity 4 has size class is greater than threshold,
#' #Activity 5 has size class is missing
#' dataframe1 <- data.frame(samplespeciesmeasure_id = c("1", "2", "3", "4", "5"),
#'                          sizemeasuretype_code = c("FL", "PD1", "PD1", "FL", "FL"),
#'                          samplespeciesmeasure_sizeclass = c(25, 75, 5, 83, NA))
#' @expect equal(., structure(list(samplespeciesmeasure_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE), samplespeciesmeasure_sizeclass = c(25, 75, 5, 83, NA)), row.names = c(NA, 5L), class = "data.frame"))
#' logbook_length_class_control(dataframe1, output = "report")
#' @export
logbook_length_class_control <- function(dataframe1,
                                         output,
                                         size_measure_type = c("FL", "PD1"),
                                         threshold = c(10, 80)) {
  # 0 - Global variables assignement ----
  sizemeasuretype_code <- NULL
  samplespeciesmeasure_sizeclass <- NULL
  logical_sizeclass <- NULL
  logical_sizemeasuretype <- NULL
  threshold_min <- NULL
  threshold_max <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespeciesmeasure_id", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass"),
    column_type = c("character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespeciesmeasure_id", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass"),
      column_type = c("character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespeciesmeasure_id", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass")]
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
    r_object = size_measure_type,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type,
      type = "character",
      output = "error"
    ))
  }
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
      output = "error"
    ))
  }
  select <- dataframe1$samplespeciesmeasure_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold_min <- min(threshold)
    dataframe1$threshold_max <- max(threshold)
  } else {
    dataframe1$threshold_min <- numeric()
    dataframe1$threshold_max <- numeric()
  }
  # Compare size class of the samples
  comparison_sizeclass_max <- codama::vector_comparison(
    first_vector = dataframe1$samplespeciesmeasure_sizeclass,
    second_vector = dataframe1$threshold_max,
    comparison_type = "less_equal",
    output = "report"
  )
  comparison_sizeclass_min <- codama::vector_comparison(
    first_vector = dataframe1$samplespeciesmeasure_sizeclass,
    second_vector = dataframe1$threshold_min,
    comparison_type = "greater_equal",
    output = "report"
  )
  dataframe1$logical_sizeclass <- comparison_sizeclass_min$logical & comparison_sizeclass_max$logical
  # Compare size measure type of the samples
  comparison_sizemeasuretype <- codama::vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = size_measure_type,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_sizemeasuretype <- comparison_sizemeasuretype$logical
  dataframe1$logical <- dataframe1$logical_sizeclass | !dataframe1$logical_sizemeasuretype
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, sizemeasuretype_code, samplespeciesmeasure_sizeclass, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(logical_sizeclass, logical_sizemeasuretype, threshold_max, threshold_min, sizemeasuretype_code))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$samplespeciesmeasure_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$samplespeciesmeasure_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples with measurements ", paste0(size_measure_type, collapse = ", "), ", greater than ", max(threshold), " or less than", min(threshold))))
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
