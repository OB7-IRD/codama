#' @name check_length_class_inspector
#' @title Gives the inconsistencies between size class of the samples depending on the species and measurement type and the valid threshold
#' @description The purpose of the check_length_class_inspector function is to provide a table of data that contains an inconsistency between the size class of the samples depending on the species and measurement type and the valid threshold (Default : 80)
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_length_class_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("YFT", "BET", "ALB"). Vector of the species inventory controlled.
#' @param size_measure_type {\link[base]{character}} expected. Default values: "FL". Vector of the size measure type controlled.
#' @param threshold {\link[base]{numeric}} expected. Default values: 80. Maximum size threshold measured
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  sizemeasuretype_code}}
#'  \item{\code{  samplespeciesmeasure_sizeclass}}
#' }
#' @doctest
#' #Activity 1, 2 and 3 are ok,
#' #Activity 4 has size class is greater than threshold,
#' #Activity 5 has size class is missing
#' dataframe1 <- data.frame(samplespeciesmeasure_id = c("1", "2", "3", "4", "5"),
#'                          species_fao_code = c("YFT", "YFT", "LTA", "YFT", "YFT"),
#'                          sizemeasuretype_code = c("FL", "PD1", "FL", "FL", "FL"),
#'                          samplespeciesmeasure_sizeclass = c(10, 90, 85, 83, NA))
#' @expect equal(., structure(list(samplespeciesmeasure_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE), samplespeciesmeasure_sizeclass = c(10, 90, 85, 83, NA)), row.names = c(NA, 5L), class = "data.frame"))
#' check_length_class_inspector(dataframe1, output = "report")
#' @export
check_length_class_inspector <- function(dataframe1,
                                         output,
                                         species = c("YFT", "BET", "ALB"),
                                         size_measure_type = "FL",
                                         threshold = 80) {
  # 0 - Global variables assignement ----
  species_fao_code <- NULL
  sizemeasuretype_code <- NULL
  samplespeciesmeasure_sizeclass <- NULL
  logical_sizeclass <- NULL
  logical_sizemeasuretype <- NULL
  logical_species <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespeciesmeasure_id", "species_fao_code", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass"),
    column_type = c("character", "character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespeciesmeasure_id", "species_fao_code", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass"),
      column_type = c("character", "character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespeciesmeasure_id", "species_fao_code", "sizemeasuretype_code", "samplespeciesmeasure_sizeclass")]
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
  select <- dataframe1$samplespeciesmeasure_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- threshold
  } else {
    dataframe1$threshold <- numeric()
  }
  # Compare size class of the samples
  comparison_sizeclass <- codama::vector_comparison(
    first_vector = dataframe1$samplespeciesmeasure_sizeclass,
    second_vector = dataframe1$threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical_sizeclass <- comparison_sizeclass$logical
  # Compare specie of the samples
  comparison_species <- codama::vector_comparison(
    first_vector = dataframe1$species_fao_code,
    second_vector = species,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_species <- comparison_species$logical
  # Compare size measure type of the samples
  comparison_sizemeasuretype <- codama::vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = size_measure_type,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_sizemeasuretype <- comparison_sizemeasuretype$logical
  dataframe1$logical <- dataframe1$logical_sizeclass | !dataframe1$logical_sizemeasuretype | !dataframe1$logical_species
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, species_fao_code, sizemeasuretype_code, samplespeciesmeasure_sizeclass, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(logical_sizeclass, logical_sizemeasuretype, logical_species, threshold, species_fao_code, sizemeasuretype_code))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples with measurements ", paste0(size_measure_type, collapse = ", "), ", for species ", paste0(species, collapse = ", "), ", greater than ", threshold)))
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
