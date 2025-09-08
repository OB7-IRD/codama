#' @name check_ldlf_inspector
#' @title Gives the inconsistencies between the sample measurement types and species or weight values
#' @description The purpose of the check_ldlf_inspector  function is to provide a table of data that contains an inconsistency between the sample measurement types and species or weight values
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_ldlf_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_ldlf_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values:  c("SKJ", "LTA", "FRI"). Vector of the species not to be associated with a type of measure.
#' @param size_measure_type_species {\link[base]{character}} expected. Default values:  c("PD1"). Vector of type of measure not to be associated with species
#' @param size_measure_type_big {\link[base]{character}} expected. Default values:  c("PD1"). Type of measure that must have a total weight or a big fish weight
#' @param size_measure_type_small {\link[base]{character}} expected. Default values: c("FL"). Type of measure that must have a total weight or a small fish weight
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  sizemeasuretype_code}}
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#'  \item{\code{  sample_totalweight}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample species 1, 3 and 4 are ok,
#' #Sample species 2 has a species that is not compatible with the type of measure,
#' #Sample species 5 has no smalls weight nor total weight,
#' #Sample species 6 has no bigs weight nor total weight
#' dataframe1 <- data.frame(samplespecies_id = c("1", "2", "3", "4", "5", "6"),
#'                          species_fao_code = c("SKJ", "SKJ", "JOS", "SKJ", "SKJ", "JOS"),
#'                          sizemeasuretype_code = c("FL", "PD1", "PD1", "FL", "FL", "PD1"),
#'                          sample_id = c("1", "1", "1", "2", "3", "4"))
#' dataframe2 <- data.frame(sample_id = c("1", "2", "3", "4"),
#'                          sample_smallsweight = c(5, 0, 0, 7),
#'                          sample_bigsweight = c(12, NA, 6, 0),
#'                          sample_totalweight = c(0, 23, NA, NA))
#' @expect equal(., structure(list(samplespecies_id = c("1", "2", "3", "4", "5", "6"), logical = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE), sizemeasuretype_code = c("FL", "PD1", "PD1", "FL", "FL", "PD1"), species_fao_code = c("SKJ", "SKJ", "JOS", "SKJ", "SKJ", "JOS"), sample_bigsweight = c(12, 12, 12, NA, 6, 0), sample_smallsweight = c(5, 5, 5, 0, 0, 7), sample_totalweight = c(0, 0, 0, 23, NA, NA)), row.names = c(NA, 6L), class = "data.frame"))
#' check_ldlf_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_ldlf_inspector <- function(dataframe1,
                                 dataframe2,
                                 output,
                                 species = c("SKJ", "LTA", "FRI"),
                                 size_measure_type_species = c("PD1"),
                                 size_measure_type_big = c("PD1"),
                                 size_measure_type_small = c("FL")) {
  # 0 - Global variables assignement ----
  logical_species <- NULL
  logical_bigsweight <- NULL
  logical_smallsweight <- NULL
  sample_id <- NULL
  sizemeasuretype_code <- NULL
  species_fao_code <- NULL
  sample_bigsweight <- NULL
  sample_smallsweight <- NULL
  sample_totalweight <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id", "species_fao_code", "sizemeasuretype_code", "sample_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
      column_type = c("character", "numeric", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight")]
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
    r_object = size_measure_type_species,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type_species,
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
    r_object = size_measure_type_small,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = size_measure_type_small,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$samplespecies_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Check species and measuretype
  comparison_species <- codama::vector_comparison(
    first_vector = dataframe1$species_fao_code,
    second_vector = species,
    comparison_type = "difference",
    output = "report"
  )
  comparison_size_measure_type_species <- codama::vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = size_measure_type_species,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_species <- !(comparison_species$logical & comparison_size_measure_type_species$logical)
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(sample_id))
  # Check bigs weight and measuretype
  comparison_size_measure_type_big <- codama::vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = size_measure_type_big,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_bigsweight <- !(comparison_size_measure_type_big$logical & ((is.na(dataframe1$sample_bigsweight) | (!is.na(dataframe1$sample_bigsweight) & dataframe1$sample_bigsweight == 0)) & (is.na(dataframe1$sample_totalweight) | (!is.na(dataframe1$sample_totalweight) & dataframe1$sample_totalweight == 0))))
  # Check smalls weight and measuretype
  comparison_size_measure_type_small <- codama::vector_comparison(
    first_vector = dataframe1$sizemeasuretype_code,
    second_vector = size_measure_type_small,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_smallsweight <- !(comparison_size_measure_type_small$logical & ((is.na(dataframe1$sample_smallsweight) | (!is.na(dataframe1$sample_smallsweight) & dataframe1$sample_smallsweight == 0)) & (is.na(dataframe1$sample_totalweight) | (!is.na(dataframe1$sample_totalweight) & dataframe1$sample_totalweight == 0))))
  # Check
  dataframe1$logical <- dataframe1$logical_species & dataframe1$logical_bigsweight & dataframe1$logical_smallsweight
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(logical_species, logical_bigsweight, logical_smallsweight, sample_id))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sizemeasuretype_code, species_fao_code, sample_bigsweight, sample_smallsweight, sample_totalweight, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$samplespecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$samplespecies_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " sample with inconsistency between the sample measurement types and species or weight values", collapse = ", ")))
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
