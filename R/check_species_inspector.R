#' @name check_species_inspector
#' @title Gives the inconsistencies between species sampled and species authorized
#' @description The purpose of the check_species_inspector function is to provide a table of data that contains an inconsistency between the species sampled and species authorized
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_species_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT"). Vector of the species authorized.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  species_fao_code}}
#' }
#' @doctest
#' #Sample species 1 is ok,
#' #Sample species 2 has a species outside the list species
#' dataframe1 <- data.frame(samplespecies_id = c("1", "2"),
#'                          species_fao_code = c("YFT", "JOS"))
#' @expect equal(., structure(list(samplespecies_id = c("1", "2"), logical = c(TRUE, FALSE), species_fao_code = c("YFT", "JOS")), row.names = c(NA, -2L), class = "data.frame"))
#' check_species_inspector(dataframe1, output = "report")
#' @export
check_species_inspector <- function(dataframe1,
                                    output,
                                    species = c("YFT", "SKJ", "BET", "ALB", "LTA", "FRI", "TUN", "KAW", "LOT")) {
  # 0 - Global variables assignement ----
  species_fao_code <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespecies_id", "species_fao_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespecies_id", "species_fao_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id", "species_fao_code")]
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
  select <- dataframe1$samplespecies_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Compare specie of the samples
  comparison_species <- codama::vector_comparison(
    first_vector = dataframe1$species_fao_code,
    second_vector = species,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical <- comparison_species$logical
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, species_fao_code, .after = logical)
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples with species not included in the authorized list (", paste0(species, collapse = ", "), ")", collapse = ", ")))
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
