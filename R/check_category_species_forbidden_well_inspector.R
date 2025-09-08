#' @name check_category_species_forbidden_well_inspector
#' @title Gives the inconsistencies between the weight categories and the species in the well
#' @description The purpose of the check_category_species_forbidden_well_inspector function is to provide a table of data that contains an inconsistency between the weight categories and the species in the well
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_distribution_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param species {\link[base]{character}} expected. Default values: c("SKJ"). Vector of species that must not have certain weight categories in well (weight_category)
#' @param weight_category {\link[base]{character}} expected. Default values: c("W-9"). Vector of weight category codes that must not have certain species in well (species)
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  wellactivityspecies_id}}
#'  \item{\code{  species_fao_code}}
#'  \item{\code{  weightcategory_code}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample well 1 and 2 are ok,
#' #Sample well 3 is associated with the species concerned (species) and the weight category
#' #              concerned (weight_category)
#' dataframe1 <- data.frame(wellactivityspecies_id = c("1", "2", "3"),
#'                          weightcategory_code = c("W-1", "W-9", "W-9"),
#'                          species_fao_code = c("SKJ", "ALB", "SKJ"))
#' @expect equal(., structure(list(wellactivityspecies_id = c("1", "2", "3"), species_fao_code  = c("SKJ", "ALB", "SKJ"), weightcategory_code = c("W-1", "W-9", "W-9"), logical = c(TRUE, TRUE, FALSE)), row.names = c(NA, -3L), class = "data.frame"))
#' check_category_species_forbidden_well_inspector(dataframe1, output = "report")
#' @export
check_category_species_forbidden_well_inspector <- function(dataframe1,
                                                            output,
                                                            species = c("SKJ"),
                                                            weight_category = c("W-9")) {
  # 0 - Global variables assignement ----
  weightcategory_code <- NULL
  species_fao_code <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("wellactivityspecies_id", "species_fao_code", "weightcategory_code"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("wellactivityspecies_id", "species_fao_code", "weightcategory_code"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("wellactivityspecies_id", "species_fao_code", "weightcategory_code")]
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
    r_object = weight_category,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = weight_category,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$wellactivityspecies_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Check weight_category and species
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(logical = ifelse(weightcategory_code %in% weight_category & species_fao_code %in% species, FALSE, TRUE))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$wellactivityspecies_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$wellactivityspecies_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " well activity with inconsistencies between the weight categories and the species in the well", collapse = ", ")))
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
