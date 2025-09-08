#' @name check_fishing_context_inspector
#' @title Gives the inconsistencies between the school type and the association
#' @description The purpose of the check_fishing_context_inspector function is to provide a table of data that contains an inconsistency with school type and the association
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_fishing_context_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_fishing_context_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param school_type_object {\link[base]{character}} expected, default : c("1"). Vector of inventory of code of the object school.
#' @param school_type_free {\link[base]{character}} expected, default : c("2"). Vector of inventory of code of the free school.
#' @param school_type_unknown {\link[base]{character}} expected, default : c("0"). Vector of inventory of code of the unknown school.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  schooltype_code}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  observedsystem_id}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  schooltype_code}}
#' }
#' @doctest
#' #Activity 1, 2, 5, 6 and 7 are ok,
#' #Activity 3 has school type object,
#' #Activity 4 has no school type object
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          schooltype_code = c("2", "1", "2", "1", NA, "0", "2"))
#' dataframe2 <- data.frame(observedsystem_id = c("1", "2", "3", "4", "5"),
#'                          activity_id = c("1", "2", "2", "3", "4"),
#'                          schooltype_code = c("2", "2", "1", "1", "2"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE), schooltype_code = c("2", "1", "2", "1", NA, "0", "2"), association_object_count = c(0, 1, 1, 0, 0, 0, 0)), row.names = c(NA, 7L), class = "data.frame"))
#' check_fishing_context_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_fishing_context_inspector <- function(dataframe1,
                                            dataframe2,
                                            output,
                                            school_type_object = c("1"),
                                            school_type_free = c("2"),
                                            school_type_unknown = c("0")) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  schooltype_code <- NULL
  association_object_count <- NULL
  threshold <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "schooltype_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "schooltype_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "schooltype_code")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("observedsystem_id", "activity_id", "schooltype_code"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("observedsystem_id", "activity_id", "schooltype_code"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("observedsystem_id", "activity_id", "schooltype_code")]
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
    r_object = school_type_object,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = school_type_object,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = school_type_free,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = school_type_free,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = school_type_unknown,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = school_type_unknown,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Filters out object-school associations
  dataframe2 <- dataframe2 %>% dplyr::filter(schooltype_code %in% school_type_object)
  # Calculates the number of object-type associations
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(association_object_count = dplyr::n())
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- 0
  } else {
    dataframe1$threshold <- numeric()
  }
  dataframe1$association_object_count[is.na(dataframe1$association_object_count)] <- 0
  # Indicates whether or not an object-type association exists
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$association_object_count,
    second_vector = dataframe1$threshold,
    comparison_type = "greater",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Case of free school : must not have any object-type association (inverse of the result obtained)
  dataframe1$logical[!is.na(dataframe1$schooltype_code) & dataframe1$schooltype_code %in% school_type_free] <- !dataframe1$logical[!is.na(dataframe1$schooltype_code) & dataframe1$schooltype_code %in% school_type_free]
  # Unknown school and NA: no constraint
  dataframe1$logical[is.na(dataframe1$schooltype_code) | dataframe1$schooltype_code %in% school_type_unknown] <- TRUE
  dataframe1 <- dplyr::relocate(.data = dataframe1, schooltype_code, association_object_count, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(threshold))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with school types that do not correspond to the observed associations")))
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
