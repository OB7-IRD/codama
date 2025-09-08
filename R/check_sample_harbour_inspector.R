#' @name check_sample_harbour_inspector
#' @title Gives inconsistencies between the presence of a sample and the absence of a harbour of landing
#' @description The purpose of the check_sample_harbour_inspector function is to provide a table of data that contains an inconsistency between the presence of a sample and the absence of a harbour of landing
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sample_harbour_inspector function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sample_harbour_inspector function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  harbour_id_landing}}
#'  \item{\code{  harbour_label_landing}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1 is ok,
#' #Sample 2 has no landing harbour
#' dataframe1 <- data.frame(sample_id = c("1", "2"),
#'                          trip_id = c("1", "2"))
#' dataframe2 <- data.frame(trip_id = c("1"),
#'                          harbour_id_landing = c("1"),
#'                          harbour_label_landing = c("harbour_1"))
#' @expect equal(., structure(list(sample_id = c("1", "2"), logical = c(TRUE, FALSE), harbour_label_landing = c("harbour_1", NA)), row.names = 1:2, class = "data.frame"))
#' check_sample_harbour_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_sample_harbour_inspector <- function(dataframe1,
                                           dataframe2,
                                           output) {
  # 0 - Global variables assignement ----
  harbour_label_landing <- NULL
  harbour_id_landing <- NULL
  trip_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "trip_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "trip_id"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "harbour_id_landing", "harbour_label_landing"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "harbour_id_landing", "harbour_label_landing"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "harbour_id_landing", "harbour_label_landing")]
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
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  if (nrow(dataframe1) > 0) {
    dataframe1$logical <- TRUE
  } else {
    dataframe1$logical <- logical()
  }
  # Check if missing harbour
  dataframe1[is.na(dataframe1$harbour_id_landing), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, harbour_label_landing, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(harbour_id_landing, trip_id))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " sample without harbour of landing", collapse = ", ")))
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
