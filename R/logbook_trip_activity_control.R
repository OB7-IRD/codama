#' @name logbook_trip_activity_control
#' @title Gives the inconsistencies between the trip and the associated activities in terms of presence
#' @description The purpose of the logbook_trip_activity_control function is to provide a table of data that contains an inconsistency between the trip and the presence or not of activity associated with this trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_trip_activity_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_trip_activity_control () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Trip 1 is ok,
#' #Trip 2 is not linked to an activity
#' dataframe1 <- data.frame(trip_id = c("1", "2"))
#' dataframe2 <- data.frame(trip_id = c("1"),
#'                          activity_id = c("1"))
#' @expect equal(., structure(list(trip_id = c("1", "2"), logical = c(TRUE, FALSE)), row.names = c(NA, -2L), class = "data.frame"))
#' logbook_trip_activity_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_trip_activity_control <- function(dataframe1,
                                          dataframe2,
                                          output) {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id"),
      column_type = c("character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "activity_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "activity_id"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "activity_id")]
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
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search trip ID in the associations trip, route, activity
  dataframe2 <- dataframe2[!is.na(dataframe2$activity_id), ]
  # Check
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$trip_id,
    second_vector = dataframe2$trip_id,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$trip_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " trip with no activity")))
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
