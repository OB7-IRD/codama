#' @name logbook_temperature_control
#' @title Gives the inconsistencies between activity sea surface temperature and valid threshold
#' @description The purpose of the logbook_temperature_control function is to provide a table of data that contains an inconsistency between the sea surface temperature for the activity and valid threshold
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_temperature_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param threshold {\link[base]{numeric}} expected. Default values: 15 and 32. Vector containing the lower and upper acceptable threshold for sea surface temperature.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_seasurfacetemperature}}
#' }
#' @doctest
#' #Activity 1 is ok,
#' #Activity 2 and 3 have temperatures outside the thresholds
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3"),
#'                          activity_seasurfacetemperature = c(20, 4, 35))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3"), logical = c(TRUE, FALSE, FALSE), activity_seasurfacetemperature = c(20, 4, 35)), row.names = c(NA, 3L), class = "data.frame"))
#' logbook_temperature_control(dataframe1, output = "report")
#' @export
logbook_temperature_control <- function(dataframe1,
                                        output,
                                        threshold = c(15, 32)) {
  # 0 - Global variables assignement ----
  activity_seasurfacetemperature <- NULL
  lower_threshold <- NULL
  upper_threshold <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_seasurfacetemperature"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_seasurfacetemperature"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "activity_seasurfacetemperature")]
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
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Compare RF1 to valid threshold
  if (nrow(dataframe1) > 0) {
    dataframe1$lower_threshold <- threshold[1]
    dataframe1$upper_threshold <- threshold[2]
  } else {
    dataframe1$lower_threshold <- numeric()
    dataframe1$upper_threshold <- numeric()
  }
  comparison_less <- codama::vector_comparison(
    first_vector = dataframe1$activity_seasurfacetemperature,
    second_vector = dataframe1$upper_threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  comparison_greater <- codama::vector_comparison(
    first_vector = dataframe1$activity_seasurfacetemperature,
    second_vector = dataframe1$lower_threshold,
    comparison_type = "greater_equal",
    output = "report"
  )
  dataframe1$logical <- comparison_less$logical & comparison_greater$logical
  # Management of the NA value for the sea surface temperature
  dataframe1[is.na(dataframe1$activity_seasurfacetemperature), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, activity_seasurfacetemperature, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(lower_threshold, upper_threshold))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with sea surface temperature outside defined thresholds")))
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
