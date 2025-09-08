#' @name logbook_temporal_limit_control
#' @title Gives the inconsistencies between trip start and end date and the dates of activity
#' @description The purpose of the logbook_temporal_limit_control function is to provide a table of data that contains an inconsistency between trip start and end date and the dates of activity (activity date outside the trip ranges, several occurrences of the activity date, ...)
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_temporal_limit_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_temporal_limit_control () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first at the trip level and the second at the activity date level), a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_startdate}}
#'  \item{\code{  trip_enddate}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  route_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  trip_id}}
#' }
#' @doctest
#' #Trip 1 is ok,
#' #Trip 2 has an extra day (2020/01/03),
#' #Trip 3 has no trip_startdate,
#' #Trip 4 has no trip_enddate,
#' #Trip 5 has a missing day (2020/02/02),
#' #Trip 6 has a double day (2020/02/13)
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5", "6"),
#'                          trip_startdate = as.Date(c("2020/01/01", "2020/01/01", NA, "2020/01/24",
#'                                                     "2020/02/01", "2020/02/13")),
#'                          trip_enddate = as.Date(c("2020/01/02", "2020/01/02", "2020/01/24", NA,
#'                                                   "2020/02/03", "2020/02/14")))
#' dataframe2 <- data.frame(route_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11",
#'                                       "12"),
#'                          activity_date = as.Date(c("2020/01/01", "2020/01/02", "2020/01/01",
#'                                                    "2020/01/02", "2020/01/03", "2020/01/24",
#'                                                    "2020/01/24", "2020/02/01", "2020/02/03",
#'                                                    "2020/02/13", "2020/02/13", "2020/02/14")),
#'                          trip_id = c("1", "1", "2", "2", "2", "3", "4", "5", "5", "6", "6", "6"))
#' @expect equal(., list(structure(list(trip_id = c("1", "2", "3", "4", "5", "6"), logical = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)), row.names = c(NA, -6L), class = "data.frame"), structure(list(trip_id = c("1", "1", "2", "2", "2", "3", "4", "5", "5", "6", "6"), trip_startdate = structure(c(18262, 18262, 18262, 18262, 18262, NA, 18285, 18293, 18293, 18305, 18305), class = "Date"), trip_enddate = structure(c(18263, 18263, 18263, 18263, 18263, 18285, NA, 18295, 18295, 18306, 18306), class = "Date"), activity_date = structure(c(18262, 18263, 18262, 18263, 18264, 18285, 18285, 18293, 18295, 18305, 18306), class = "Date"), inter_activity_date = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE), exter_activity_date = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), count_freq = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L), logical = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)), row.names = c(NA, -11L), class = "data.frame")))
#' logbook_temporal_limit_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_temporal_limit_control <- function(dataframe1,
                                           dataframe2,
                                           output) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  trip_startdate <- NULL
  trip_enddate <- NULL
  activity_date <- NULL
  inter_activity_date <- NULL
  exter_activity_date <- NULL
  nb_day <- NULL
  logical_nb_day <- NULL
  logical_tmp <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_startdate", "trip_enddate"),
    column_type = c("character", "Date", "Date"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_startdate", "trip_enddate"),
      column_type = c("character", "Date", "Date"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_startdate", "trip_enddate")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("route_id", "activity_date", "trip_id"),
    column_type = c("character", "Date", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("route_id", "activity_date", "trip_id"),
      column_type = c("character", "Date", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("route_id", "activity_date", "trip_id")]
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
  # Merge date
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  # Calculate the temporal indicator per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  trip_date_activity_data_detail <- dataframe1
  # Calculation if the date is in the interval of the beginning of the trip and the end of the trip
  trip_date_activity_data_detail$trip_startdate_greater_equal <- codama::vector_comparison(
    first_vector = dataframe1$activity_date,
    second_vector = dataframe1$trip_startdate,
    comparison_type = "greater_equal",
    output = "report"
  )$logical
  trip_date_activity_data_detail$trip_enddate_less_equal <- codama::vector_comparison(
    first_vector = dataframe1$activity_date,
    second_vector = dataframe1$trip_enddate,
    comparison_type = "less_equal",
    output = "report"
  )$logical
  trip_date_activity_data_detail$inter_activity_date <- trip_date_activity_data_detail$trip_startdate_greater_equal & trip_date_activity_data_detail$trip_enddate_less_equal
  # Calculation if the date is outside the interval of the beginning of the trip and the end of the trip
  trip_date_activity_data_detail$trip_startdate_less <- codama::vector_comparison(
    first_vector = dataframe1$activity_date,
    second_vector = dataframe1$trip_startdate,
    comparison_type = "less",
    output = "report"
  )$logical
  trip_date_activity_data_detail$trip_enddate_greater <- codama::vector_comparison(
    first_vector = dataframe1$activity_date,
    second_vector = dataframe1$trip_enddate,
    comparison_type = "greater",
    output = "report"
  )$logical
  trip_date_activity_data_detail$exter_activity_date <- trip_date_activity_data_detail$trip_startdate_less | trip_date_activity_data_detail$trip_enddate_greater
  # Calculates the number of occurrences of each activity date
  trip_date_activity_data_detail <- trip_date_activity_data_detail %>%
    dplyr::group_by(trip_id, trip_startdate, trip_enddate, activity_date, inter_activity_date, exter_activity_date) %>%
    dplyr::summarise(count_freq = length(activity_date), .groups = "drop")
  # Calculation if an inconsistency among the different tests on the date has been found
  trip_date_activity_data_detail$logical <- trip_date_activity_data_detail$inter_activity_date & !trip_date_activity_data_detail$exter_activity_date & trip_date_activity_data_detail$count_freq == 1
  trip_date_activity_data_detail <- trip_date_activity_data_detail %>%
    data.frame()
  # Calculation if the number of days is consistent and if there are inconsistencies in the dates for the trips
  dataframe1 <- trip_date_activity_data_detail %>%
    dplyr::group_by(trip_id, trip_startdate, trip_enddate) %>%
    dplyr::summarise(nb_day = length(activity_date), logical_tmp = all(logical), .groups = "drop")
  # Calculation if an inconsistency among the different tests on the trip has been found
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(logical_nb_day = (1 + trip_enddate - trip_startdate) == nb_day) %>%
    dplyr::group_by(trip_id, trip_startdate, trip_enddate) %>%
    dplyr::summarise(logical = all(logical_nb_day, logical_tmp), .groups = "drop")
  # Management of missing trip start and end date
  dataframe1[is.na(dataframe1$trip_startdate) | is.na(dataframe1$trip_enddate), "logical"] <- FALSE
  dataframe1 <- subset(dataframe1, select = -c(trip_startdate, trip_enddate, logical_tmp, nb_day)) %>%
    data.frame()
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " trips with missing or surplus days")))
  }
  if (output == "report") {
    return(list(dataframe1, trip_date_activity_data_detail))
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
