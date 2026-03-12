#' @name logbook_buoy_time_control
#' @title Gives the inconsistencies between the time elapsed for an operation on the same buoy
#' @description The purpose of the logbook_buoy_time_control function is to provide a table of data that contains an inconsistency between the time elapsed for an operation on a floating object with the same buoy
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_buoy_time_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_buoy_time_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param threshold_time {\link[base]{numeric}} expected. Default values: 60 Maximum valid time threshold (minutes) between operation on the same buoy.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  transmittingbuoy_id}}
#'  \item{\code{  transmittingbuoy_code}}
#'  \item{\code{  transmittingbuoytype_id}}
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_time}}
#'  \item{\code{  trip_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first without geographical location and the second with geographical location), a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Buoy 1, 2, 3, 4 and 5 are ok,
#' #Buoy 6 is followed by an operation on the same object within a time interval shorter than
#' # the threshold
#' #Buoy 7 is preceded by an operation on the same object within a time interval shorter than
#' # the threshold
#' #Buoy 8 is linked to an activity that involves two operations on the same object
#' #Buoy 9 is linked to an activity that involves two operations on the same object
#' dataframe1 <- data.frame(transmittingbuoy_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
#'                          transmittingbuoy_code = c("1", "1", "1", "1", "2", "3", "3", "4", "4"),
#'                          transmittingbuoytype_id = c("1", "2", "2", "2", "2", "1", "1", "2", "2"),
#'                          activity_id = c("1", "2", "3", "4", "5", "6", "7", "8", "8"))
#' dataframe2 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                          activity_date = as.Date(c("2020/01/01", "2020/01/01", "2020/01/01",
#'                                                    "2020/01/01", "2020/01/01", "2020/01/02",
#'                                                    "2020/01/02", "2020/01/03")),
#'                          activity_time = c("15:26:01", "15:36:01", "17:49:00", "18:30:00",
#'                                            "18:31:00", "09:26:01", "09:42:01", "21:35:01"),
#'                          trip_id = c("1", "1", "1", "2", "2", "2", "2", "2"))
#' @expect equal(., structure(list(transmittingbuoy_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), transmittingbuoy_code = c("1", "1", "1", "1", "2", "3", "3", "4", "4"), activity_date = structure(c(18262, 18262, 18262, 18262, 18262, 18263, 18263, 18264, 18264), class = "Date"), activity_time = c("15:26:01", "15:36:01", "17:49:00", "18:30:00", "18:31:00", "09:26:01", "09:42:01", "21:35:01", "21:35:01"), time_interval_before = structure(c(NA, NA, 132.983333333333, NA, NA, NA, 16, NA, 0), class = "difftime", units = "mins"), time_interval_after = structure(c(NA, 132.983333333333, NA, NA, NA, 16, NA, 0, NA), class = "difftime", units = "mins"), logical = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)), row.names = c(NA, -9L), class = c("tbl_df", "tbl", "data.frame")))
#' logbook_buoy_time_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_buoy_time_control <- function(dataframe1,
                                      dataframe2,
                                      output,
                                      threshold_time = 60) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  activity_date <- NULL
  activity_time_bis <- NULL
  trip_id <- NULL
  activity_date_time <- NULL
  transmittingbuoy_code <- NULL
  transmittingbuoytype_id <- NULL
  count <- NULL
  transmittingbuoy_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("transmittingbuoy_id", "transmittingbuoy_code", "transmittingbuoytype_id", "activity_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("transmittingbuoy_id", "transmittingbuoy_code", "transmittingbuoytype_id", "activity_id"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("transmittingbuoy_id", "transmittingbuoy_code", "transmittingbuoytype_id", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "activity_date", "activity_time", "trip_id"),
    column_type = c("character", "Date", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "activity_date", "activity_time", "trip_id"),
      column_type = c("character", "Date", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "activity_date", "activity_time", "trip_id")]
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
    r_object = threshold_time,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_time,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$transmittingbuoy_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge
  dataframe1 <- dplyr::inner_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  # Gives a temporary hour for activities that are missing an hour
  dataframe1$activity_time_bis <- dataframe1$activity_time
  dataframe1[is.na(dataframe1$activity_time), "activity_time_bis"] <- "00:00:00"
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(activity_date_time = as.POSIXct(paste(activity_date, activity_time_bis), tz = "UTC")) %>%
    dplyr::group_by(trip_id, transmittingbuoy_code, transmittingbuoytype_id) %>%
    dplyr::mutate(count = dplyr::n())
  # Sort row and calcul time interval
  dataframe_group_severval <- dataframe1 %>%
    dplyr::filter(count > 1) %>%
    dplyr::mutate(activity_date_time = as.POSIXct(paste(activity_date, activity_time_bis), tz = "UTC")) %>%
    dplyr::group_by(trip_id, transmittingbuoy_code, transmittingbuoytype_id) %>%
    dplyr::arrange(activity_date_time, .by_group = TRUE) %>%
    dplyr::mutate(time_interval_before = difftime(activity_date_time, dplyr::lag(activity_date_time), units = "mins"), time_interval_after = difftime(dplyr::lead(activity_date_time), activity_date_time, units = "mins")) %>%
    dplyr::ungroup()
  # Merge
  dataframe1 <-  dplyr::left_join(dataframe1, dataframe_group_severval[, c("transmittingbuoy_id", "time_interval_before", "time_interval_after")], by = dplyr::join_by(transmittingbuoy_id))
  dataframe1 <- dataframe1 %>%
    dplyr::ungroup()
  # Logical
  dataframe1$logical <- FALSE
  dataframe1[(is.na(dataframe1$time_interval_before) | dataframe1$time_interval_before >= threshold_time) & (is.na(dataframe1$time_interval_after) | dataframe1$time_interval_after >= threshold_time), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, activity_id, transmittingbuoytype_id, activity_time_bis, activity_date_time, count))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$transmittingbuoy_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$transmittingbuoy_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " buoy with operations on floating objects performed in less than ", threshold_time, " seconds", collapse = ", ")))
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
