#' @name logbook_time_route_control
#' @title Gives the inconsistencies between the fishing times or sea times indicated for the route and activities carried out
#' @description The purpose of the logbook_time_route_control function is to provide a table of data that contains an inconsistency between the fishing times or sea times indicated for the route and activities carried out
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_time_route_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_time_route_control () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_time_route_control () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @param threshold_sea_time {\link[base]{numeric}} expected. Default values: 24. Maximum valid time for sea time
#' @param threshold_fishing_time {\link[base]{numeric}} expected. Default values: 13. Maximum valid time for fishing time
#' @param vessel_activity_sea_time {\link[base]{character}} expected. Default values: c("1", "2", "3", "4", "6", "6", "8", "9", "10", "11", "12", "13", "13", "13", "13", "13", "13", "14", "15", "17", "18", "19", "20", "22", "23", "24", "25", "26", "27", "29", "30", "31", "32", "36", "37", "38", "39", "50", "99", "101", "101", "102", "102", "103"). Code activities. First criterion for identifying activities that must have sea time
#' @param object_operation_sea_time {\link[base]{character}} expected. Default values: c(NA, NA, NA, NA, "99", NA, NA, NA, NA, NA, NA, "1", "2", "4", "6", "8", "9", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "99", NA, "99", NA, NA). Code object operation. Second criterion for identifying activities that must have sea time (Indicate NA if you want the code object operation to be missing)
#' @param vessel_activity_fishing_time {\link[base]{character}} expected. Default values: c("2", "3", "4", "6", '6', "8", "12", "13", "13", "13", "13", "13", "13", "14", "15", "17", "19", "20", "23", "25", "26", "27", "29", "30", "31", "102", "102"). Code activities. First criterion for identifying activities that must have fishing time
#' @param object_operation_fishing_time {\link[base]{character}} expected. Default values: c(NA, NA, NA, "99", NA, NA, NA, "1", "2", "4", "6", "8", "9", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "99", NA). Code object operation. Second criterion for identifying activities that must have fishing time (Indicate NA if you want the code object operation to be missing)
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  route_id}}
#'  \item{\code{  route_seatime}}
#'  \item{\code{  route_fishingtime}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  route_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  objectoperation_code}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Day 1 is ok,
#' #Day 2 has a fishing time equal to 0 but includes 1 fishing activitie
#' #Day 3 has a sea time equal to 0 but includes 1 activitie on the sea
#' #Day 4 has a missing sea time,
#' #Day 5 has a missing fishing time,
#' #Day 6 has a sea time greater than threshold (threshold_sea_time),
#' #Day 7 has a fishing time greater than threshold (threshold_fishing_time),
#' #Day 8 has a fishing time greater than sea time
#' dataframe1 <- data.frame(route_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                          route_seatime = c(6, 2, 0, NA, 8, 26, 15, 8),
#'                          route_fishingtime = c(5, 0, 2, 4, NA, 7, 14, 9))
#' dataframe2 <- data.frame(activity_id = c("1", "2", "3", "4", "5"),
#'                          vesselactivity_code = c("2","13", "103","15", "6"),
#'                          route_id = c("1", "1", "2", "2", "3"))
#' dataframe3 <- data.frame(floatingobject_id = c("1"),
#'                          objectoperation_code = c("1"),
#'                          activity_id = c("2"))
#' @expect equal(., structure(list(route_id = c("1", "2", "3", "4", "5", "6", "7", "8"), logical = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), route_seatime = c(6, 2, 0, NA, 8, 26, 15, 8), route_fishingtime = c(5, 0, 2, 4, NA, 7, 14, 9), nb_activity_must_seatime = c(2, 2, 1, 0, 0, 0, 0, 0), nb_activity_must_fishingtime = c(2, 1, 1, 0, 0, 0, 0, 0)), row.names = c(NA, 8L), class = "data.frame"))
#' logbook_time_route_control(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
logbook_time_route_control <- function(dataframe1,
                                       dataframe2,
                                       dataframe3,
                                       output,
                                       threshold_sea_time = 24,
                                       threshold_fishing_time = 13,
                                       vessel_activity_sea_time = c("1", "2", "3", "4", "6", "6", "8", "9", "10", "11", "12", "13", "13", "13", "13", "13", "13", "14", "15", "17", "18", "19", "20", "22", "23", "24", "25", "26", "27", "29", "30", "31", "32", "36", "37", "38", "39", "50", "99", "101", "101", "102", "102", "103"),
                                       object_operation_sea_time = c(NA, NA, NA, NA, "99", NA, NA, NA, NA, NA, NA, "1", "2", "4", "6", "8", "9", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "99", NA, "99", NA, NA),
                                       vessel_activity_fishing_time = c("2", "3", "4", "6", "6", "8", "12", "13", "13", "13", "13", "13", "13", "14", "15", "17", "19", "20", "23", "25", "26", "27", "29", "30", "31", "102", "102"),
                                       object_operation_fishing_time = c(NA, NA, NA, "99", NA, NA, NA, "1", "2", "4", "6", "8", "9", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "99", NA)) {
  # 0 - Global variables assignement ----
  route_id <- NULL
  route_seatime <- NULL
  route_fishingtime <- NULL
  activity_id <- NULL
  threshold <- NULL
  logical_activity_seatime <- NULL
  logical_activity_fishingtime <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("route_id", "route_seatime", "route_fishingtime"),
    column_type = c("character", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("route_id", "route_seatime", "route_fishingtime"),
      column_type = c("character", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("route_id", "route_seatime", "route_fishingtime")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "vesselactivity_code", "route_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "vesselactivity_code", "route_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "vesselactivity_code", "route_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("floatingobject_id", "objectoperation_code", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("floatingobject_id", "objectoperation_code", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("floatingobject_id", "objectoperation_code", "activity_id")]
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
    r_object = threshold_sea_time,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_sea_time,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_fishing_time,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_fishing_time,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (length(vessel_activity_sea_time) != length(object_operation_sea_time)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, the following arguments must be of the same size : \"vessel_activity_sea_time\", \"object_operation_sea_time\"\n"
    )
  }
  if (!codama::r_type_checking(
    r_object = vessel_activity_sea_time,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity_sea_time,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = object_operation_sea_time,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = object_operation_sea_time,
      type = "character",
      output = "error"
    ))
  }
  if (length(vessel_activity_fishing_time) != length(object_operation_fishing_time)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, the following arguments must be of the same size : \"vessel_activity_fishing_time\", \"object_operation_fishing_time\"\n"
    )
  }
  if (!codama::r_type_checking(
    r_object = vessel_activity_fishing_time,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity_fishing_time,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = object_operation_fishing_time,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = object_operation_fishing_time,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$route_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Compare sea time and the threshold
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- threshold_sea_time
  } else {
    dataframe1$threshold <- numeric()
  }
  comparison_sea_threshold <- codama::vector_comparison(
    first_vector = dataframe1$route_seatime,
    second_vector = dataframe1$threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  # Compare fishing time and the threshold
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- threshold_fishing_time
  } else {
    dataframe1$threshold <- numeric()
  }
  comparison_fishing_threshold <- codama::vector_comparison(
    first_vector = dataframe1$route_fishingtime,
    second_vector = dataframe1$threshold,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical <- comparison_sea_threshold$logical & comparison_fishing_threshold$logical
  # Sea time must be equal to or greater than fishing time
  dataframe1[!is.na(dataframe1$route_fishingtime) & dataframe1$route_fishingtime > 0 & !is.na(dataframe1$route_seatime) & dataframe1$route_seatime < dataframe1$route_fishingtime, "logical"] <- FALSE
  # Merge
  data_route_activity_objectoperation <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(route_id))
  data_route_activity_objectoperation <- dplyr::left_join(data_route_activity_objectoperation, dataframe3, by = dplyr::join_by(activity_id))
  # Sea time category conditions
  condition_seatime <- as.list(as.data.frame(t(data.frame(vessel_activity_sea_time, object_operation_sea_time))))
  # Selection of activities that must have sea time
  activity_seatime <- purrr::map(condition_seatime, ~ data_route_activity_objectoperation %>%
                                   dplyr::filter((vesselactivity_code == .x[1] | (is.na(vesselactivity_code) & is.na(.x[1]))) & (objectoperation_code == .x[2] | (is.na(objectoperation_code) & is.na(.x[2])))))
  activity_seatime <- dplyr::bind_rows(activity_seatime)
  # Count the number of activities requiring time at sea per route
  activity_seatime <- activity_seatime %>%
    dplyr::group_by(route_id, route_seatime) %>%
    dplyr::summarise(nb_activity_must_seatime = length(unique(activity_id)), logical_activity_seatime = FALSE, .groups = "drop")
  # Time at sea per route are correct
  activity_seatime[!is.na(activity_seatime$route_seatime) & activity_seatime$route_seatime > 0, "logical_activity_seatime"] <- TRUE
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, activity_seatime, by = dplyr::join_by(route_id, route_seatime))
  dataframe1[is.na(dataframe1$nb_activity_must_seatime), "nb_activity_must_seatime"] <- 0
  dataframe1[is.na(dataframe1$logical_activity_seatime), "logical_activity_seatime"] <- TRUE
  # Fishing time time category conditions
  condition_fishingtime <- as.list(as.data.frame(t(data.frame(vessel_activity_fishing_time, object_operation_fishing_time))))
  # Selection of activities that must have fishing time
  activity_fishingtime <- purrr::map(condition_fishingtime, ~ data_route_activity_objectoperation %>%
                                       dplyr::filter((vesselactivity_code == .x[1] | (is.na(vesselactivity_code) & is.na(.x[1]))) & (objectoperation_code == .x[2] | (is.na(objectoperation_code) & is.na(.x[2])))))
  activity_fishingtime <- dplyr::bind_rows(activity_fishingtime)
  # Count the number of activities requiring time at sea per route
  activity_fishingtime <- activity_fishingtime %>%
    dplyr::group_by(route_id, route_fishingtime) %>%
    dplyr::summarise(nb_activity_must_fishingtime = length(unique(activity_id)), logical_activity_fishingtime = FALSE, .groups = "drop")
  # Fishing time per route are correct
  activity_fishingtime[!is.na(activity_fishingtime$route_fishingtime) & activity_fishingtime$route_fishingtime > 0, "logical_activity_fishingtime"] <- TRUE
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, activity_fishingtime, by = dplyr::join_by(route_id, route_fishingtime))
  dataframe1[is.na(dataframe1$nb_activity_must_fishingtime), "nb_activity_must_fishingtime"] <- 0
  dataframe1[is.na(dataframe1$logical_activity_fishingtime), "logical_activity_fishingtime"] <- TRUE
  dataframe1$logical <- dataframe1$logical & dataframe1$logical_activity_seatime & dataframe1$logical_activity_fishingtime
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(threshold, logical_activity_seatime, logical_activity_fishingtime))
  dataframe1 <- dplyr::relocate(.data = dataframe1, route_seatime, route_fishingtime, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$route_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$route_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " routes with sea time or fishing time inconsistency", collapse = ", ")))
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
