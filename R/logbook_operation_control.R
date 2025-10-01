#' @name logbook_operation_control
#' @title Gives the inconsistencies between fishing success status, vessel activity, type of school or weight caught
#' @description The purpose of the logbook_operation_control function is to provide a table of data that contains an inconsistency with succes status and vessel activity, the type of school or the weight caught
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_operation_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_activity {\link[base]{character}} expected. Default values: c("6"). Vector of inventory of codes for activities that must have a success status
#' @param school_type {\link[base]{character}} expected, default : c("0"). Vector of inventory of code of the school type must not have specific success status (defined success_status_school_type)
#' @param success_status_school_type {\link[base]{character}} expected, default : c("0", "1"). Vector of inventory of code of the success status must not have specific school type (defined school_type)
#' @param success_status_weight {\link[base]{character}} expected, default : c("0"). Vector of inventory of code of the success status must not have weigth
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  schooltype_code}}
#'  \item{\code{  successstatus_code}}
#'  \item{\code{  activity_weight}}
#'  \item{\code{  vesselactivity_code}}
#' }
#' @doctest
#' #Activity 1, 2, 4 and 8 are ok,
#' #Activity 3 has no weight,
#' #Activity 5 has success status,
#' #Activity 6 has no school type,
#' #Activity 7 has no success status,
#' #Activity 9 has success status in success_status_school_type,
#' #Activity 10 has weight
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
#'                          schooltype_code = c("2", "1", "2", NA, NA, NA, "1", "0", "0", "0", "1"),
#'                          successstatus_code = c("1", "0", "2", NA, "2", "2", NA, "2", "1", "2",
#'                                                 "0"),
#'                          activity_weight = c(10, 0, 0, NA, NA, 6, 8, 2, 8, NA, 4),
#'                          vesselactivity_code = c("6", "6", "6", "1", "1", "6", "6", "6", "6", "6",
#'                                                  "6"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"), logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), vesselactivity_code = c("6", "6", "6", "1", "1", "6", "6", "6", "6", "6", "6"), successstatus_code = c("1", "0", "2", NA, "2", "2", NA, "2", "1", "2", "0"), schooltype_code = c("2", "1", "2", NA, NA, NA, "1", "0", "0", "0", "1"), activity_weight = c(10, 0, 0, NA, NA, 6, 8, 2, 8, NA, 4)), row.names = c(NA, 11L), class = "data.frame"))
#' logbook_operation_control(dataframe1, output = "report")
#' @export
logbook_operation_control <- function(dataframe1,
                                      output,
                                      vessel_activity = c("6"),
                                      school_type = c("0"),
                                      success_status_school_type = c("0", "1"),
                                      success_status_weight = c("0")) {
  # 0 - Global variables assignement ----
  vesselactivity_code <- NULL
  successstatus_code <- NULL
  schooltype_code <- NULL
  activity_weight <- NULL
  threshold <- NULL
  logical_successstatus_vesselactivity <- NULL
  logical_successstatus_schooltype <- NULL
  logical_successstatus_weight <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "schooltype_code", "successstatus_code", "activity_weight", "vesselactivity_code"),
    column_type = c("character", "character", "character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "schooltype_code", "successstatus_code", "activity_weight", "vesselactivity_code"),
      column_type = c("character", "character", "character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "schooltype_code", "successstatus_code", "activity_weight", "vesselactivity_code")]
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
    r_object = vessel_activity,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = school_type,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = school_type,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = success_status_school_type,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = success_status_school_type,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = success_status_weight,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = success_status_weight,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Indicates whether the vessel activity requires a success status
  comparison_successstatus_vesselactivity <- codama::vector_comparison(
    first_vector = dataframe1$vesselactivity_code,
    second_vector = vessel_activity,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_successstatus_vesselactivity <- comparison_successstatus_vesselactivity$logical
  # Case of success status NA: must not have activity 6 (inverse of the result obtained)
  dataframe1$logical_successstatus_vesselactivity[is.na(dataframe1$successstatus_code)] <- !dataframe1$logical_successstatus_vesselactivity[is.na(dataframe1$successstatus_code)]
  # Indicates indeterminate school must not have positive or negative success status
  logical_successstatus_schooltype_indeterminate <- codama::vector_comparison(
    first_vector = dataframe1$schooltype_code,
    second_vector = school_type,
    comparison_type = "difference",
    output = "report"
  )
  dataframe1$logical_successstatus_schooltype_indeterminate <- !logical_successstatus_schooltype_indeterminate$logical
  # Case of success status indeterminate or NA: no constraints
  dataframe1$logical_successstatus_schooltype_indeterminate[!(dataframe1$successstatus_code %in% success_status_school_type)] <- TRUE
  # Indicates whether the success status requires a school type
  dataframe1$logical_successstatus_schooltype <- !is.na(dataframe1$schooltype_code)
  # Case of success status NA: no constraints
  dataframe1$logical_successstatus_schooltype[is.na(dataframe1$successstatus_code)] <- TRUE
  # Indicates whether captured weight is greater than 0
  if (nrow(dataframe1) > 0) {
    dataframe1$threshold <- 0
  } else {
    dataframe1$threshold <- numeric()
  }
  comparison_successstatus_weight <- codama::vector_comparison(
    first_vector = dataframe1$activity_weight,
    second_vector = dataframe1$threshold,
    comparison_type = "greater",
    output = "report"
  )
  dataframe1$logical_successstatus_weight <- comparison_successstatus_weight$logical
  # Case of success status null : must not have weight (inverse of the result obtained)
  dataframe1$logical_successstatus_weight[dataframe1$successstatus_code %in% success_status_weight] <- !dataframe1$logical_successstatus_weight[dataframe1$successstatus_code %in% success_status_weight]
  # NA success status: no constraints
  dataframe1$logical_successstatus_weight[is.na(dataframe1$successstatus_code)] <- TRUE
  # Combines test results
  dataframe1$logical <- dataframe1$logical_successstatus_vesselactivity & dataframe1$logical_successstatus_schooltype_indeterminate & dataframe1$logical_successstatus_schooltype & dataframe1$logical_successstatus_weight
  dataframe1 <- dplyr::relocate(.data = dataframe1, vesselactivity_code, successstatus_code, schooltype_code, activity_weight, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(threshold, logical_successstatus_vesselactivity, logical_successstatus_schooltype_indeterminate, logical_successstatus_schooltype, logical_successstatus_weight))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with a succes status that doesn't match either the vessel activity, the type of school or the weight caught.")))
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
