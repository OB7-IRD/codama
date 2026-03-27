#' @name logbook_observed_system_floatting_object_pair_control
#' @title Identification of incoherent between the presence of floatting object and observed system
#' @description The purpose of the logbook_observed_system_floatting_object_pair_control function is to provide a table of data that contains a incoherent between the presence of floatting object and observed system for certain vessel activity.
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_observed_system_floatting_object_pair_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_observed_system_floatting_object_pair_control.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_observed_system_floatting_object_pair_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_activity {\link[base]{character}} expected. Default values: c("6"). Vector containing the code for the vessel activity, which, if it contains a floating object, must also have an observer system (code observed_system) and vice versa.
#' @param observed_system {\link[base]{character}} expected. Default values: c("20"). Vector containing the code of the observed system that must be linked to the vessel activity
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vesselactivity_code}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  observedsystem_id}}
#'  \item{\code{  observedsystem_code}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Activity 1, 2, 3 and 4 are ok,
#' #Activity 5 has no observed system 20
#' #Activity 6 has observed system missing
#' #Activity 7 has floating object missing
#' #Activity 8 has vessel activity missing
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                          vesselactivity_code = c("6", "6", "1", "2", "6", "6", "6", NA))
#' dataframe2 <- data.frame(floatingobject_id = c("1", "2", "3", "4", "5"),
#'                          activity_id = c("1", "4", "5", "6", "8"))
#' dataframe3 <- data.frame(observedsystem_id = c("1", "2", "3", "4", "5", "6"),
#'                          observedsystem_code = c("20", "2", "20", "2", "20", "20"),
#'                          activity_id = c("1", "2", "3", "5", "7", "8"))
#' @expect equal (.,structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"), vesselactivity_code = c("6", "6", "1", "2", "6", "6", "6", NA), logical = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE), flotting_object = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE), observed_system = c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)), row.names = c(NA, -8L), class = "data.frame"))
#' logbook_observed_system_floatting_object_pair_control(dataframe1,
#'                                                       dataframe2,
#'                                                       dataframe3,
#'                                                       output = "report")
#' @export
logbook_observed_system_floatting_object_pair_control <- function(dataframe1,
                                                                  dataframe2,
                                                                  dataframe3,
                                                                  output,
                                                                  vessel_activity = c("6"),
                                                                  observed_system = c("20")) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  observedsystem_code <- NULL
  flotting_object <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "vesselactivity_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "vesselactivity_code"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "vesselactivity_code")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("floatingobject_id", "activity_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("floatingobject_id", "activity_id"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("floatingobject_id", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("observedsystem_id", "observedsystem_code", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("observedsystem_id", "observedsystem_code", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe3 <- dataframe3[, c("observedsystem_id", "observedsystem_code", "activity_id"), drop = FALSE]
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
      output = "message"
    ))
  }
  # Checks the type of vessel_activity
  if (!codama::r_type_checking(
    r_object = vessel_activity,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity,
      type = "character",
      output = "message"
    ))
  }
  # Checks the type of observed_system
  if (!codama::r_type_checking(
    r_object = observed_system,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = observed_system,
      type = "character",
      output = "message"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Detect if flotting object per activity
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(flotting_object = dplyr::n() > 0)
  # Detect if observed system per activity
  dataframe3 <- dataframe3 %>%
    dplyr::filter(observedsystem_code %in% observed_system) %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(observed_system = dplyr::n() > 0)
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3, by = dplyr::join_by(activity_id))
  # Replace NA flotting_object by FALSE
  dataframe1[is.na(dataframe1$flotting_object), "flotting_object"] <- FALSE
  # Replace NA observed_system by FALSE
  dataframe1[is.na(dataframe1$observed_system), "observed_system"] <- FALSE
  # Logical
  dataframe1$logical <- FALSE
  dataframe1[(dataframe1$flotting_object & dataframe1$observed_system) | (!dataframe1$flotting_object & !dataframe1$observed_system), "logical"] <- TRUE
  # If vessel activity not applicable
  dataframe1[!(dataframe1$vesselactivity_code %in% vessel_activity),  "logical"] <- TRUE
  # If missing vessel activity
  dataframe1[is.na(dataframe1$vesselactivity_code),  "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, flotting_object, observed_system, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity (code ", paste0(vessel_activity, collapse = ", "), ") with only one part of the floating object/observed system pair")))
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
