#' @name logbook_vessel_activity_missing_floating_object_control
#' @title Identification of incoherent vessel activity and missing floating object
#' @description The purpose of the logbook_vessel_activity_missing_floating_object_control function is to provide a table of data that contains a incoherent vessel activity and missing floating object.
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_vessel_activity_missing_floating_object_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_vessel_activity_missing_floating_object_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_activity {\link[base]{character}} expected. Default values: c("13"). Vector containing the vessel activity code that must have a floating object.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vesselactivity_code}}
#' Dataframe 2:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Activity 1 and 2 are ok,
#' #Activity 3 has floating object missing
#' #Activity 4 has vessel activity missing
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4"),
#'                          vesselactivity_code = c("13", "1", "13", NA))
#' dataframe2 <- data.frame(floatingobject_id = c("1"),
#'                          activity_id = c("1"))
#' @expect equal (.,structure(list(activity_id = c("1", "2", "3", "4"), vesselactivity_code = c("13", "1", "13", NA), logical = c(TRUE, TRUE, FALSE, FALSE), count_floating_object = c(1, 0, 0, 0)), row.names = c(NA, -4L), class = "data.frame"))
#' logbook_vessel_activity_missing_floating_object_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_vessel_activity_missing_floating_object_control <- function(dataframe1,
                                                                    dataframe2,
                                                                    output,
                                                                    vessel_activity = c("13")) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  count_floating_object <- NULL
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
    dataframe1 <- dataframe1[, c("activity_id", "vesselactivity_code"), drop = FALSE]
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
  # Checks the type of country_species
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
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Count the number of floating objects per activity
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(count_floating_object = dplyr::n())
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  # Replace NA count_floating_object by 0
  dataframe1[is.na(dataframe1$count_floating_object), "count_floating_object"] <- 0
  # Logical
  dataframe1$logical <- FALSE
  dataframe1[dataframe1$count_floating_object > 0, "logical"] <- TRUE
  # If vessel activity not mandatory floating object
  dataframe1[!(dataframe1$vesselactivity_code %in% vessel_activity),  "logical"] <- TRUE
  # If missing vessel activity
  dataframe1[is.na(dataframe1$vesselactivity_code),  "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, count_floating_object, .after = logical)
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with missing floating objects")))
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
