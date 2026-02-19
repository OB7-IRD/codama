#' @name logbook_vessel_activity_object_operation_control
#' @title Identification of incoherent vessel activity and object operation
#' @description The purpose of the logbook_vessel_activity_object_operation_control function is to provide a table of data that contains a incoherent vessel activity and object operation for floating object.
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_vessel_activity_object_operation_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_vessel_activity_object_operation_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_activity {\link[base]{character}} expected. Default values: c("6"). Vector containing the vessel activity code that must not have a floating object with this object operation (operation_objet).
#' @param operation_objet {\link[base]{character}} expected. Default values: c("1"). Vector containing the code for the operation on the object that must not have this vessel activity (vessel_activity).
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vesselactivity_code}}
#' Dataframe 2:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  objectoperation_code}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Activity 1 and 2 are ok,
#' #Activity 3 has conflit between vessel activity and operation objet
#' #Activity 4 has vessel activity missing
#' #Activity 5 has operation objet missing
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5"),
#'                          vesselactivity_code = c("6", "1", "6", NA, "6"))
#' dataframe2 <- data.frame(floatingobject_id = c("1", "2", "3", "4"),
#'                          objectoperation_code = c("2", "1", "1", NA),
#'                          activity_id = c("1", "2", "3", "5"))
#' @expect equal (.,structure(list(activity_id = c("1", "2", "3", "4", "5"), vesselactivity_code = c("6", "1", "6", NA, "6"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE)), row.names = c(NA, 5L), class = "data.frame"))
#' logbook_vessel_activity_object_operation_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_vessel_activity_object_operation_control <- function(dataframe1,
                                                             dataframe2,
                                                             output,
                                                             vessel_activity = c("6"),
                                                             operation_objet = c("1")) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  objectoperation_code <- NULL
  count_operation_objet <- NULL
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
    column_name = c("floatingobject_id", "objectoperation_code", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("floatingobject_id", "objectoperation_code", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("floatingobject_id", "objectoperation_code", "activity_id")]
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
  # Checks the type of operation_objet
  if (!codama::r_type_checking(
    r_object = operation_objet,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = operation_objet,
      type = "character",
      output = "message"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Count the number of problematic operation objet per activity
  dataframe2 <- dataframe2 %>%
    dplyr::filter(objectoperation_code %in% c(operation_objet, NA)) %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(count_operation_objet = dplyr::n())
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  # Replace NA count_operation_objet by 0
  dataframe1[is.na(dataframe1$count_operation_objet), "count_operation_objet"] <- 0
  # Logical
  dataframe1$logical <- FALSE
  dataframe1[dataframe1$count_operation_objet == 0, "logical"] <- TRUE
  # If vessel activity not prohibited operation objet
  dataframe1[!(dataframe1$vesselactivity_code %in% vessel_activity),  "logical"] <- TRUE
  # If missing vessel activity
  dataframe1[is.na(dataframe1$vesselactivity_code),  "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(count_operation_objet))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with vessel activity ", paste0(vessel_activity, collapse = ", "), " and prohibited operation objet ", paste0(operation_objet, collapse = ", "))))
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
