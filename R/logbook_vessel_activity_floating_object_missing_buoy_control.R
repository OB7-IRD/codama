#' @name logbook_vessel_activity_floating_object_missing_buoy_control
#' @title Identification of incoherent vessel activity and missing buoy
#' @description The purpose of the logbook_vessel_activity_floating_object_missing_buoy_control function is to provide a table of data that contains a incoherent vessel activity and missing buoy.
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_vessel_activity_floating_object_missing_buoy_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_vessel_activity_floating_object_missing_buoy_control.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_vessel_activity_floating_object_missing_buoy_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_activity {\link[base]{character}} expected. Default values: c("13"). Vector containing the vessel activity code that must have a buoy.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vesselactivity_code}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  transmittingbuoy_id}}
#'  \item{\code{  floatingobject_id}}
#' }
#' @doctest
#' #Floating object 1, 2 and 3 are ok,
#' #Floating object 4 has buoy missing
#' #Floating object 5 has vessel activity missing
#' dataframe1 <- data.frame(floatingobject_id = c("1", "2", "3", "4", "5"),
#'                          activity_id = c("1", "2", "3", "3", "4"))
#' dataframe2 <- data.frame(activity_id = c("1", "2", "3", "4"),
#'                          vesselactivity_code = c("13", "1", "13", NA))
#' dataframe3 <- data.frame(transmittingbuoy_id = c("1", "2"),
#'                          floatingobject_id = c("1", "3"))
#' @expect equal (.,structure(list(floatingobject_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE), count_buoy = c(1, 0, 1, 0, 0), vesselactivity_code = c("13", "1", "13", "13", NA)), row.names = c(NA, 5L), class = "data.frame"))
#' logbook_vessel_activity_floating_object_missing_buoy_control(dataframe1,
#'                                                              dataframe2,
#'                                                              dataframe3,
#'                                                              output = "report")
#' @export
logbook_vessel_activity_floating_object_missing_buoy_control <- function(dataframe1,
                                                                         dataframe2,
                                                                         dataframe3,
                                                                         output,
                                                                         vessel_activity = c("13")) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  floatingobject_id <- NULL
  count_buoy <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("floatingobject_id", "activity_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("floatingobject_id", "activity_id"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("floatingobject_id", "activity_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "vesselactivity_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "vesselactivity_code"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "vesselactivity_code"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("transmittingbuoy_id", "floatingobject_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("transmittingbuoy_id", "floatingobject_id"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe3 <- dataframe3[, c("transmittingbuoy_id", "floatingobject_id")]
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
  select <- dataframe1$floatingobject_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Count the number of buoy per activity
  dataframe3 <- dataframe3 %>%
    dplyr::group_by(floatingobject_id) %>%
    dplyr::summarise(count_buoy = dplyr::n())
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3, by = dplyr::join_by(floatingobject_id))
  # Replace NA count_buoy by 0
  dataframe1[is.na(dataframe1$count_buoy), "count_buoy"] <- 0
  # Logical
  dataframe1$logical <- FALSE
  dataframe1[dataframe1$count_buoy > 0, "logical"] <- TRUE
  # If vessel activity not mandatory buoy
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  dataframe1[!(dataframe1$vesselactivity_code %in% vessel_activity),  "logical"] <- TRUE
  # If missing vessel activity and no buoy
  dataframe1[is.na(dataframe1$vesselactivity_code) & dataframe1$count_buoy == 0,  "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, count_buoy, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(activity_id))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$floatingobject_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$floatingobject_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " floating object from vessel activity ", paste0(vessel_activity, collapse = ", "), " with missing buoy")))
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
