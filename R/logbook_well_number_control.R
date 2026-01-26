#' @name logbook_well_number_control
#' @title Gives the inconsistencies between the well number of sampling activity and well plan trip
#' @description The purpose of the logbook_well_number_control  function is to provide a table of data that contains an inconsistency between sample well number and associated trip well numbers
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_well_number_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_well_number_control () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_well_number_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_type {\link[base]{character}} expected. Default values: c("5", "6", "10"). Vector of codes for vessel types with a well plan.
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sampleactivity_id}}
#'  \item{\code{  sample_well}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  wellactivity_id}}
#'  \item{\code{  well_label}}
#'  \item{\code{  activity_id}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  trip_id}}
#'  \item{\code{  vesseltype_code}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample activity 1, 2 and 3 are ok,
#' #Sample activity 4 has different well,
#' #Sample activity 5 has well missing in sample,
#' #Sample activity 6 has activity missing in well plan,
#' #Sample activity 7 has well missing in well plan
#' dataframe1 <- data.frame(sampleactivity_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          sample_well = c("well_1", "well_2", "well_5", "well_1",
#'                                          NA, "well_1", "well_2"),
#'                          activity_id = c("1", "1", "2", "3", "4", "5", "6"),
#'                          trip_id = c("1", "1", "2", "3", "3", "3", "3"))
#' dataframe2 <- data.frame(wellactivity_id = c("1", "2", "3", "4", "5"),
#'                          well_label = c("well_1", "well_2", "well_2", "well_2", NA),
#'                          activity_id = c("1", "1", "3", "4", "6"))
#' dataframe3 <- data.frame(trip_id = c("1", "2", "3"),
#'                          vesseltype_code = c("6", "1", "6"))
#' @expect equal(., structure(list(sampleactivity_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE), sample_well = c("well_1", "well_2", "well_5", "well_1", NA, "well_1", "well_2")), row.names = c(NA, 7L), class = "data.frame"))
#' logbook_well_number_control(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
logbook_well_number_control <- function(dataframe1,
                                        dataframe2,
                                        dataframe3,
                                        output,
                                        vessel_type = c("5", "6", "10")) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  sample_well <- NULL
  activity_id <- NULL
  wellactivity_id <- NULL
  vesseltype_code <- NULL
  well_label <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sampleactivity_id", "sample_well", "activity_id", "trip_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sampleactivity_id", "sample_well", "activity_id", "trip_id"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sampleactivity_id", "sample_well", "activity_id", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("wellactivity_id", "well_label", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("wellactivity_id", "well_label", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("wellactivity_id", "well_label", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("trip_id", "vesseltype_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("trip_id", "vesseltype_code"),
      column_type = c("character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("trip_id", "vesseltype_code")]
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
    r_object = vessel_type,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_type,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$sampleactivity_id
  nrow_first <- length(select)
  # 2 - Data design ----
  # Merge
  if (nrow(dataframe2) > 0) {
    dataframe2$logical <- TRUE
  } else {
    dataframe2$logical <- logical()
  }
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id == activity_id, sample_well == well_label))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3, by = dplyr::join_by(trip_id))
  # Search well not link
  dataframe1[is.na(dataframe1$wellactivity_id), "logical"] <- FALSE
  # Case the well number is empty
  dataframe1[is.na(dataframe1$sample_well), "logical"] <- FALSE
  # Vessel types without a well plan
  dataframe1[!(dataframe1$vesseltype_code %in% vessel_type), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, activity_id, wellactivity_id, vesseltype_code))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_well, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sampleactivity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples activity inconsistency with well number", collapse = ", ")))
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
