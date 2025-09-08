#' @name logbook_anapo_activity_control
#' @title Gives the inconsistencies between the VMS and the presence of activity
#' @description The purpose of the logbook_anapo_activity_control function is to provide a table of data that contains an inconsistency between the VMS and the presence of activit
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_anapo_activity_control function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_anapo_activity_control function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @param vessel_type {\link[base]{character}} expected. Default values: c("1", "2", "5", "6", "10"). List of vessel type code to be inspected
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  vms_id}}
#'  \item{\code{  vms_date}}
#'  \item{\code{  vessel_code}}
#'  \item{\code{  vms_codevessel}}
#'  \item{\code{  vessel_type}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  vessel_code}}
#' }
#' @doctest
#' #VMS 1, 3, 4 and 5 are ok,
#' #VMS 2 has not linked to an activity
#' dataframe1 <- data.frame(vms_id = c("1", "2", "3", "4", "5"),
#'                          vms_date = as.Date(c("2020/01/01", "2020/01/02", "2020/02/02",
#'                                               "2020/02/02", "2020/02/03")),
#'                          vessel_code = c("1", "1", "2", "3", "4"),
#'                          vms_codevessel = c("vessel_1", "vessel_1", "vessel_2", "vessel_2",
#'                                             "vessel_4"),
#'                          vessel_type = c("1", "1", "1", "1", "3"))
#' dataframe2 <- data.frame(activity_id = c("1", "2"),
#'                          activity_date = as.Date(c("2020/01/01", "2020/02/02")),
#'                          vessel_code = c("1", "2"))
#' @expect equal(., structure(list(vms_id = c("1", "2", "3", "4", "5"), vessel_code = c("1", "1", "2", "3", "4"), vms_date = structure(c(18262, 18263, 18294, 18294, 18295), class = "Date"), vms_codevessel = c("vessel_1", "vessel_1", "vessel_2", "vessel_2", "vessel_4"), vessel_type = c("1", "1", "1", "1", "3"), logical = c(TRUE, FALSE, TRUE, TRUE, TRUE), nb_activity = c(1, 0, 1, 0, 0)), row.names = c(NA, -5L), class = "data.frame"))
#' logbook_anapo_activity_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_anapo_activity_control <- function(dataframe1,
                                           dataframe2,
                                           output,
                                           vessel_type = c("1", "2", "5", "6", "10")) {
  # 0 - Global variables assignement ----
  activity_date <- NULL
  vessel_code <- NULL
  vms_date <- NULL
  activity_date <- NULL
  nb_activity <- NULL
  vms_codevessel <- NULL
  vms_date <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("vms_id", "vms_date", "vessel_code", "vms_codevessel", "vessel_type"),
    column_type = c("character", "Date", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("vms_id", "vms_date", "vessel_code", "vms_codevessel", "vessel_type"),
      column_type = c("character", "Date", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("vms_id", "vms_date", "vessel_code", "vms_codevessel", "vessel_type")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("activity_id", "activity_date", "vessel_code"),
    column_type = c("character", "Date", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("activity_id", "activity_date", "vessel_code"),
      column_type = c("character", "Date", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("activity_id", "activity_date", "vessel_code")]
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
  select <- dataframe1$vms_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculation number date
  dataframe2_nb_activity <- dataframe2 %>%
    dplyr::group_by(activity_date, vessel_code) %>%
    dplyr::summarise(nb_activity = dplyr::n(), .groups = "drop")
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2_nb_activity, by = dplyr::join_by(vms_date == activity_date, vessel_code == vessel_code))
  # Case of NA nb_activity and indicates when there are VMS-related activities
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      nb_activity = dplyr::coalesce(nb_activity, 0),
      logical = nb_activity > 0
    )
  # Case of vessel types not to be controlled
  dataframe1[!(dataframe1$vessel_type %in% vessel_type), "logical"] <- TRUE
  # Case of a VMS point assigned to several different vessels according to Observe/Turbobat (vessel_code) but originating from the same vessel according to VMS (vms_codevessel) (see link between Turbobat file and VMS)
  # If the match is valid (logical TRUE) for one of the vessel, it is assigned to all the others
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(vms_codevessel, vms_date) %>%
    dplyr::mutate(logical = ifelse(any(logical), TRUE, logical)) %>%
    dplyr::ungroup()
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, nb_activity, .after = logical)
  dataframe1 <- dplyr::relocate(.data = dataframe1, vms_date, .after = vessel_code) %>%
    data.frame()
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$vms_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$vms_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " VMS with no activity")))
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
