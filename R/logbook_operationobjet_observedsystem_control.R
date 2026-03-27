#' @name logbook_operationobjet_observedsystem_control
#' @title Identification of incoherent operation objet and observed system
#' @description The purpose of the logbook_operationobjet_observedsystem_control function is to provide a table of data that contains a incoherent operation objet and the presence of observed system.
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_operationobjet_observedsystem_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_operationobjet_observedsystem_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param operation_objet {\link[base]{character}} expected. Default values: c("1"). Vector containing the code for the operation on the object that must not have the observed system.
#' @param observed_system {\link[base]{character}} expected. Default values: c("20"). Vector containing the code of the observed system that must not be linked to the object operation.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  objectoperation_code}}
#'  \item{\code{  activity_id}}
#' Dataframe 2:
#'  \item{\code{  observedsystem_id}}
#'  \item{\code{  observedsystem_code}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Floating object 1, 2 and 3 are ok,
#' #Floating object 4 has a conflict between the operation on the object and the observed system
#' #Floating object 5 has operation on the object missing
#' dataframe1 <- data.frame(floatingobject_id = c("1", "2", "3", "4", "5"),
#'                          objectoperation_code = c("1", "1", "2", "1", NA),
#'                          activity_id = c("1", "2", "3", "3", "4"))
#' dataframe2 <- data.frame(observedsystem_id = c("1", "2", "3"),
#'                          observedsystem_code = c("1", "2", "20"),
#'                          activity_id = c("1", "1", "3"))
#' @expect equal (.,structure(list(floatingobject_id = c("1", "2", "3", "4", "5"), objectoperation_code = c("1", "1", "2", "1", NA), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE), count_observed_system = c(0, 0, 1, 1, 0)), row.names = c(NA, 5L), class = "data.frame"))
#' logbook_operationobjet_observedsystem_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_operationobjet_observedsystem_control <- function(dataframe1,
                                                          dataframe2,
                                                          output,
                                                          operation_objet = c("1"),
                                                          observed_system = c("20")) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  observedsystem_code <- NULL
  count_observed_system <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("floatingobject_id", "objectoperation_code", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("floatingobject_id", "objectoperation_code", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("floatingobject_id", "objectoperation_code", "activity_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("observedsystem_id", "observedsystem_code", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("observedsystem_id", "observedsystem_code", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("observedsystem_id", "observedsystem_code", "activity_id"), drop = FALSE]
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
  select <- dataframe1$floatingobject_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Count the number of problematic observed system per activity
  dataframe2 <- dataframe2 %>%
    dplyr::filter(observedsystem_code %in% observed_system) %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(count_observed_system = dplyr::n())
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  # Replace NA count_floating_object by 0
  dataframe1[is.na(dataframe1$count_observed_system), "count_observed_system"] <- 0
  # Logical
  dataframe1$logical <- FALSE
  dataframe1[dataframe1$count_observed_system == 0, "logical"] <- TRUE
  # If operation objet accepts the observed system
  dataframe1[!(dataframe1$objectoperation_code %in% operation_objet),  "logical"] <- TRUE
  # If missing operation objet
  dataframe1[is.na(dataframe1$objectoperation_code),  "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(activity_id))
  dataframe1 <- dplyr::relocate(.data = dataframe1, count_observed_system, .after = logical)
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " floating objects with an observed prohibited system")))
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
