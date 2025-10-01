#' @name logbook_floating_object_buoy_id_redundancy_control
#' @title Identification of incoherent buoy operation on a same object
#' @description The purpose of the logbook_floating_object_buoy_id_redundancy_control function is to provide a table of data that contains a incoherent buoy operation on a same object.
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_buoy_id_redundancy_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_buoy_id_redundancy_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param operation_code {\link[base]{character}} expected. Default values: c("2","3"). Vector containing the operation code to detected to avoid incoherence.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  floatingobject_id}}
#' Dataframe 2:
#'  \item{\code{  transmittingbuoy_id}}
#'  \item{\code{  transmittingbuoyoperation_code}}
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  transmittingbuoy_code}}
#' }
#' @doctest
#' dataframe1 <- data.frame(floatingobject_id = c("1","2","3","4", "5"))
#' dataframe2 <- data.frame(transmittingbuoy_id = c("1","2","3","4", "5", "6"),
#'                          floatingobject_id = c("1","2","3","3","5","5"),
#'                          transmittingbuoyoperation_code = c("2","3","2","4", "2", "3"),
#'                          transmittingbuoy_code = c("1","2","3","3", "1", "1"))
#'
#' @expect equal (.,structure(list(floatingobject_id = c("1", "2", "3", "4", "5"), transmittingbuoy_code = c("1", "2", "3", NA, "1"), logical = c(FALSE, FALSE, FALSE, FALSE, TRUE)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -5L)))
#' logbook_floating_object_buoy_id_redundancy_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_floating_object_buoy_id_redundancy_control <- function(dataframe1,
                                                               dataframe2,
                                                               output,
                                                               operation_code = c("2", "3")) {
  # 0 - Global variables assignement ----
  floatingobject_id <- NULL
  transmittingbuoy_code <- NULL
  transmittingbuoyoperation_code <- NULL

  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("floatingobject_id"),
    column_type = c("character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("floatingobject_id"),
      column_type = c("character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("floatingobject_id"), drop = FALSE]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("transmittingbuoy_id", "transmittingbuoyoperation_code", "floatingobject_id", "transmittingbuoy_code"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("transmittingbuoy_id", "transmittingbuoyoperation_code", "floatingobject_id", "transmittingbuoy_code"),
      column_type = c("character", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("transmittingbuoy_id", "transmittingbuoyoperation_code", "floatingobject_id", "transmittingbuoy_code")]
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
    r_object = operation_code,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = operation_code,
      type = "character",
      output = "message"
    ))
  }

  select <- dataframe1$floatingobject_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Fusion of dataframe 1 and datframe 2 (to not loose FOB without buoy)
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(floatingobject_id))

  # Check buoy id and redundance in operation 2,3
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(floatingobject_id, transmittingbuoy_code) %>%
    dplyr::summarise(logical = sum(operation_code %in% transmittingbuoyoperation_code) == length(operation_code), .groups = "drop")


  # Control of raw's number

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
    return(print(paste0("There are ", sum(!dataframe1$logical), " floating objects with a buoy with operation code ", paste0(operation_code, collapse = ", "))))
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
