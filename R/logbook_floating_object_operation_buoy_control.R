#' @name logbook_floating_object_operation_buoy_control
#' @title Identification of incoherent between the floating object operation and buoy operation
#' @description The purpose of the logbook_floating_object_operation_buoy_control function is to provide a table of data that contains a incoherent with the floating object operation and buoy operation
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_operation_buoy_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_operation_buoy_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param operation_object_buoy {\link[base]{list}} expected. Default values: list("1" = c("2", "4", "5"), "2" = c("4"), "4" = c("3", "4"), "5" = c("1", "3", "4"), "7" = c("3"), "8" = c("4"), "9" = c("4"), "6" = c("3"), "11" = c("1", "2", "3"), "12" = c("3", "1")). List of the inventory of floating object operation (left side) prohibited with buoy operation used (right side)
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  transmittingbuoy_id}}
#'  \item{\code{  transmittingbuoyoperation_code}}
#'  \item{\code{  floatingobject_id}}
#' Dataframe 2:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  objectoperation_code}}
#' }
#' @doctest
#' # Buoy 1 and 2 are ok,
#' # Buoy 3 has incompatible buoy operations and object operations
#' dataframe1 <- data.frame(transmittingbuoy_id = c("1","2","3"),
#'                          transmittingbuoyoperation_code = c("1", "4", "1"),
#'                          floatingobject_id = c("1","2","2"))
#' dataframe2 <- data.frame(floatingobject_id = c("1","2"),
#'                          objectoperation_code = c("1", "11"))
#' @expect equal (.,structure(list(transmittingbuoy_id = c("1", "2", "3"), transmittingbuoyoperation_code = c("1", "4", "1"), objectoperation_code = c("1", "11", "11"), logical = c(TRUE, TRUE, FALSE)), row.names = c(NA, 3L), class = "data.frame"))
#' logbook_floating_object_operation_buoy_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_floating_object_operation_buoy_control <- function(dataframe1,
                                                           dataframe2,
                                                           output,
                                                           operation_object_buoy = list("1" = c("2", "4", "5"), "2" = c("4"), "4" = c("3", "4"), "5" = c("1", "3", "4"), "7" = c("3"), "8" = c("4"), "9" = c("4"), "6" = c("3"), "11" = c("1", "2", "3"), "12" = c("3", "1"))) {
  # 0 - Global variables assignement ----
  floatingobject_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("transmittingbuoy_id", "transmittingbuoyoperation_code", "floatingobject_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("transmittingbuoy_id", "transmittingbuoyoperation_code", "floatingobject_id"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("transmittingbuoy_id", "transmittingbuoyoperation_code", "floatingobject_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("floatingobject_id", "objectoperation_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("floatingobject_id", "objectoperation_code"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("floatingobject_id", "objectoperation_code"), drop = FALSE]
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
  # Checks the type of operation_object_buoy
  if (!codama::r_type_checking(
    r_object = operation_object_buoy,
    type = "list",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = operation_object_buoy,
      type = "list",
      output = "message"
    ))
  }

  select <- dataframe1$transmittingbuoy_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(floatingobject_id))
  # Select tags when the tag code and object code match the argument operation_object_buoy (pair prohibited)
  condition <- as.list(as.data.frame(t(data.frame(object = names(operation_object_buoy), buoy = I(unname(operation_object_buoy))))))
  dataframe1_select_error <- purrr::map(condition, ~ dataframe1 %>% dplyr::filter((objectoperation_code %in% .x[[1]] & transmittingbuoyoperation_code %in% .x[[2]])))
  dataframe1_select_error <- dplyr::bind_rows(dataframe1_select_error)
  # logical
  dataframe1$logical <- FALSE
  # Returns TRUE if the ID is not in the detected list
  dataframe1$logical[!(dataframe1$transmittingbuoy_id %in% dataframe1_select_error$transmittingbuoy_id)] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(floatingobject_id))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) < nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$transmittingbuoy_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(is.na(dataframe1$logical)) > 0) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$transmittingbuoy_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " buoy with an operation on an buoy that is prohibited with the operation on a flotting object link")))
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
