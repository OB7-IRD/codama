#' @name logbook_floating_object_part_dimensions_when_leaving_control
#' @title Identification of incoherent between the number of dimensions element DFAD when leaving and the object operation
#' @description The purpose of the logbook_floating_object_part_dimensions_when_leaving_control function is to provide a table of data that contains a incoherent with the the number of dimensions element of part of objects DFAD for floating object when leaving and the object operation
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_part_dimensions_when_leaving_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_part_dimensions_when_leaving_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param object_operation {\link[base]{character}} expected. Default values: c("1"). Vector containing the codes for object operation with traditionally has a DFAD element when leaving
#' @param object_material_code {\link[base]{character}} expected. Default values: c("1-1-1-1-6", "1-1-1-1-7", "1-1-1-1-8", "1-1-1-1-9", "1-1-2-10", "1-1-2-11", "1-1-2-12", "1-1-2-13"). Vector containing the codes for object material with traditionally has a DFAD element when leaving
#' @param threshold {\link[base]{numeric}} expected. Default values: 2. Minimum number of object material requirements
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  objectoperation_code}}
#' Dataframe 2:
#'  \item{\code{  floatingobjectpart_id}}
#'  \item{\code{  objectmaterial_code}}
#'  \item{\code{  floatingobjectpart_whenleaving}}
#'  \item{\code{  floatingobject_id}}
#' }
#' @doctest
#' #Floating object 1, and 2 is ok,
#' #Floating object 3 does not have the required number (threshold) of elements (object_material_code)
#' #Floating object 4 has no elements object material
#' dataframe1 <- data.frame(floatingobject_id = c("1", "2", "3", "4"),
#'                          objectoperation_code = c("1", "8", "1", "1"))
#' dataframe2 <- data.frame(floatingobjectpart_id = c("1", "2", "3", "4"),
#'                          objectmaterial_code = c("1-1-1-1-6", "1-1-2-11", "1-1-1-1-7", "1-1"),
#'                          floatingobjectpart_whenleaving = c("3", "5", "1", "true"),
#'                          floatingobject_id = c("1", "1", "3", "3"))
#' @expect equal (.,structure(list(floatingobject_id = c("1", "2", "3", "4"), objectoperation_code = c("1", "8", "1", "1"), logical = c(TRUE, TRUE, FALSE, FALSE), count_objet_part = c(2, 0, 1, 0)), row.names = c(NA, -4L), class = "data.frame"))
#' logbook_floating_object_part_dimensions_when_leaving_control(dataframe1,
#'                                                              dataframe2,
#'                                                              output = "report")
#' @export
logbook_floating_object_part_dimensions_when_leaving_control <- function(dataframe1,
                                                                         dataframe2,
                                                                         output,
                                                                         object_operation = c("1"),
                                                                         object_material_code = c("1-1-1-1-6", "1-1-1-1-7", "1-1-1-1-8", "1-1-1-1-9", "1-1-2-10", "1-1-2-11", "1-1-2-12", "1-1-2-13"),
                                                                         threshold = 2) {
  # 0 - Global variables assignement ----
  floatingobject_id <- NULL
  objectmaterial_code <- NULL
  floatingobjectpart_whenleaving <- NULL
  count_objet_part <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("floatingobject_id", "objectoperation_code"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("floatingobject_id", "objectoperation_code"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("floatingobject_id", "objectoperation_code")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("floatingobjectpart_id", "objectmaterial_code", "floatingobjectpart_whenleaving", "floatingobject_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("floatingobjectpart_id", "objectmaterial_code", "floatingobjectpart_whenleaving", "floatingobject_id"),
      column_type = c("character", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("floatingobjectpart_id", "objectmaterial_code", "floatingobjectpart_whenleaving", "floatingobject_id")]
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
  # Checks the type of object_operation
  if (!codama::r_type_checking(
    r_object = object_operation,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = object_operation,
      type = "character",
      output = "message"
    ))
  }
  # Checks the type of object_material_code
  if (!codama::r_type_checking(
    r_object = object_material_code,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = object_material_code,
      type = "character",
      output = "message"
    ))
  }
  # Checks the type of threshold
  if (!codama::r_type_checking(
    r_object = threshold,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$floatingobject_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Count the number of objet part DFAD when leaving per floating object
  data_objet_part <- dataframe2 %>%
    dplyr::group_by(floatingobject_id) %>%
    dplyr::filter(!is.na(floatingobjectpart_whenleaving) & floatingobjectpart_whenleaving != "false" & objectmaterial_code %in% object_material_code) %>%
    dplyr::summarise(count_objet_part = dplyr::n())
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, data_objet_part, by = dplyr::join_by(floatingobject_id))
  # Replace NA count_floating_object by 0
  dataframe1[is.na(dataframe1$count_objet_part), "count_objet_part"] <- 0
  # Logical
  dataframe1$logical <- FALSE
  dataframe1[dataframe1$count_objet_part >= threshold, "logical"] <- TRUE
  # If object operation not mandatory objet part when leaving
  dataframe1[!(dataframe1$objectoperation_code %in% object_operation),  "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, count_objet_part, .after = logical)
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " floating objects with fewer than ", threshold, " of the required elements when leaving")))
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
