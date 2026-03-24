#' @name logbook_floating_object_part_when_leaving_control
#' @title Identification of incoherent between the number of part of objects for floating object when arriving and when leaving
#' @description The purpose of the logbook_floating_object_part_when_leaving_control function is to provide a table of data that contains a incoherent with the number of part of objects for floating object when arriving are  greater than  when leaving
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_part_when_leaving_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_part_when_leaving_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param object_operation {\link[base]{character}} expected. Default values: c("8"). Vector containing the codes for object operation with traditionally more items at the end than at the beginning
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  floatingobject_id}}
#'  \item{\code{  objectoperation_code}}
#' Dataframe 2:
#'  \item{\code{  floatingobjectpart_id}}
#'  \item{\code{  floatingobjectpart_whenarriving}}
#'  \item{\code{  floatingobjectpart_whenleaving}}
#'  \item{\code{  floatingobject_id}}
#' }
#' @doctest
#' #Floating object 1, 2 and 3 is ok,
#' #Floating object 4 has no elements that have been added
#' #Floating object 5 does not include all of these elements that have been preserved
#' dataframe1 <- data.frame(floatingobject_id = c("1", "2", "3", "4", "5"),
#'                          objectoperation_code = c("1", "8", "8", "8", "8"))
#' dataframe2 <- data.frame(floatingobjectpart_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                          floatingobjectpart_whenarriving = c("true", NA, "false", "true", NA,
#'                                                              "true", "true", NA),
#'                          floatingobjectpart_whenleaving = c(NA, "true", "true", "true", "9",
#'                                                             "true", "false", "true"),
#'                          floatingobject_id = c("1", "2", "2", "3", "3", "4", "5", "5"))
#' @expect equal (.,structure(list(floatingobject_id = c("1", "2", "3", "4", "5"), objectoperation_code = c("1", "8", "8", "8", "8"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE), objet_part_additional_leaving = c(FALSE, TRUE, TRUE, FALSE, TRUE), objet_part_retained_leaving = c(FALSE, TRUE, TRUE, TRUE, FALSE)), row.names = c(NA, 5L), class = "data.frame"))
#' logbook_floating_object_part_when_leaving_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_floating_object_part_when_leaving_control <- function(dataframe1,
                                                              dataframe2,
                                                              output,
                                                              object_operation = c("8")) {
  # 0 - Global variables assignement ----
  floatingobject_id <- NULL
  floatingobjectpart_whenarriving <- NULL
  floatingobjectpart_whenleaving <- NULL
  count_objet_part_only_leaving <- NULL
  count_objet_part_disappeared_leaving <- NULL
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
    column_name = c("floatingobjectpart_id", "floatingobjectpart_whenarriving", "floatingobjectpart_whenleaving", "floatingobject_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("floatingobjectpart_id", "floatingobjectpart_whenarriving", "floatingobjectpart_whenleaving", "floatingobject_id"),
      column_type = c("character", "character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("floatingobjectpart_id", "floatingobjectpart_whenarriving", "floatingobjectpart_whenleaving", "floatingobject_id")]
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
  select <- dataframe1$floatingobject_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Count the number of objet part only when leaving per floating object
  data_objet_part_only_leaving <- dataframe2 %>%
    dplyr::group_by(floatingobject_id) %>%
    dplyr::filter((is.na(floatingobjectpart_whenarriving) | floatingobjectpart_whenarriving == "false") & (!is.na(floatingobjectpart_whenleaving) & floatingobjectpart_whenleaving != "false")) %>%
    dplyr::summarise(count_objet_part_only_leaving = dplyr::n())
  # Count the number of objet part disappeared when leaving per floating object
  data_objet_part_disappeared_leaving <- dataframe2 %>%
    dplyr::group_by(floatingobject_id) %>%
    dplyr::filter((!is.na(floatingobjectpart_whenarriving) & floatingobjectpart_whenarriving != "false") & (is.na(floatingobjectpart_whenleaving) | floatingobjectpart_whenleaving == "false")) %>%
    dplyr::summarise(count_objet_part_disappeared_leaving = dplyr::n())
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, data_objet_part_only_leaving, by = dplyr::join_by(floatingobject_id))
  dataframe1 <- dplyr::left_join(dataframe1, data_objet_part_disappeared_leaving, by = dplyr::join_by(floatingobject_id))
  # Replace NA count_floating_object by 0
  dataframe1[is.na(dataframe1$count_objet_part_only_leaving), "count_objet_part_only_leaving"] <- 0
  dataframe1[is.na(dataframe1$count_objet_part_disappeared_leaving), "count_objet_part_disappeared_leaving"] <- 0
  # Logical
  dataframe1$logical <- FALSE
  dataframe1[dataframe1$count_objet_part_only_leaving > 0 & dataframe1$count_objet_part_disappeared_leaving == 0, "logical"] <- TRUE
  dataframe1$objet_part_additional_leaving <- FALSE
  dataframe1[dataframe1$count_objet_part_only_leaving > 0, "objet_part_additional_leaving"] <- TRUE
  dataframe1$objet_part_retained_leaving <- FALSE
  dataframe1[dataframe1$count_objet_part_disappeared_leaving == 0, "objet_part_retained_leaving"] <- TRUE
  # If object operation not mandatory objet part when leaving
  dataframe1[!(dataframe1$objectoperation_code %in% object_operation),  "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(count_objet_part_only_leaving, count_objet_part_disappeared_leaving))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " floating objects without adding any elements to the floating object when leaving")))
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
