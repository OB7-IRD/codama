#' @name logbook_floating_object_part_material_control
#' @title Identification of incoherent floating object material
#' @description The purpose of the logbook_floating_object_part_material_control function is to provide a table of data that contains a incoherent floating object material. Indeed, certain floating objects are not used in check fishing.
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_part_material_control.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_floating_object_part_material_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param object_material {\link[base]{character}} expected. Default values: c("1-2"). Vector containing the codes for materials that are not traditionally used on a floating object.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  floatingobject_id}}
#' Dataframe 2:
#'  \item{\code{  floatingobjectpart_id}}
#'  \item{\code{  objectmaterial_code}}
#'  \item{\code{  floatingobject_id}}
#' }
#' @doctest
#' #Floating object 1 is ok,
#' #Floating object 2 has object material not used in the fishing.
#' dataframe1 <- data.frame(floatingobject_id = c("1", "2"))
#' dataframe2 <- data.frame(floatingobjectpart_id = c("1", "2"),
#'                          objectmaterial_code = c("1-1", "1-2"),
#'                          floatingobject_id = c("1", "2"))
#' @expect equal (.,structure(list(floatingobject_id = c("1", "2"), logical = c(TRUE, FALSE)), row.names = c(NA, -2L), class = "data.frame"))
#' logbook_floating_object_part_material_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_floating_object_part_material_control <- function(dataframe1,
                                                          dataframe2,
                                                          output,
                                                          object_material = c("1-2")) {
  # 0 - Global variables assignement ----
  floatingobject_id <- NULL
  objectmaterial_code <- NULL
  count_material_object <- NULL
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
    column_name = c("floatingobjectpart_id", "objectmaterial_code", "floatingobject_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("floatingobjectpart_id", "objectmaterial_code", "floatingobject_id"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe2 <- dataframe2[, c("floatingobjectpart_id", "objectmaterial_code", "floatingobject_id")]
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
    r_object = object_material,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = object_material,
      type = "character",
      output = "message"
    ))
  }
  select <- dataframe1$floatingobject_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Count the number of material objet not traditionally used per floating object
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(floatingobject_id) %>%
    dplyr::filter(objectmaterial_code %in% object_material) %>%
    dplyr::summarise(count_material_object = dplyr::n())
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(floatingobject_id))
  # Replace NA count_floating_object by 0
  dataframe1[is.na(dataframe1$count_material_object), "count_material_object"] <- 0
  # Logical
  dataframe1$logical <- FALSE
  dataframe1[dataframe1$count_material_object == 0, "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(count_material_object))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " floating objects with materials not traditionally used")))
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
