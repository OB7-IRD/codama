#' @name logbook_buoy_code_control
#' @title Identification of an inconsistent buoy code and buoy type naming convention
#' @description The purpose of the logbook_buoy_code_control function is to provide a table of data that contains a incoherent buoy code and buoy type naming convention
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_buoy_code_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  transmittingbuoy_id}}
#'  \item{\code{  transmittingbuoy_code}}
#'  \item{\code{  transmittingbuoytype_regex}}
#' }
#' @doctest
#' #Buoy 1 is ok,
#' #Buoy 2 has start by 0
#' #Buoy 3 has a 7 digit identifier.
#' dataframe1 <- data.frame(transmittingbuoy_id = c("1", "2", "3", "4"),
#'                          transmittingbuoy_code = c("111111", "011111", "1234567", NA),
#'                          transmittingbuoytype_regex = c("[0-9]{6}", "[0-9]{6}", "[0-9]{6}",
#'                                                         "[0-9]{6}"))
#' @expect equal (.,structure(list(transmittingbuoy_id = c("1", "2", "3", "4"), transmittingbuoy_code = c("111111", "011111", "1234567", NA), logical = c(TRUE, FALSE, FALSE, FALSE), transmittingbuoytype_regex = c("[0-9]{6}", "[0-9]{6}", "[0-9]{6}", "[0-9]{6}")), row.names = c(NA, 4L), class = "data.frame"))
#' logbook_buoy_code_control(dataframe1, output = "report")
#' @export
logbook_buoy_code_control <- function(dataframe1,
                                      output) {
  # 0 - Global variables assignement ----
  transmittingbuoy_code <- NULL
  transmittingbuoytype_regex <- NULL
  logical_start_0 <- NULL
  logical_regex <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("transmittingbuoy_id", "transmittingbuoy_code", "transmittingbuoytype_regex"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("transmittingbuoy_id", "transmittingbuoy_code", "transmittingbuoytype_regex"),
      column_type = c("character", "character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("transmittingbuoy_id", "transmittingbuoy_code", "transmittingbuoytype_regex")]
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
  select <- dataframe1$transmittingbuoy_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Logical
  dataframe1$logical <- FALSE
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(logical_regex = stringr::str_detect(transmittingbuoy_code, paste0("^", transmittingbuoytype_regex, "$")), logical_start_0 = stringr::str_detect(transmittingbuoy_code, "^0", negate = TRUE))
  dataframe1[!is.na(dataframe1$transmittingbuoy_code) & dataframe1$logical_regex & dataframe1$logical_start_0, "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, transmittingbuoytype_regex, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(logical_start_0, logical_regex))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || sum(is.na(dataframe1$logical)) > 0) {
    all <- c(select, dataframe1$transmittingbuoy_id)
    number_occurrences <- table(all)
    text <- ""
    if (sum(number_occurrences == 1) > 0) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (sum(number_occurrences > 2) > 0) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " buoy with a name that does not comply with the convention for its type")))
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
