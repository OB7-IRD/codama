#' @name logbook_buoy_owner_control
#' @title Identification of an inconsistent buoy whose owner is missing
#' @description The purpose of the logbook_buoy_owner_control function is to provide a table of data that contains a incoherent buoy whose owner is missing
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_buoy_owner_control.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  transmittingbuoy_id}}
#'  \item{\code{  transmittingbuoyownership_id}}
#' }
#' @doctest
#' #Buoy 1 is ok,
#' #Buoy 2 has owner missing
#' dataframe1 <- data.frame(transmittingbuoy_id = c("1", "2"),
#'                          transmittingbuoyownership_id = c("1", NA))
#' @expect equal (.,structure(list(transmittingbuoy_id = c("1", "2"), logical = c(TRUE, FALSE)), row.names = 1:2, class = "data.frame"))
#' logbook_buoy_owner_control(dataframe1, output = "report")
#' @export
logbook_buoy_owner_control <- function(dataframe1,
                                       output) {
  # 0 - Global variables assignement ----
  transmittingbuoyownership_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("transmittingbuoy_id", "transmittingbuoyownership_id"),
    column_type = c("character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("transmittingbuoy_id", "transmittingbuoyownership_id"),
      column_type = c("character", "character"),
      output = "message"
    )
  } else {
    dataframe1 <- dataframe1[, c("transmittingbuoy_id", "transmittingbuoyownership_id")]
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
  dataframe1[!is.na(dataframe1$transmittingbuoyownership_id), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(transmittingbuoyownership_id))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " buoy with missing owner")))
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
