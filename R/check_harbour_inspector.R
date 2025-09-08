#' @name check_harbour_inspector
#' @title Gives the inconsistencies between the harbour of landing of the previous trip and the harbour of departure of the current trip
#' @description The purpose of the check_harbour_inspector function is to provide a table of data that contains an inconsistency with the harbour of landing of the previous trip and the harbour of departure of the current trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_harbour_inspector () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  harbour_id_departure}}
#'  \item{\code{  harbour_label_departure}}
#'  \item{\code{  trip_previous_id}}
#'  \item{\code{  harbour_id_landing_trip_previous}}
#'  \item{\code{  harbour_label_landing_trip_previous}}
#' }
#' @doctest
#' #Trip 1 and 2 are ok,
#' #Trip 3 has different harbour,
#' #Trip 4 has no harbour departure,
#' #Trip 5 has no harbour landing for trip previous
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5"),
#'                          harbour_id_departure = c("1", "2", "3", NA, "1"),
#'                          harbour_label_departure = c("Harbour_1", "Harbour_2", "Harbour_3", NA,
#'                                                      "Harbour_1"),
#'                          trip_previous_id = c(NA, "1", "2", NA, "6"),
#'                          harbour_id_landing_trip_previous = c(NA, "2", "2", NA, NA),
#'                          harbour_label_landing_trip_previous = c(NA, "Harbour_2", "Harbour_2", NA,
#'                                                                  NA))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE), harbour_label_landing_trip_previous = c(NA, "Harbour_2", "Harbour_2", NA, NA), harbour_label_departure = c("Harbour_1", "Harbour_2", "Harbour_3", NA, "Harbour_1")), row.names = c(NA, 5L), class = "data.frame"))
#' check_harbour_inspector(dataframe1, output = "report")
#' @export
check_harbour_inspector <- function(dataframe1,
                                    output) {
  # 0 - Global variables assignement ----
  harbour_label_departure <- NULL
  harbour_label_landing_trip_previous <- NULL
  harbour_id_departure <- NULL
  harbour_id_landing_trip_previous <- NULL
  trip_previous_id <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "harbour_id_departure", "harbour_label_departure", "trip_previous_id", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous"),
    column_type = c("character", "character", "character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "harbour_id_departure", "harbour_label_departure", "trip_previous_id", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous"),
      column_type = c("character", "character", "character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "harbour_id_departure", "harbour_label_departure", "trip_previous_id", "harbour_id_landing_trip_previous", "harbour_label_landing_trip_previous")]
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
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Compare landing total weight of the trip with vessel capacity
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$harbour_id_landing_trip_previous,
    second_vector = dataframe1$harbour_id_departure,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Management of missing harbour landing in trip previous
  dataframe1[is.na(dataframe1$harbour_id_landing_trip_previous), "logical"] <- FALSE
  # Management of not trip previous
  dataframe1[is.na(dataframe1$trip_previous_id), "logical"] <- TRUE
  # Management of missing harbour departure
  dataframe1[is.na(dataframe1$harbour_id_departure), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, harbour_label_landing_trip_previous, harbour_label_departure, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(harbour_id_departure, harbour_id_landing_trip_previous, trip_previous_id))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$trip_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$trip_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " trips with departure port different from the landing harbour of a previous trip")))
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
