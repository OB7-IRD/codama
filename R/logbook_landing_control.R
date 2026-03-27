#' @name logbook_landing_control
#' @title Gives the inconsistencies between the total landed weight for canneries and local market in the trip and vessel capacity link to trip
#' @description The purpose of the logbook_landing_control function is to provide a table of data that contains an inconsistency with the total landed weight greater than vessel capacity for the trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_landing_control () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_landingtotalweight}}
#'  \item{\code{  trip_localmarkettotalweight}}
#'  \item{\code{  vessel_capacity}}
#' }
#' @doctest
#' #Trip 1 and 4 are ok,
#' #Trip 2 has a total weight greater than the vessel's capacity,
#' #Trip 3 has its vessel's capacity missing
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4"),
#'                          trip_landingtotalweight = c(10, 15, 2, NA),
#'                          trip_localmarkettotalweight = c(2, 1, 7, NA),
#'                          vessel_capacity = c(20, 10, NA, 13))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4"), logical = c(TRUE, FALSE, FALSE, TRUE), vessel_capacity = c(14, 7, NA, 9.1), trip_weighttotal = c(12, 16, 9, 0)), row.names = c(NA, 4L), class = "data.frame"))
#' logbook_landing_control(dataframe1, output = "report")
#' @export
logbook_landing_control <- function(dataframe1,
                                    output) {
  # 0 - Global variables assignement ----
  vessel_capacity <- NULL
  trip_weighttotal <- NULL
  trip_landingtotalweight <- NULL
  trip_localmarkettotalweight <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_landingtotalweight", "trip_localmarkettotalweight", "vessel_capacity"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_landingtotalweight", "trip_localmarkettotalweight", "vessel_capacity"),
      column_type = c("character", "numeric", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_landingtotalweight", "trip_localmarkettotalweight", "vessel_capacity")]
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
  # Calculate the landing total weight per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates 0)
  dataframe1$trip_weighttotal <- rowSums(x = dataframe1[, c("trip_landingtotalweight", "trip_localmarkettotalweight")], na.rm = TRUE)
  # Converts cubic meters to tons
  dataframe1$vessel_capacity <- dataframe1$vessel_capacity * 0.7
  # Compare landing total weight of the trip with vessel capacity
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$trip_weighttotal,
    second_vector = dataframe1$vessel_capacity,
    comparison_type = "less",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  dataframe1 <- dplyr::relocate(.data = dataframe1, vessel_capacity, trip_weighttotal, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(trip_landingtotalweight, trip_localmarkettotalweight))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " trips with vessel capacity smaller than the landing total weight")))
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
