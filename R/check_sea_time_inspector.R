#' @name check_sea_time_inspector
#' @title Gives the inconsistencies between the sum of the sea times indicated for the route and the one indicated for the trip
#' @description The purpose of the check_sea_time_inspector function is to provide a table of data that contains an inconsistency between the sum of the sea times indicated for the route and the one indicated for the trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sea_time_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_sea_time_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_seatime}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  route_id}}
#'  \item{\code{  trip_id}}
#'  \item{\code{  route_seatime}}
#' }
#' @doctest
#' #Trip 1 is ok,
#' #Trip 2 has different sea times,
#' #Trip 3 has no trip_seatime,
#' #Trip 4 has no route_seatime,
#' #Trip 5 has trip_seatime = 0
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5"),
#'                          trip_seatime = c(10, 15, NA, 8, 0))
#' dataframe2 <- data.frame(route_id = c("1", "2", "3", "4", "5", "6"),
#'                          trip_id = c("1", "1", "2", "2", "3", "5"),
#'                          route_seatime = c(4, 6, 10, 6, 5, 0))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, FALSE, FALSE, FALSE, FALSE), trip_seatime = c(10, 15, NA, 8, 0), sum_route_seatime = c(10, 16, 5, NA, 0)), row.names = c(NA, -5L), class = "data.frame"))
#' check_sea_time_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_sea_time_inspector <- function(dataframe1,
                                     dataframe2,
                                     output) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  route_seatime <- NULL
  trip_seatime <- NULL
  sum_route_seatime <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_seatime"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_seatime"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_seatime")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("route_id", "trip_id", "route_seatime"),
    column_type = c("character", "character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("route_id", "trip_id", "route_seatime"),
      column_type = c("character", "character", "numeric"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("route_id", "trip_id", "route_seatime")]
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
  # Calculate the sum of the sea time per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_route_seatime = ifelse(all(is.na(route_seatime)), route_seatime[NA_integer_], sum(route_seatime, na.rm = TRUE)))
  # Group the pair to compare
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  # Compare trip IDs and sea time of the trip or the sum of the route
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$trip_seatime,
    second_vector = dataframe1$sum_route_seatime,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, trip_seatime, sum_route_seatime, .after = logical)
  # Management of the 0 value for the time at sea
  dataframe1[!is.na(dataframe1$trip_seatime) & dataframe1$trip_seatime == 0, "logical"] <- FALSE
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " trip with a sea time different from the sum of the sea times of each activity")))
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
