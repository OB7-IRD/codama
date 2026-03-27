#' @name logbook_landing_total_weight_control
#' @title Gives the inconsistencies between the total weight landed during the trip for the canneries and the sum of the weights of each landing for the canneries linked to the trip
#' @description The purpose of the logbook_landing_total_weight_control function is to provide a table of data that contains an inconsistency between the sum of the weights of each landing for the canneries and the one indicated for the trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_landing_total_weight_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_landing_total_weight_control () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @param epsilon {\link[base]{numeric}} expected, default : 0.01. Gives the threshold at which the difference is considered too large.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  trip_id}}
#'  \item{\code{  trip_landingtotalweight}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  landing_id}}
#'  \item{\code{  landing_weight}}
#'  \item{\code{  trip_id}}
#' }
#' @doctest
#' #Trip 1 and 4 are ok,
#' #Trip 2 has different landing weight,
#' #Trip 3 has no trip_landingtotalweight,
#' #Trip 5 has no landing_weight
#' dataframe1 <- data.frame(trip_id = c("1", "2", "3", "4", "5"),
#'                          trip_landingtotalweight = c(10, 15, NA, 0, 4))
#' dataframe2 <- data.frame(landing_id = c("1", "2", "3", "4", "5", "6"),
#'                          landing_weight = c(4, 6, 10, 6, 2, NA),
#'                          trip_id = c("1", "1", "2", "2", "3", "4"))
#' @expect equal(., structure(list(trip_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, FALSE, FALSE, TRUE, FALSE), trip_landingtotalweight = c(10, 15, NA, 0, 4), sum_weightlanding = c(10, 16, 2, NA, NA)), row.names = c(NA, 5L), class = "data.frame"))
#' logbook_landing_total_weight_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_landing_total_weight_control <- function(dataframe1,
                                                 dataframe2,
                                                 output,
                                                 epsilon = 0.01) {
  # 0 - Global variables assignement ----
  trip_id <- NULL
  landing_weight <- NULL
  trip_landingtotalweight <- NULL
  sum_weightlanding <- NULL
  difference <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("trip_id", "trip_landingtotalweight"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("trip_id", "trip_landingtotalweight"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("trip_id", "trip_landingtotalweight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("landing_id", "landing_weight", "trip_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("landing_id", "landing_weight", "trip_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("landing_id", "landing_weight", "trip_id")]
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
  # Checks the type of epsilon
  if (!codama::r_type_checking(
    r_object = epsilon,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = epsilon,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$trip_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculate the landing total weight per trip (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(trip_id) %>%
    dplyr::summarise(sum_weightlanding = ifelse(all(is.na(landing_weight)), landing_weight[NA_integer_], sum(landing_weight, na.rm = TRUE)))
  # Merge and calcul difference
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  dataframe1$difference <- ifelse(is.na(dataframe1$trip_landingtotalweight), 0, dataframe1$trip_landingtotalweight) - ifelse(is.na(dataframe1$sum_weightlanding), 0, dataframe1$sum_weightlanding)
  dataframe1$difference <- abs(dataframe1$difference)
  if (nrow(dataframe1) > 0) {
    dataframe1$epsilon <- epsilon
  } else {
    dataframe1$epsilon <- numeric()
  }
  # Compare sum difference with epsilon
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$difference,
    second_vector = dataframe1$epsilon,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  dataframe1 <- dplyr::relocate(.data = dataframe1, trip_landingtotalweight, sum_weightlanding, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(difference, epsilon))
  # Management of missing landing weight in trip
  dataframe1[is.na(dataframe1$trip_landingtotalweight), "logical"] <- FALSE
  # Management of missing sum of the landing
  dataframe1[is.na(dataframe1$sum_weightlanding) & !is.na(dataframe1$trip_landingtotalweight) & dataframe1$trip_landingtotalweight > 0, "logical"] <- FALSE
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " trips with a landing weight for the canneries different from the sum of the weights of each landing for the canneries")))
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
