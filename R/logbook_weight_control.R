#' @name logbook_weight_control
#' @title Gives the inconsistencies between the sum of the weight indicated for catches and the one indicated for the activity
#' @description The purpose of the logbook_weight_control function is to provide a table of data that contains an inconsistency between the sum of the weight indicated for the catch and the one indicated for the activity
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weight_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weight_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param epsilon {\link[base]{numeric}} expected, default : 0.01. Gives the threshold at which the difference is considered too large.
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_weight}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  catch_id}}
#'  \item{\code{  catch_weight}}
#'  \item{\code{  activity_id}}
#' }
#' @doctest
#' #Activity 1, 2 and 4 are ok,
#' #Activity 3 has different weight
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4"),
#'                          activity_weight = c(10, 15.01, 6, NA))
#' dataframe2 <- data.frame(catch_id = c("1", "2", "3", "4", "5"),
#'                          catch_weight = c(10, 10, 5, 3, 2),
#'                          activity_id = c("1", "2", "2", "3", "3"))
#' @expect equal(., structure(list(activity_id = c("1", "2", "3", "4"), logical = c(TRUE, TRUE, FALSE, TRUE), activity_weight = c(10, 15.01, 6, NA), sum_catch_weight = c(10, 15, 5, NA)), row.names = c(NA, 4L), class = "data.frame"))
#' logbook_weight_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_weight_control <- function(dataframe1,
                                   dataframe2,
                                   output,
                                   epsilon = 0.01) {
  # 0 - Global variables assignement ----
  activity_id <- NULL
  catch_weight <- NULL
  activity_weight <- NULL
  sum_catch_weight <- NULL
  difference <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_weight"),
    column_type = c("character", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_weight"),
      column_type = c("character", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "activity_weight")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("catch_id", "catch_weight", "activity_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("catch_id", "catch_weight", "activity_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("catch_id", "catch_weight", "activity_id")]
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
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Calculate the sum of the weight per activity (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(sum_catch_weight = ifelse(all(is.na(catch_weight)), catch_weight[NA_integer_], sum(catch_weight, na.rm = TRUE)))
  # Merge and calcul difference
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id))
  dataframe1$difference <- ifelse(is.na(dataframe1$activity_weight), 0, dataframe1$activity_weight) - ifelse(is.na(dataframe1$sum_catch_weight), 0, dataframe1$sum_catch_weight)
  dataframe1$difference <- abs(dataframe1$difference)
  if (nrow(dataframe1) > 0) {
    dataframe1$epsilon <- epsilon
  } else {
    dataframe1$epsilon <- numeric()
  }
  # Compare weight of the activity or the sum of the catch
  # Compare sum difference with epsilon
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$difference,
    second_vector = dataframe1$epsilon,
    comparison_type = "less_equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  # Management of the NA value for the weight activity and catch
  dataframe1[is.na(dataframe1$activity_weight) & is.na(dataframe1$sum_catch_weight), "logical"] <- TRUE
  # Management of the 0 value for the weight activity
  dataframe1[!is.na(dataframe1$activity_weight) & dataframe1$activity_weight == 0 & is.na(dataframe1$sum_catch_weight), "logical"] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, activity_weight, sum_catch_weight, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(difference, epsilon))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with a weight different from the sum of the weight of each catch")))
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
