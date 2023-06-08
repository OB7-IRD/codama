#' @name vector_comparison
#' @title Vector comparison
#' @description Identify similarities and differences between two R vectors.
#' @param first_vector {\link[base]{vector}} expected.
#' @param second_vector {\link[base]{vector}} expected.
#' @param comparison_type {\link[base]{character}} expected. Type of comparison expected, you can choose between "difference", "equal", "less", "greater", "less_equal" or "greater_equal". "Difference" highlight element(s) of the first vector not present in the second, "equal" check if two vectors are exactly the same, "less" check if the elements of the first vector are strictly less than their pair in the second vector , "greater" check if the elements of the first vector are strictly greater than their pair in the second vector, "less_equal" check if the elements of the first vector are less than or equal to their pair in the second vector or "greater_equal" check if the elements of the first vector are greater than or equal to their pair in the second vector.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @examples
#' vector_comparison(
#'   first_vector = c(1, 2, 3, 5),
#'   second_vector = c(1, 2, 4, 3),
#'   comparison_type = "difference",
#'   output = "report"
#' )
#' @export
#' @importFrom dplyr tibble left_join mutate case_when
vector_comparison <- function(first_vector,
                              second_vector,
                              comparison_type,
                              output) {
  # arguments verifications ----
  if (missing(x = first_vector)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, missing \"first_vector\" argument.\n"
    )
  }
  if (missing(x = second_vector)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, missing \"second_vector\" argument.\n"
    )
  }
  if (missing(x = comparison_type)) {
    stop(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " - Error, missing \"comparison_type\" argument.\n"
    )
  }
  if (r_type_checking(
    r_object = comparison_type,
    type = "character",
    length = 1L,
    allowed_value = c(
      "difference",
      "equal"
    ),
    output = "logical"
  ) == TRUE && class(x = first_vector) != class(x = second_vector)) {
    stop(cat(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " Error - invalid data, different classes between vectors\n"
    ))
  }
  if (r_type_checking(
    r_object = comparison_type,
    type = "character",
    length = 1L,
    allowed_value = c(
      "equal",
      "less",
      "greater",
      "less_equal",
      "greater_equal"
    ),
    output = "logical"
  ) == TRUE && length(x = first_vector) != length(x = second_vector)) {
    stop(cat(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " Error - invalid data, different sizes between vectors\n"
    ))
  }
  if (r_type_checking(
    r_object = comparison_type,
    type = "character",
    length = 1L,
    allowed_value = c(
      "difference",
      "equal",
      "less",
      "greater",
      "less_equal",
      "greater_equal"
    ),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = comparison_type,
      type = "character",
      length = 1L,
      allowed_value = c(
        "difference",
        "equal",
        "less",
        "greater",
        "less_equal",
        "greater_equal"
      ),
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = comparison_type,
    type = "character",
    length = 1L,
    allowed_value = c(
      "less",
      "greater",
      "less_equal",
      "greater_equal"
    ),
    output = "logical"
  ) == TRUE) {
    if (class(x = first_vector) != class(x = second_vector) && (!is.numeric(first_vector) || !is.numeric(second_vector))) {
      stop(cat(
        format(
          x = Sys.time(),
          "%Y-%m-%d %H:%M:%S"
        ),
        " Error - invalid data, different classes between vectors and is not either integer or numeric\n"
      ))
    }
    if (r_type_checking(
      r_object = first_vector,
      type = "numeric",
      output = "logical"
    ) != TRUE && r_type_checking(
      r_object = first_vector,
      type = "integer",
      output = "logical"
    ) != TRUE && inherits(first_vector, "Date") != TRUE) {
      stop(cat(
        format(
          x = Sys.time(),
          "%Y-%m-%d %H:%M:%S"
        ),
        " Error - invalid class for \"first_vector\" and \"second_vector\", the accepted classes are: numeric, integer or Date\n"
      ))
    }
  }
  if (r_type_checking(
    r_object = output,
    type = "character",
    length = 1L,
    allowed_value = c(
      "message",
      "report",
      "logical"
    ),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      length = 1L,
      allowed_value = c(
        "message",
        "report",
        "logical"
      ),
      output = "message"
    ))
  }
  # global process ----
  data <- dplyr::tibble("first_vector" = first_vector)
  if (comparison_type == "difference") {
    data_final <- dplyr::left_join(
      x = data,
      y = (dplyr::tibble("first_vector" = setdiff(
        x = first_vector,
        y = second_vector
      )) %>%
        dplyr::mutate(logical = FALSE)),
      by = "first_vector"
    ) %>%
      dplyr::mutate(logical = dplyr::case_when(
        is.na(logical) ~ TRUE,
        TRUE ~ logical
      ))
    message_success <- "- Success, all elements of the first vector are present in the second sector.\n"
    message_failure_singular <- "element of the first vector is not present in the second sector.\n"
    message_failure_plural <- "elements of the first vector are not present in the second sector.\n"
  } else if (comparison_type == "equal") {
    data_final <- dplyr::mutate(
      .data = data,
      second_vector = second_vector,
      logical = dplyr::case_when(
        first_vector == second_vector ~ TRUE,
        TRUE ~ FALSE
      )
    )
    message_success <- "- Success, the two vectors are identical.\n"
    message_failure_singular <- "element of the first vector is not equal than their pair in the second sector.\n"
    message_failure_plural <- "elements of the first vector are not equal than their pair in the second sector.\n"
  } else if (comparison_type == "less") {
    data_final <- dplyr::mutate(
      .data = data,
      second_vector = second_vector,
      logical = dplyr::case_when(
        first_vector < second_vector ~ TRUE,
        TRUE ~ FALSE
      )
    )
    message_success <- "- Success, all elements of the first vector are less than their pair in the second sector.\n"
    message_failure_singular <- "element of the first vector is not less than their pair in the second sector.\n"
    message_failure_plural <- "elements of the first vector are not less than their pair in the second sector.\n"
  } else if (comparison_type == "greater") {
    data_final <- dplyr::mutate(
      .data = data,
      second_vector = second_vector,
      logical = dplyr::case_when(
        first_vector > second_vector ~ TRUE,
        TRUE ~ FALSE
      )
    )
    message_success <- "- Success, all elements of the first vector are greater than their pair in the second sector.\n"
    message_failure_singular <- "element of the first vector is not greater than their pair in the second sector.\n"
    message_failure_plural <- "elements of the first vector are not greater than their pair in the second sector.\n"
  } else if (comparison_type == "less_equal") {
    data_final <- dplyr::mutate(
      .data = data,
      second_vector = second_vector,
      logical = dplyr::case_when(
        first_vector <= second_vector ~ TRUE,
        TRUE ~ FALSE
      )
    )
    message_success <- "- Success, all elements of the first vector are less than or equal to their pair in the second sector.\n"
    message_failure_singular <- "element of the first vector is not less than or equal to their pair in the second sector.\n"
    message_failure_plural <- "elements of the first vector are not less than or equal to their pair in the second sector.\n"
  } else if (comparison_type == "greater_equal") {
    data_final <- dplyr::mutate(
      .data = data,
      second_vector = second_vector,
      logical = dplyr::case_when(
        first_vector >= second_vector ~ TRUE,
        TRUE ~ FALSE
      )
    )
    message_success <- "- Success, all elements of the first vector are greater than or equal to their pair in the second sector.\n"
    message_failure_singular <- "element of the first vector is not greater than or equal to their pair in the second sector.\n"
    message_failure_plural <- "elements of the first vector are not greater than or equal to their pair in the second sector.\n"
  }

  # Output if the comparison is respected
  if (sum(data_final$logical) == nrow(data_final)) {
    if (output == "message") {
      return(cat(
        format(
          x = Sys.time(),
          "%Y-%m-%d %H:%M:%S"
        ),
        message_success
      ))
    } else if (output == "logical") {
      return(TRUE)
    }
  } else {
    # Output if the comparison is not respected
    if (output == "message") {
      mismatch_element <- sum(!data_final$logical)
      return(cat(
        format(
          x = Sys.time(),
          "%Y-%m-%d %H:%M:%S"
        ),
        "- Failure,",
        mismatch_element,
        ifelse(test = mismatch_element == 1,
               yes = message_failure_singular,
               no = message_failure_plural
        )
      ))
    } else if (output == "logical") {
      return(FALSE)
    }
  }
  if (output == "report") {
    return(data_final)
  }
}
