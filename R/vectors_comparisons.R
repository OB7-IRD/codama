#' @name vectors_comparisons
#' @title Vectors comparisons
#' @description Identify similarities and differences between two R vectors
#' @param first_vector {\link[base]{vector}} expected.
#' @param second_vector {\link[base]{vector}} expected.
#' @param comparison_type {\link[base]{character}} expected. Type of comparison expected, you can choose between "difference" or "equality". "Difference" highlight element(s) of the first vector not present in the second, and "equality" check if two vectors are exactly the same.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @export
#' @importFrom dplyr tibble left_join mutate case_when
vectors_comparisons <- function(first_vector,
                                second_vector,
                                comparison_type,
                                output) {
  # arguments verifications ----
  if (missing(first_vector)) {
    cat(format(x = Sys.time(),
               "%Y-%m-%d %H:%M:%S"),
        "- invalid \"first_vector\" argument\n")
    stop()
  }
  if (missing(second_vector)) {
    cat(format(x = Sys.time(),
               "%Y-%m-%d %H:%M:%S"),
        "- invalid \"second_vector\" argument\n")
    stop()
  }
  if (class(first_vector) != class(second_vector)) {
    cat(format(x = Sys.time(),
               "%Y-%m-%d %H:%M:%S"),
        "- invalid data, different classes between vectors\n")
    stop()
  }
  if (missing(comparison_type)
      || class(comparison_type) != "character"
      || length(comparison_type) != 1
      || (! comparison_type %in% c("difference",
                                   "equality"))) {
    cat(format(x = Sys.time(),
               "%Y-%m-%d %H:%M:%S"),
        "- invalid \"comparison_type\" argument\n")
    stop()
  }
  if (missing(output)
      || class(output) != "character"
      || length(output) != 1
      || (! output %in% c("message",
                                   "report",
                                   "logical"))) {
    cat(format(x = Sys.time(),
               "%Y-%m-%d %H:%M:%S"),
        "- invalid \"output\" argument")
    stop()
  }
  # processes ----
  data <- dplyr::tibble("first_vector" = first_vector)
  if (comparison_type == "difference") {
    if (length(x = setdiff(x = first_vector,
                           y = second_vector)) != 0) {
      if (output == "message") {
        mismatch_element <- length(x = setdiff(x = first_vector,
                                               y = second_vector))
        cat(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            "- Failure,",
            mismatch_element,
            ifelse(test = mismatch_element == 1,
                   yes = "element of the first vector is not present in the second sector.\n",
                   no = "elements of the first vector are not present in the second sector.\n"))
      } else if (output == "logical") {
        return(FALSE)
      } else if (output == "report") {
        data_final <- dplyr::left_join(x = data,
                                       y = (dplyr::tibble("first_vector" = setdiff(x = first_vector,
                                                                                   y = second_vector)) %>%
                                              dplyr::mutate(vectors_comparisons_output = "data not present in the second sector")),
                                       by = "first_vector") %>%
          dplyr::mutate(vectors_comparisons_output = dplyr::case_when(
            is.na(vectors_comparisons_output) ~ "no difference",
            TRUE ~ vectors_comparisons_output
          ))
        return(data_final)
      }
    } else {
      if (output == "message") {
        cat(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            "- Success, all elements of the first vector are present in the second sector.\n")
      } else if (output == "logical") {
        return(TRUE)
      } else if (output == "report") {
        data_final <- dplyr::mutate(.data = data,
                                    vectors_comparisons_detail = "no difference")
        return(data_final)
      }
    }
  } else if (comparison_type == "equality") {
    if (identical(x = first_vector,
                  y = second_vector)) {
      if (output == "message") {
        cat(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            "- Success, the two vectors are identical.\n")
      } else if (output == "logical") {
        return(TRUE)
      } else if (output == "report") {
        data_final <- dplyr::mutate(.data = data,
                                    vectors_comparisons_detail = "equality")
        return(data_final)
      }
    } else {
      if (output == "message") {
        cat(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            "- Failure, the two vectors are not identical.\n")
      } else if (output == "logical") {
        return(FALSE)
      } else if (output == "report") {
        data_final <- dplyr::mutate(.data = data,
                                    vectors_comparisons_detail = "no equality")
        return(data_final)
      }
    }
  }
}
