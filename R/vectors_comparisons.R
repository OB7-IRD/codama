#' @name vectors_comparisons
#' @title Vectors comparisons
#' @description Identify similarities and differences between two R vectors.
#' @param first_vector {\link[base]{vector}} expected.
#' @param second_vector {\link[base]{vector}} expected.
#' @param comparison_type {\link[base]{character}} expected. Type of comparison expected, you can choose between "difference" or "equality". "Difference" highlight element(s) of the first vector not present in the second, and "equality" check if two vectors are exactly the same.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @examples
#' vectors_comparisons(first_vector = c(1, 2, 3, 5),
#'                     second_vector = c(1, 2, 4, 3),
#'                     comparison_type = "difference",
#'                     output = "report")
#' @export
#' @importFrom dplyr tibble left_join mutate case_when
vectors_comparisons <- function(first_vector,
                                second_vector,
                                comparison_type,
                                output) {
  # arguments verifications ----
  if (missing(x = first_vector)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, missing \"first_vector\" argument.\n")
  }
  if (missing(x = second_vector)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, missing \"second_vector\" argument.\n")
  }
  if (missing(x = comparison_type)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, missing \"comparison_type\" argument.\n")
  }
  if (class(x = first_vector) != class(x = second_vector)) {
    stop(cat(format(x = Sys.time(),
                    "%Y-%m-%d %H:%M:%S"),
             " Error - invalid data, different classes between vectors\n"))
  }
  if (r_type_checking(r_object = comparison_type,
                      type = "character",
                      length = 1L,
                      allowed_values = c("difference",
                                         "equality"),
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = comparison_type,
                           type = "character",
                           length = 1L,
                           allowed_values = c("difference",
                                              "equality"),
                           output = "message"))
  }
  if (r_type_checking(r_object = output,
                      type = "character",
                      length = 1L,
                      allowed_values = c("message",
                                         "report",
                                         "logical"),
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = output,
                           type = "character",
                           length = 1L,
                           allowed_values = c("message",
                                              "report",
                                              "logical"),
                           output = "message"))
  }
  # global process ----
  data <- dplyr::tibble("first_vector" = first_vector)
  if (comparison_type == "difference") {
    if (length(x = setdiff(x = first_vector,
                           y = second_vector)) != 0) {
      if (output == "message") {
        mismatch_element <- length(x = setdiff(x = first_vector,
                                               y = second_vector))
        return(cat(format(x = Sys.time(),
                          "%Y-%m-%d %H:%M:%S"),
                   "- Failure,",
                   mismatch_element,
                   ifelse(test = mismatch_element == 1,
                          yes = "element of the first vector is not present in the second sector.\n",
                          no = "elements of the first vector are not present in the second sector.\n")))
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
        return(cat(format(x = Sys.time(),
                          "%Y-%m-%d %H:%M:%S"),
                   "- Success, all elements of the first vector are present in the second sector.\n"))
      } else if (output == "logical") {
        return(TRUE)
      } else if (output == "report") {
        data_final <- dplyr::mutate(.data = data,
                                    vectors_comparisons_detail = "no difference")
        return(data_final)
      }
    }
  } else if (comparison_type == "equality") {
    if (length(x = first_vector) != length(x = second_vector)) {

    }
    if (identical(x = first_vector,
                  y = second_vector)) {
      if (output == "message") {
        return(cat(format(x = Sys.time(),
                          "%Y-%m-%d %H:%M:%S"),
                   "- Success, the two vectors are identical.\n"))
      } else if (output == "logical") {
        return(TRUE)
      } else if (output == "report") {
        data_final <- dplyr::mutate(.data = data,
                                    second_vector = second_vector,
                                    vectors_comparisons_detail = "equality")
        return(data_final)
      }
    } else {
      if (output == "message") {
        return(cat(format(x = Sys.time(),
                          "%Y-%m-%d %H:%M:%S"),
                   "- Failure, the two vectors are not identical.\n"))
      } else if (output == "logical") {
        return(FALSE)
      } else if (output == "report") {
        browser()
        data_final <- dplyr::full_join(x = )

        data_final <- dplyr::mutate(.data = data,
                                    second_vector = second_vector,
                                    vectors_comparisons_detail = dplyr::case_when(
                                      first_vector == second_vector ~ "equality",
                                      TRUE ~ "no equality"))
        return(data_final)
      }
    }
  }
}
