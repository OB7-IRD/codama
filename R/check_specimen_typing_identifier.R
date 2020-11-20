#' @name check_specimen_typing_identifier
#' @title Fish identifier checking in data specimen
#' @param data Object of type \code{\link[base]{data.frame}} expected.
#' @export
check_specimen_typing_identifier <- function(data) {
  if (missing(data)
      || ! is.data.frame(data)
      || nrow(data) == 0
      || ncol(data) != 1
      || names(data) != c("fish_identifier")) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        "- Error: invalid \"data.frame\" argument\n",
        "An object of class \"data.frame\" with 1 column (named \"fish_identifier\") and, at least, one row.\n",
        sep = "")
  } else {
    data_tmp <- data %>%
      dplyr::group_by(fish_identifier) %>%
      dplyr::summarise(count_fish_identifier = dplyr::n(),
                       .groups = "drop") %>%
      dplyr::mutate(output_check_specimen_typing_identifier = dplyr::case_when(
        count_fish_identifier != 1 ~ "failed",
        TRUE ~ "passed"
      ))
    data_final <- data %>%
      dplyr::left_join(data_tmp[, c("fish_identifier",
                                    "output_check_specimen_typing_identifier")])
    return(data_final)
  }
}
