#' @name check_specimen_lwr
#' @title Fork length checking in data specimen
#' @param data Object of type \code{\link[base]{data.frame}} expected.
#' @param threshold Object of type \code{\link[base]{numeric}} expected.
#' @export
check_specimen_lwr <- function(data,
                               threshold = 5) {
  if (missing(data)
      || ! is.data.frame(data)
      || nrow(data) == 0
      || ncol(data) != 4
      || names(data) != c("fish_identifier",
                          "species_code_fao",
                          "fork_length",
                          "whole_fish_weight")) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        "- Error: invalid \"data.frame\" argument\n",
        "An object of class data.frame with 4 columns (named in order \"fish_identifier\", \"species_code_fao\", \"fork_length\", \"whole_fish_weight\") and, at least, one row.\n",
        sep = "")
  } else if (class(threshold) != "numeric"
             || length(threshold) != 1) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        "- Error: invalid \"threshold\" argument\n",
        "One value of class numeric expected.\n",
        sep = "")
  } else {
    data_final <- data
    for (specie in unique(data$species_code_fao)) {
      data_specie <- dplyr::filter(.data = data,
                                   species_code_fao == specie)
      if (specie == "YFT") {
        data_specie_lwr <- data_specie %>%
          dplyr::mutate(lwr = (2.153 * 10 ^ -5) * (fork_length ^ 2.976),
                        lwr_threshold = lwr * threshold / 100,
                        lwr_min = lwr - lwr_threshold,
                        lwr_max = lwr + lwr_threshold,
                        output_check_specimen_lwr = dplyr::case_when(
                          lwr >= lwr_min & lwr <= lwr_max ~ "passed",
                          lwr < lwr_min & lwr > lwr_max ~ "failed",
                          TRUE ~ "not_run"
                        ))
        data_final <- dplyr::left_join(x = data_final,
                                       y = data_specie_lwr[,c("fish_identifier", "output_check_specimen_lwr")],
                                       by = "fish_identifier")
      } else {
        cat(format(x = Sys.time(),
                   format = "%Y-%m-%d %H:%M:%S"),
            "- Warning: no lwr available for the specie ",
            specie,
            ", switch to next one.\n",
            sep = "")
      }
    }
    return(data_final)
  }
}
