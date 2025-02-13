#' @name fate_unknown_control
#' @title Fate unknown control
#' @author Chloé Tellier, Esther Mollier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the observer data cases where fate code 9 (other) or 11 (discarded unknown) were used.
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fate_control.
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled.
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled.
#' @param country_code {\link[base]{character}} expected. Countries for which control will be made.
#' @param path_file {\link[base]{character}} expected. Path to save the final xlsx with all the informations for the correction.
#' @return The function returns two xlsx tables, one for the catch and one for the samples.
#' @export
fate_unknown_control <- function(data_connection,
                                 start_year,
                                 end_year,
                                 program,
                                 ocean,
                                 country_code,
                                 path_file = NULL) {
  # 0 - Global variables assignment ----
  fate_code <- NULL
  # 1 - Arguments verification ----
  if (r_type_checking(
    r_object = start_year,
    type = "integer",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = start_year,
      type = "integer",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = end_year,
    type = "integer",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = end_year,
      type = "integer",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = program,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = program,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = ocean,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = ocean,
      type = "character",
      output = "message"
    ))
  }
  if (r_type_checking(
    r_object = country_code,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = country_code,
      type = "character",
      output = "message"
    ))
  }
  if (!is.null(x = path_file) && r_type_checking(
    r_object = path_file,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = path_file,
      type = "character",
      output = "message"
    ))
  }
  # 2 - Data extraction ----
  observe_catch_sql <- paste(readLines(con = system.file("sql",
                                                         "observe_catch.sql",
                                                         package = "codama"
  )), collapse = "\n")
  observe_sample_sql <- paste(readLines(con = system.file("sql",
                                                          "observe_sample.sql",
                                                          package = "codama"
  )), collapse = "\n")
  # Correction of the sql query if ocean or country code not selected
  if ("%" %in% ocean) {
    observe_catch_sql <- sub(
      pattern = "AND o.label1 in (?ocean)",
      replacement = "AND o.label1 like (?ocean)",
      x = observe_catch_sql,
      fixed = TRUE
    )
    observe_sample_sql <- sub(
      pattern = "AND o.label1 in (?ocean)",
      replacement = "AND o.label1 like (?ocean)",
      x = observe_sample_sql,
      fixed = TRUE
    )
  }
  if ("%" %in% country_code) {
    observe_catch_sql <- sub(
      pattern = "AND co.iso3code in (?country_code)",
      replacement = "AND co.iso3code like (?country_code)",
      x = observe_catch_sql,
      fixed = TRUE
    )
    observe_sample_sql <- sub(
      pattern = "AND co.iso3code in (?country_code)",
      replacement = "AND co.iso3code like (?country_code)",
      x = observe_sample_sql,
      fixed = TRUE
    )
  }
  observe_catch_sql_final <- DBI::sqlInterpolate(
    conn = data_connection[[2]],
    sql = observe_catch_sql,
    start_year = DBI::SQL(start_year),
    end_year = DBI::SQL(end_year),
    program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
    ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
    country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
  )
  observe_sample_sql_final <- DBI::sqlInterpolate(
    conn = data_connection[[2]],
    sql = observe_sample_sql,
    start_year = DBI::SQL(start_year),
    end_year = DBI::SQL(end_year),
    program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
    ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
    country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
  )
  catch <- dplyr::tibble(DBI::dbGetQuery(
    conn = data_connection[[2]],
    statement = observe_catch_sql_final
  ))
  sample <- dplyr::tibble(DBI::dbGetQuery(
    conn = data_connection[[2]],
    statement = observe_sample_sql_final
  ))
  # 3 - Data manipulation ----
  catch_fate_unknown <- catch %>%
    dplyr::filter(fate_code %in% c(9, 11))
  cat(
    " Number of observations in catch with a fate code 9 or 11 :",
    nrow(catch_fate_unknown),
    "\n"
  )
  sample_fate_unknown <- sample %>%
    dplyr::filter(fate_code %in% c(9, 11))
  cat(
    " Number of observations in sample with a fate code 9 or 11 :",
    nrow(sample_fate_unknown),
    "\n"
  )
  # 4 - Export ----
  if (!is.null(x = path_file)) {
    timestamp <- format(
      lubridate::now(),
      "%Y%m%d_%H%M%S"
    )
    openxlsx::write.xlsx(as.data.frame(catch_fate_unknown),
                         file = paste0(
                           path_file,
                           "/catch_fate_unknown_control_",
                           country_code,
                           "_",
                           ocean,
                           "_",
                           start_year,
                           "-",
                           end_year,
                           "_",
                           timestamp,
                           ".xlsx"
                         ),
                         rowNames = FALSE
    )
    openxlsx::write.xlsx(as.data.frame(sample_fate_unknown),
                         file = paste0(
                           path_file,
                           "/sample_fate_unknown_control_",
                           country_code,
                           "_",
                           ocean,
                           "_",
                           start_year,
                           "-",
                           end_year,
                           "_",
                           timestamp,
                           ".xlsx"
                         ),
                         rowNames = FALSE
    )
  }
}
