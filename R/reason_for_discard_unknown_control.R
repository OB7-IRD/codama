#' @name reason_for_discard_unknown_control
#' @title Reason for discard unknown control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the observer data cases where the reason for discard is 99 or NA.
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the sex_control.
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234".
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on which control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param path_file {\link[base]{character}} expected. By default NULL. Path to save the final xlsx.
#' @return The function returns one xlsx table.
#' @export
reason_for_discard_unknown_control <- function(data_connection,
                                               start_year,
                                               end_year,
                                               program,
                                               ocean,
                                               country_code,
                                               path_file = NULL) {
  # 0 - Global variables assignment ----
  fate_code <- NULL
  reason_discarded <- NULL
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
  # Correction of the sql query if ocean or country code not selected
  if ("%" %in% ocean) {
    observe_catch_sql <- sub(
      pattern = "AND o.label1 in (?ocean)",
      replacement = "AND o.label1 like (?ocean)",
      x = observe_catch_sql,
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
  catch <- dplyr::tibble(DBI::dbGetQuery(
    conn = data_connection[[2]],
    statement = observe_catch_sql_final
  ))
  # 3 - Data manipulation ----
  catch_reason_discard_unknown <- catch %>%
    dplyr::filter(fate_code %in% c(1, 2, 3, 4, 5, 10, 11, 13, 14),
                  is.na(reason_discarded) | reason_discarded == "Other (to detail in the comments)")
  cat(
    "Number of observations in catch with a reason of discard 99 or null :",
    nrow(catch_reason_discard_unknown),
    "(corresponding to",
    sum(catch_reason_discard_unknown$count, na.rm = TRUE),
    "individuals)",
    "\n"
  )
  # 4 - Export ----
  timestamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")
  if (!is.null(x = path_file)) {
    openxlsx::write.xlsx(as.data.frame(catch_reason_discard_unknown),
                         file = paste0(
                           path_file,
                           "/reason_for_discard_unknown_control_",
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
