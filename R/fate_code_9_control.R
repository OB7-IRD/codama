#' @name fate_code_9_control
#' @title Fate code 9 control
#' @description Identifies in the Observer data all the fate code 9 and the comment associated
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fate_code_9_control
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled.
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled.
#' @param country_code {\link[base]{character}} expected. Countries for which control will be made.
#' @param path_file {\link[base]{character}} expected. Path to save the final xlsx with all the informations for the correction.
#' @return The function return a xlsx table.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr group_by summarise filter
#' @importFrom lubridate today
fate_code_9_control <- function(data_connection,
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
  if (data_connection[[1]] == "observe") {
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
      start_year = DBI::SQL(paste0(paste0(start_year, collapse = ", "))),
      end_year = DBI::SQL(paste0(paste0(end_year, collapse = ", "))),
      program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
      ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
      country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
    )
    observe_sample_sql_final <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = observe_sample_sql,
      start_year = DBI::SQL(paste0(paste0(start_year, collapse = ", "))),
      end_year = DBI::SQL(paste0(paste0(end_year, collapse = ", "))),
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
  }
  # 3 - Data design ----
  catch_fate_code_9 <- catch %>%
    dplyr::filter(fate_code == 9)
  cat(
    " Number of observations in catch with a fate code 9 : ",
    nrow(catch_fate_code_9),
    "\n"
  )
  sample_fate_code_9 <- sample %>%
    dplyr::filter(fate_code == 9)
  cat(
    " Number of observations in sample with a fate code 9 : ",
    nrow(sample_fate_code_9),
    "\n"
  )

  # 4 - Export ----
  ## Fold creation for the fate code 9 in catch
  folder_catch_fate_code_9 <- paste0(
    path_file,
    "/catch_fate_code_9"
  )
  if (file.exists(folder_catch_fate_code_9) == FALSE) {
    dir.create(folder_catch_fate_code_9)
  }
  if (!is.null(x = path_file)) {
    timestamp <- format(
      lubridate::now(),
      "%Y%m%d_%H%M%S"
    )
    openxlsx::write.xlsx(as.data.frame(catch_fate_code_9),
                         file = paste0(
                           path_file,
                           "/catch_fate_code_9/catch_fate_code_9_control_",
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
  ## Fold creation for the fate code 9 in sample
  folder_sample_fate_code_9 <- paste0(
    path_file,
    "/sample_fate_code_9"
  )
  if (file.exists(folder_sample_fate_code_9) == FALSE) {
    dir.create(folder_sample_fate_code_9)
  }
  if (!is.null(x = path_file)) {
    timestamp <- format(
      lubridate::now(),
      "%Y%m%d_%H%M%S"
    )
    openxlsx::write.xlsx(as.data.frame(sample_fate_code_9),
                         file = paste0(
                           path_file,
                           "/sample_fate_code_9/sample_fate_code_9_control_",
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
