#' @name more_samples_than_count
#' @title More_samples_than_count
#' @description Identifies in the Observer data all the occurrences of observations with a number of samples superior to the number of catches.
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the more_samples_than_count
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234"
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on which control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param path_file {\link[base]{character}} expected. Path to save the final xlsx.
#' @return The function return a xlsx table.
#' @export
more_samples_than_count <- function(data_connection,
                                    start_year,
                                    end_year,
                                    program,
                                    ocean,
                                    country_code,
                                    path_file = NULL) {
  # 0 - Global variables assignement ----
  set_id <- NULL
  count <- NULL
  home_id <- NULL
  observer <- NULL
  trip_start_date <- NULL
  trip_end_date <- NULL
  observation_date <- NULL
  observation_time <- NULL
  fao_code <- NULL
  fate_code <- NULL
  nb_catch <- NULL
  nb_measure <- NULL
  # 1 - Arguments verification ----
  r_type_checking(
    r_object = start_year,
    type = "integer"
  )
  r_type_checking(
    r_object = end_year,
    type = "integer"
  )
  r_type_checking(
    r_object = program,
    type = "character"
  )
  r_type_checking(
    r_object = ocean,
    type = "character"
  )
  r_type_checking(
    r_object = country_code,
    type = "character"
  )
  r_type_checking(
    r_object = path_file,
    type = "character"
  )
  # 2 - Data extraction ----
  if (data_connection[[1]] == "observe") {
    observe_catch_sql <- paste(
      readLines(con = system.file("sql",
                                  "observe_catch.sql",
                                  package = "codama"
      )),
      collapse = "\n"
    )
    observe_sample_sql <- paste(
      readLines(con = system.file("sql",
                                  "observe_sample.sql",
                                  package = "codama"
      )),
      collapse = "\n"
    )
    ## Correction of the sql query if ocean or country code not selected
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
  ## Summarise catch and sample by set_id, ocean, program, fao code and fate code
  summarise_catch <- catch %>%
    dplyr::group_by(
      set_id,
      ocean,
      home_id,
      program,
      observer,
      trip_start_date,
      trip_end_date,
      observation_date,
      observation_time,
      fao_code,
      fate_code
    ) %>%
    dplyr::rename(fate_code_catch = fate_code) %>%
    dplyr::summarise(
      nb_catch = sum(count),
      .groups = "drop"
    )
  summarise_sample <- sample %>%
    dplyr::group_by(
      set_id,
      ocean,
      home_id,
      program,
      observer,
      trip_start_date,
      trip_end_date,
      observation_date,
      observation_time,
      fao_code,
      fate_code
    ) %>%
    dplyr::rename(fate_code_sample = fate_code) %>%
    dplyr::summarise(
      nb_measure = sum(count),
      .groups = "drop"
    )
  ## Merge of the two tables
  summarise_sample_catch <- merge(
    x = summarise_catch,
    y = summarise_sample,
    by = c(
      "set_id",
      "ocean",
      "home_id",
      "program",
      "observer",
      "trip_start_date",
      "trip_end_date",
      "observation_date",
      "observation_time",
      "fao_code"
    ),
    all.x = TRUE,
    all.y = TRUE
  )
  ## Add zero if catch is NA after merging (meaning there is no catch observation for this sampled species and fate)
  summarise_sample_catch <- summarise_sample_catch %>%
    dplyr::mutate(nb_catch = ifelse(is.na(nb_catch),
                                    0,
                                    nb_catch
    ))
  ## Filter if sample > catch
  more_samples_than_count <- summarise_sample_catch %>%
    dplyr::filter(nb_measure > nb_catch)
  # 4 - Export ----
  # Add more conditions if ocean of country_code not given
  if (!is.null(x = path_file)) {
    timestamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")
    openxlsx::write.xlsx(as.data.frame(more_samples_than_count),
                         file = paste0(
                           path_file,
                           "/more_samples_than_count_",
                           paste(country_code,
                                 collapse = "_"
                           ),
                           "_",
                           paste(ocean,
                                 collapse = "_"
                           ),
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
