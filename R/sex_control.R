#' @name sex_control
#' @title Sex control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description This function aims to check the coherence of the sex of a sample according to the possibility of sexing this species. For each inconsistency, it returns a table containing the details of the observation to correct.
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the sex_control
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234"
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on which control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param path_file {\link[base]{character}} expected. By default NULL. Path to save the final xlsx.
#' @return The function return one xlsx table.
#' @export
sex_control <- function(data_connection,
                        start_year,
                        end_year,
                        program,
                        ocean,
                        country_code,
                        path_file = NULL) {
  # 0 - Global variables assignment ----
  fao_code <- NULL
  species_group <- NULL
  sex <- NULL
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
  observe_sample_sql <- paste(readLines(con = system.file("sql",
                                                          "observe_sample.sql",
                                                          package = "codama"
  )), collapse = "\n")
  # Correction of the sql query if ocean or country code not selected
  if ("%" %in% ocean) {
    observe_sample_sql <- sub(
      pattern = "AND o.label1 in (?ocean)",
      replacement = "AND o.label1 like (?ocean)",
      x = observe_sample_sql,
      fixed = TRUE
    )
  }
  if ("%" %in% country_code) {
    observe_sample_sql <- sub(
      pattern = "AND co.iso3code in (?country_code)",
      replacement = "AND co.iso3code like (?country_code)",
      x = observe_sample_sql,
      fixed = TRUE
    )
  }
  observe_sample_sql_final <- DBI::sqlInterpolate(
    conn = data_connection[[2]],
    sql = observe_sample_sql,
    start_year = DBI::SQL(start_year),
    end_year = DBI::SQL(end_year),
    program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
    ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
    country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
  )
  sample <- dplyr::tibble(DBI::dbGetQuery(
    conn = data_connection[[2]],
    statement = observe_sample_sql_final
  ))
  # 3 - Data manipulation ----
  ## Filter sample to find all the sample with sex and which is not a sexable specie
  sample_with_sex <- sample %>%
    dplyr::filter(!(fao_code %in% c("DOL", "CFW", "DOX")),
                  !(species_group %in% c("Rays", "Sharks", "Turtles",
                                         "Cetaceans", "Whale shark")),
                  sex %in% c("Female", "Male"))
  ## Number of sample for which the sex is wrong
  cat(
    "Number of samples of non sexable species with sex :",
    nrow(sample_with_sex),
    "(corresponding to",
    sum(sample_with_sex$count),
    "individuals in total)"
  )
  # 4 - Export ----
  timestamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")
  if (!is.null(x = path_file)) {
    openxlsx::write.xlsx(as.data.frame(sample_with_sex),
                         file = paste0(
                           path_file,
                           "/sex_control_",
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
