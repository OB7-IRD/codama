#' @name observed_system_vs_species_caught_control
#' @title Observed system vs species caught control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the Observer data any inconsistencies between observed systems and species caught.
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fate_control.
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled.
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled.
#' @param country_code {\link[base]{character}} expected. Countries for which control will be made.
#' @param path_file {\link[base]{character}} expected. Path to save the final xlsx with all the informations for the correction.
#' @return The function returns one xlsx table.
#' @export
observed_system_vs_species_caught_control <- function(data_connection,
                                                      start_year,
                                                      end_year,
                                                      program,
                                                      ocean,
                                                      country_code,
                                                      path_file = NULL) {
  # 0 - Global variables assignment ----
  observed_system_code <- NULL
  fao_code <- NULL
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
  # Requête qui extrait :
  # les activités de pêche (code 6) --> obligatoire
  # sur lesquelles dans les systèmes observés il y a 9, 10, 11, 12, 21, 22, 111, ou 112
  # ou alors dans les captures il y a RHN ou Cetacé
  observed_system_species_caught_sql <- paste(
    readLines(con = system.file("sql",
                                "observed_systems_vs_species_caught.sql",
                                package = "codama")), collapse = "\n"
  )
  # Correction of the sql query if ocean or country code not selected
  if ("%" %in% ocean) {
    observed_system_species_caught_sql <- sub(
      pattern = "AND o.label1 in (?ocean)",
      replacement = "AND o.label1 like (?ocean)",
      x = observed_system_species_caught_sql,
      fixed = TRUE
    )
  }
  if ("%" %in% country_code) {
    observed_system_species_caught_sql <- sub(
      pattern = "AND co.iso3code in (?country_code)",
      replacement = "AND co.iso3code like (?country_code)",
      x = observed_system_species_caught_sql,
      fixed = TRUE
    )
  }
  observed_system_species_caught_sql_final <- DBI::sqlInterpolate(
    conn = data_connection[[2]],
    sql = observed_system_species_caught_sql,
    start_year = DBI::SQL(start_year),
    end_year = DBI::SQL(end_year),
    program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
    ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
    country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
  )
  observed_system_species_caught <- dplyr::tibble(DBI::dbGetQuery(
    conn = data_connection[[2]],
    statement = observed_system_species_caught_sql_final
  ))
  # 3 - Data manipulation ----
  # observed_system_code : 9 / 10 / 11 / 111 / 112 pour MYS
  # observed_system_code : 12 / 21 / 22 pour RHN
  # fao_code : MYS / RHN
  observed_system_species_caught_error <- observed_system_species_caught %>%
    dplyr::filter(
      # Si RHN dans systèmes observés et pas RHN dans capture
      (grepl("12|21|22", observed_system_code) & !(grepl("RHN", fao_code))) |
        # Si RHN en capture et pas RHN dans systèmes observés
        (grepl("RHN", fao_code) & !(grepl("12|21|22", observed_system_code))) |
        # Si baleine en capture et pas dans systèmes observés
        (grepl("MYS", fao_code) & !(grepl("9|10|11|111|112", observed_system_code)))
    )
  cat(
    " Number of observations with inconsistency between observed systems and species caught :",
    nrow(observed_system_species_caught_error),
    "\n"
  )
  # 4 - Export ----
  if (!is.null(x = path_file)) {
    timestamp <- format(
      lubridate::now(),
      "%Y%m%d_%H%M%S"
    )
    openxlsx::write.xlsx(as.data.frame(observed_system_species_caught_error),
                         file = paste0(
                           path_file,
                           "/observed_system_vs_species_caught_control",
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
