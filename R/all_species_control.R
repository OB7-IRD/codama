#' @name all_species_control
#' @title All species control
#' @author Esther Mollier, Philippe S. Sabarros
#' @note Version 1.0
#' @description This function aims to check the coherence of the species caught in a given ocean according to their distribution area. For each inconsistency, it returns a table containing the details of the observation to correct.
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the all_species_control
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234"
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on which control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param path_file {\link[base]{character}} expected. By default NULL. Path to save the final xlsx.
#' @return The function return two xlsx tables.
#' @export
all_species_control <- function(data_connection,
                                start_year,
                                end_year,
                                program,
                                ocean,
                                country_code,
                                path_file = NULL) {
  # 0 - Global variables assignment ----
  fao_code <- NULL
  scientific_name <- NULL
  common_name <- NULL
  weight_tons <- NULL
  count <- NULL
  ocean_presence <- NULL
  default_measure_type <- NULL
  correspondence <- NULL
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
  observe_catch_sql <- paste(
    readLines(con = system.file(
      "sql",
      "observe_catch.sql",
      package = "codama"
    )),
    collapse = "\n"
  )
  observe_species_sql <- paste(
    readLines(con = system.file(
      "sql",
      "observe_species.sql",
      package = "codama"
    )),
    collapse = "\n"
  )
  ## Correction of the sql query if ocean or country code not selected
  if (ocean == "%") {
    observe_catch_sql <- sub(
      pattern = "AND o.label1 in (?ocean)",
      replacement = "AND o.label1 like (?ocean)",
      x = observe_catch_sql,
      fixed = TRUE
    )
  }
  if (country_code == "%") {
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
  species <- dplyr::tibble(DBI::dbGetQuery(
    conn = data_connection[[2]],
    statement = observe_species_sql
  ))
  # 3 - Data manipulation ----
  ## Overall species control
  ### Summarise catch by fao code, scientific_name, common name and ocean
  summarise_catch <- catch %>%
    dplyr::group_by(
      fao_code,
      scientific_name,
      common_name,
      ocean
    ) %>%
    dplyr::summarise(
      nb_catch = sum(count),
      weight_catch = sum(weight_tons),
      .groups = "drop"
    ) %>%
    dplyr::rename(ocean_catch = ocean)
  ### Summarise species
  summarise_species <- species %>%
    dplyr::rename(ocean_presence = ocean) %>%
    dplyr::group_by(fao_code) %>%
    dplyr::select(-default_measure_type)
  if (ocean != "%") {
    summarise_species <- summarise_species %>%
      dplyr::filter(ocean_presence == ocean)
  }
  ### Merge of the two tables to add the ocean_presence for each species
  summarise_catch_species <- merge(
    summarise_catch,
    summarise_species,
    by = c("fao_code"),
    all.x = TRUE,
    all.y = FALSE
  )
  ### Add a column to check if both oceans are the same
  overall_species_control <- summarise_catch_species %>%
    dplyr::mutate(correspondence = ifelse(
      is.na(ocean_presence),
      FALSE,
      TRUE
    ))
  ## Detailed species control
  ### Selection of the species for which we found an inconsistency
  inconsistent_observation <- overall_species_control %>%
    dplyr::filter(correspondence == FALSE)
  inconsistent_species_list <- unique(inconsistent_observation$fao_code)
  if (nrow(inconsistent_observation) != 0) {
    cat("Species not caught in their spatial range: ",
        length(inconsistent_species_list),
        "\n")
  } else {
    cat("No species caught outside its spatial range ",
        "\n")
  }
  ### Found in catch data these observations
  detailed_species_control <- catch %>%
    dplyr::filter(fao_code %in% inconsistent_species_list)
  # 4 - Export ----
  ## Fold creation for the overall species control
  folder_overall_species_control <- paste0(
    path_file,
    "/overall_species_control"
  )
  if (file.exists(folder_overall_species_control) == FALSE) {
    dir.create(folder_overall_species_control)
  }
  ## Export overall species control
  timestamp <- format(
    lubridate::now(),
    "%Y%m%d_%H%M%S"
  )
  if (!is.null(x = path_file)) {
    openxlsx::write.xlsx(
      overall_species_control,
      file = paste0(
        path_file,
        "/overall_species_control",
        "/overall_species_control_",
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
  ## Fold creation for the detailed species control
  folder_detailed_species_control <- paste0(
    path_file,
    "/detailed_species_control"
  )
  if (file.exists(folder_detailed_species_control) == FALSE) {
    dir.create(folder_detailed_species_control)
  }
  ## Export detailed species control
  timestamp <- format(
    lubridate::now(),
    "%Y%m%d_%H%M%S"
  )
  if (!is.null(x = path_file)) {
    openxlsx::write.xlsx(
      detailed_species_control,
      file = paste0(
        path_file,
        "/detailed_species_control",
        "/detailed_species_control_",
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
