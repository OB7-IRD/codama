#' @name measure_type_control
#' @title Measure_type_control
#' @description This function allows to check the coherence of a sampled species measure type according to its default measure type.
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the measure_type_control
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234"
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on wich control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param path_file {\link[base]{character}} expected. Path to save the final xlsx.
#' @return The function return a xlsx table.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr group_by summarise select mutate
#' @importFrom lubridate now
#' @importFrom openxlsx write.xlsx
measure_type_control <- function(data_connection,
                                 start_year,
                                 end_year,
                                 program,
                                 ocean,
                                 country_code,
                                 path_file = NULL) {
  # 0 - Global variables assignment ----
  fao_code <- NULL
  size_type <- NULL
  count <- NULL
  default_measure_type <- NULL
  fao_code <- NULL
  correspondence <- NULL
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
    observe_sample_sql <- paste(
      readLines(con = system.file("sql",
                                  "observe_sample.sql",
                                  package = "codama"
      )),
      collapse = "\n"
    )
    observe_species_sql <- paste(
      readLines(con = system.file("sql",
                                  "observe_species.sql",
                                  package = "codama"
      )),
      collapse = "\n"
    )
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
      start_year = DBI::SQL(paste0(paste0(start_year, collapse = ", "))),
      end_year = DBI::SQL(paste0(paste0(end_year, collapse = ", "))),
      program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
      ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
      country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
    )
    sample <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = observe_sample_sql_final
    ))
    species <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = observe_species_sql
    ))
  }
  # 3 - Data design ----
  ## Overall measure type control
  ### Remove the duplicated fao code (link to the ocean column)
  species <- species[!duplicated(species$fao_code), ]
  ### Summarise sample by fao code and size-type
  summarise_samplemeasure <- sample %>%
    dplyr::group_by(fao_code, size_type) %>%
    dplyr::summarise(nb_measure = sum(count), .groups = "drop")
  #### Summarise species
  summarise_species <- species %>%
    dplyr::select(-ocean)
  ### Merge of the two tables to add the default measure type for each species
  summarise_samplemeasure_species <- merge(summarise_samplemeasure,
                                           summarise_species,
                                           by = c("fao_code"),
                                           all.x = TRUE,
                                           all.y = FALSE
  )
  ### Add a column to check if both measure types are the same
  overall_measure_type_control <- summarise_samplemeasure_species %>%
    dplyr::mutate(correspondence = ifelse(size_type == default_measure_type,
                                          TRUE,
                                          FALSE
    ))
  ## Detailed measure type control
  ### Selection of the species for which we found an inconsistency
  inconsistent_observation <- overall_measure_type_control %>%
    dplyr::filter(correspondence == FALSE)
  cat(
    " Observation(s) with a wrong measure type: ",
    nrow(inconsistent_observation),
    "\n"
  )
  if (nrow(inconsistent_observation)!=0){
    for (i in nrow(inconsistent_observation)) {
      cat(inconsistent_observation$fao_code,
          " in ",
          inconsistent_observation$size_type,
          "\n"
      )
    }
  }
  ### Found in catch data these observations
  detailed_measure_type_control <- data.frame()
  if (nrow(inconsistent_observation)!=0){
    for (i in nrow(inconsistent_observation)) {
      detailed_measure_type_control <- rbind(
        detailed_measure_type_control,
        sample %>%
          dplyr::filter(fao_code == inconsistent_observation$fao_code[i] &
                          size_type == inconsistent_observation$size_type[i])
      )
    }
  }
  # 4 - Export ----
  ## Fold creation for the overall measure type control
  folder_overall_measure_type_control <- paste0(
    path_file,
    "/overall_measure_type_control"
  )
  if (file.exists(folder_overall_measure_type_control) == FALSE) {
    dir.create(folder_overall_measure_type_control)
  }
  ## Export overall measure type control
  timestamp <- format(
    lubridate::now(),
    "%Y%m%d_%H%M%S"
  )
  if (!is.null(x = path_file)) {
    openxlsx::write.xlsx(overall_measure_type_control,
                         file = paste0(
                           path_file,
                           "/overall_measure_type_control",
                           "/overall_measure_type_control_",
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
  folder_detailed_measure_type_control <- paste0(
    path_file,
    "/detailed_measure_type_control"
  )
  if (file.exists(folder_detailed_measure_type_control) == FALSE) {
    dir.create(folder_detailed_measure_type_control)
  }
  ## Export overall species control
  timestamp <- format(
    lubridate::now(),
    "%Y%m%d_%H%M%S"
  )
  if (!is.null(x = path_file)) {
    openxlsx::write.xlsx(detailed_measure_type_control,
                         file = paste0(
                           path_file,
                           "/detailed_measure_type_control",
                           "/detailed_measure_type_control_",
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
