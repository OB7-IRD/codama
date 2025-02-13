#' @name sample_with_no_fate_control
#' @title Sample with no fate control
#' @author Esther Mollier, Philippe S. Sabarros
#' @note Version 1.0
#' @description This function aims to find all the samples in the observer data that don't have a fate and attributes it one when possible
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the sample_with_no_fate_control
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234"
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on which control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param path_file {\link[base]{character}} expected. By default NULL. Path to save the final xlsx.
#' @return The function return an xlsx tables.
#' @export
sample_with_no_fate_control <- function(data_connection,
                                        start_year,
                                        end_year,
                                        program,
                                        ocean,
                                        country_code,
                                        path_file = NULL) {
  # 0 - Global variables assignment ----
  fate_code <- NULL
  set_id <- NULL
  fao_code <- NULL
  new_fate_id <- NULL
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
  ## Filter sample to find all the samplemeasure without no fate
  sample_with_no_fate <- sample %>%
    dplyr::filter(is.na(fate_code))
  ## Select all the sets concerned
  set_id_samples_with_no_fate <- unique(sample_with_no_fate$set_id)
  ## Create a column for the new fate id
  sample_with_no_fate$new_fate_id <- NA
  ## We select all the catches and samples for each set containing samples with no fate
  for (i in seq_len(length(set_id_samples_with_no_fate))) {
    catch_i <- catch %>%
      dplyr::filter(set_id == set_id_samples_with_no_fate[i])
    sample_i <- sample_with_no_fate %>%
      dplyr::filter(set_id == set_id_samples_with_no_fate[i])
    ## For each set we select all the species concerned by an absence of fate
    species_sample_i <- unique(sample_i$fao_code)
    ## For each species concerned in the set we select the catches and samples with no fate
    for (j in seq_len(length(species_sample_i))) {
      catch_j <- catch_i %>%
        dplyr::filter(fao_code == species_sample_i[j])
      sample_j <- sample_i %>%
        dplyr::filter(fao_code == species_sample_i[j])
      ## For each species we select the samplemeasure id concerned
      sample_j_topiaid <- unique(sample_j$samplemeasure_id)
      ## If there is only one fate in catches we attribute it to the sample in the column new_fate_id
      if (length(unique(catch_j$fate_code)) == 1) {
        catch_fate_id <- catch_j$fate_id
        sample_with_no_fate <- sample_with_no_fate %>%
          dplyr::mutate(new_fate_id = ifelse(sample_with_no_fate$samplemeasure_id %in% sample_j_topiaid,
                                             catch_fate_id,
                                             sample_with_no_fate$new_fate_id
          ))
      }
    }
  }
  ## Number of sample for which we attributed a new fate
  sample_to_be_corrected <- sample_with_no_fate %>%
    dplyr::filter(!is.na(new_fate_id))
  cat(
    "Number of samples with not fate for which a fate can be reassigned :",
    nrow(sample_to_be_corrected),
    "\n"
  )
  # Number of samples for which we can't attribut a new fate
  cat(
    "Number of samples with no fate for which a fate cannot be reassigned :",
    nrow(sample_with_no_fate) - nrow(sample_to_be_corrected),
    "\n"
  )
  # Percentage of sample for which we attributed a new fate
  percentage_sample_with_new_fate <- (nrow(sample_to_be_corrected)) / (nrow(sample_with_no_fate))
  cat(
    "Proportion of samples with no fate for which a fate can be reassigned :",
    round(100 * percentage_sample_with_new_fate), "%",
    "\n"
  )
  # 4 - Export ----
  timestamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")
  if (!is.null(x = path_file)) {
    openxlsx::write.xlsx(as.data.frame(sample_with_no_fate),
                         file = paste0(
                           path_file,
                           "/sample_with_no_fate_control_",
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
