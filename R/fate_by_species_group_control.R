#' @name fate_by_species_group_control
#' @title Fate by species control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description Identifies in the observer data all the species with an inconsistent fate according to their species group.
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the fate_control.
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled.
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled.
#' @param country_code {\link[base]{character}} expected. Countries for which control will be made.
#' @param path_file {\link[base]{character}} expected. Path to save the final xlsx with all the informations for the correction.
#' @param export_table {\link[base]{logical}} expected. Save or not the xlsx tables.
#' @return The function returns two xlsx tables, one for the catch and one for the samples.
#' @export
fate_by_species_group_control <- function(data_connection,
                                          start_year,
                                          end_year,
                                          program,
                                          ocean,
                                          country_code,
                                          path_file = NULL,
                                          export_table) {
  # 0 - Global variables assignment ----
  species_group <- NULL
  fate_code <- NULL
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
  # Thon mineur : species_group = "Tunas nei" & fao_code %in% thon_mineur_sp
  thon_mineur_sp <- c("BLT", "FRI", "FRZ", "KAW", "LTA")
  # Thon majeur : species_group = "Tunas nei" & fao_code %in% thon_majeur_sp
  thon_majeur_sp <- c("ALB", "BET", "SKJ", "YFT", "TUS")
  # Espece sensible : species_group = "Sharks" ou "Turtles" ou "Rays"
  # Bycatch : species_group = "Other bony fishes"
  # Catch
  catch_fate_by_species_group <- catch %>%
    dplyr::filter(
      # Les codes 1 (échappé du filet) / 2 (sorti vivant du filet) / 3 (sorti mort du filet) sont réservés aux baleines et requins baleine
      (fate_code %in% c(1, 2, 3) & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        # Les requins baleine / baleines ne peuvent pas prendre que les codes 1, 2 ou 3
        (species_group %in% c("Cetaceans", "Whale shark") & !(fate_code %in% c(1, 2, 3))) |
        # Le code 6 (conservé à destination de la conserverie) est réservé aux SKJ / YFT / BET / ALB
        (fate_code == 6 & !(fao_code %in% c("YFT", "BET", "SKJ", "ALB"))) |
        # Le code 15 (marché local) est réservé aux bycatch ou aux thons mineurs
        (fate_code == 15 & (!(species_group %in% c("Billfishes", "Other bony fishes") | (species_group == "Tunas nei" & !(fao_code %in% c("YFT", "BET", "SKJ", "ALB")))))) |
        # Le code 10 (ailerons seulement) est réservé aux requins
        (fate_code == 10 & species_group != "Sharks")
    )
  # On a les catch avec un fate by species group qui pose souci
  # Combien peuvent être corrigées automatiquement ?
  catch_correction_automatic <- catch_fate_by_species_group %>%
    dplyr::filter(
      # Cas 1a : code 6 et bycatch : correction en code 15
      (fate_code == 6 & species_group == "Other bony fishes") |
        # Cas 1b : code 6 et thon mineur : correction en code 15
        (fate_code == 6 & species_group == "Tunas nei" & fao_code %in% thon_mineur_sp) |
        # Cas 1c : code 6 et billfish : correction en code 15
        (fate_code == 6 & species_group == "Billfishes") |
        # Cas 2a : code 6 et espèce sensible : correction en code 16
        (fate_code == 6 & species_group %in% c("Sharks", "Turtles", "Rays")) |
        # Cas 2b : code 15 et espèce sensible : correction en code 16
        (fate_code == 15 & species_group %in% c("Sharks", "Turtles", "Rays")) |
        # Cas 3 : code 15 et thon majeur : correction en code 6
        (fate_code == 15 & species_group == "Tunas nei" & fao_code %in% thon_majeur_sp) |
        # Cas 4 : code 1 / 2 et pas RHN / MYS : correction en code 4
        (fate_code %in% c(1, 2, 3) & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        # Cas 5 : code 3 et pas RHN / MYS : correction en code 5
        (fate_code == 3 & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        # Cas 6 : RHN / MYS et code 4 : correction en code 2
        (fate_code == 4 & species_group %in% c("Cetaceans", "Whale shark")) |
        # Cas 7 : RHN / MYS et code 5 : correction en code 3
        (fate_code == 5 & species_group %in% c("Cetaceans", "Whale shark"))
    )
  cat(
    " Number of observations in catch with an inconsistent fate according to the species group:",
    nrow(catch_fate_by_species_group),
    "(corresponding to",
    sum(catch_fate_by_species_group$count, na.rm = TRUE),
    "individuals)",
    "\n",
    "Number of observations in catch with an inconsistent fate according to the species group that can be automatically corrected:",
    nrow(catch_correction_automatic),
    "\n",
    "Proportion of observations that can be automatically corrected:",
    round(100 * nrow(catch_correction_automatic) / nrow(catch_fate_by_species_group)),
    "%",
    "\n"
  )
  # Sample
  sample_fate_by_species_group <- sample %>%
    dplyr::filter(
      (fate_code %in% c(1, 2, 3) & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        (fate_code == 6 & !(fao_code %in% c("YFT", "BET", "SKJ", "ALB"))) |
        (fate_code == 15 & (!(species_group %in% c("Billfishes", "Other bony fishes") | (species_group == "Tunas nei" & !(fao_code %in% c("YFT", "BET", "SKJ", "ALB")))))) |
        (fate_code == 10 & species_group != "Sharks")
    )
  # On a les sample avec un fate by species group qui pose souci
  # Combien peuvent être corrigés automatiquement ?
  sample_correction_automatic <- sample_fate_by_species_group %>%
    # On ne garde que ce qui peut être corrigé automatiquement
    dplyr::filter(
      # Cas 1a : code 6 et bycatch
      (fate_code == 6 & species_group == "Other bony fishes") |
        # Cas 1b : code 6 et thon mineur
        (fate_code == 6 & species_group == "Tunas nei" & fao_code %in% thon_mineur_sp) |
        # Cas 1c : code 6 et billfish
        (fate_code == 6 & species_group == "Billfishes") |
        # Cas 2a : code 6 et espèce sensible
        (fate_code == 6 & species_group %in% c("Sharks", "Turtles", "Rays")) |
        # Cas 2b : code 15 et espèce sensible
        (fate_code == 15 & species_group %in% c("Sharks", "Turtles", "Rays")) |
        # Cas 3 : code 15 et thon majeur
        (fate_code == 15 & species_group == "Tunas nei" & fao_code %in% thon_majeur_sp) |
        # Cas 4 : code 1 / 2 et pas RHN / MYS
        (fate_code %in% c(1, 2, 3) & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        # Cas 5 : code 3 et pas RHN / MYS
        (fate_code == 3 & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        # Cas 6 : RHN / MYS et code 4
        (fate_code == 4 & species_group %in% c("Cetaceans", "Whale shark")) |
        # Cas 7 : RHN / MYS et code 5
        (fate_code == 5 & species_group %in% c("Cetaceans", "Whale shark"))
    )
  cat(
    " Number of observations in sample with an inconsistent fate according to the species group:",
    nrow(sample_fate_by_species_group),
    "(corresponding to",
    sum(sample_fate_by_species_group$count, na.rm = TRUE),
    "individuals)",
    "\n",
    "Number of observations in sample with an inconsistent fate according to the species group that can be automatically corrected:",
    nrow(sample_correction_automatic),
    "\n",
    "Proportion of observations that can be automatically corrected:",
    round(100 * nrow(sample_correction_automatic) / nrow(sample_fate_by_species_group)),
    "%",
    "\n"
  )
  # 4 - Export ----
  if (export_table == TRUE) {
    if (!is.null(x = path_file)) {
      timestamp <- format(
        lubridate::now(),
        "%Y%m%d_%H%M%S"
      )
      openxlsx::write.xlsx(as.data.frame(catch_fate_by_species_group),
                           file = paste0(
                             path_file,
                             "/catch_fate_by_species_group_control",
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
      openxlsx::write.xlsx(as.data.frame(sample_fate_by_species_group),
                           file = paste0(
                             path_file,
                             "/sample_fate_by_species_group_control",
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
  tables_fate_by_species_group <- list(catch = catch_fate_by_species_group,
                                       sample = sample_fate_by_species_group)
  return(tables_fate_by_species_group)
}
