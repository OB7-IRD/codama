#' @name round_size_control
#' @title Round size control
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description This function controls the accuracy (decimals) of size samples. It returns a table containing samples with decimal values that are not rounded to the inferior centimeter (or to the nearest half centimeter for PD1 measurements).
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the size_control.
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234".
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on which control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param path_file {\link[base]{character}} expected. By default NULL. Path to save the final xlsx.
#' @param export_table {\link[base]{logical}} expected. Save or not the xlsx table.
#' @return The function returns one xlsx table.
#' @export
round_size_control <- function(data_connection,
                         start_year,
                         end_year,
                         program,
                         ocean,
                         country_code,
                         path_file = NULL,
                         export_table) {
  # 0 - Global variables assignment ----
  size_type <- NULL
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
  # Code sizemeasuretype :
  # CCL CCW CDML CFL CLFL CLJFL CTL DL DML DW EFL FL IDL LJFL PAL PD1 PFL SCL SCW SL TL UNK
  # Cm inférieur :
  # CCL Longueur courbe de la carapace
  # CCW Largeur courbe de la carapace
  # CDML Longueur courbe dorsale du manteau
  # CFL Longueur courbe à la fourche
  # CLFL Longueur cleithrum-fourche
  # CLJFL Longueur courbe maxillaire inférieure-fourche
  # CTL Longueur courbe totale
  # DL Longueur du disque
  # DML Longueur dorsale du manteau
  # DW Largeur du disque
  # EFL Longueur oeil-fourche
  # FL Longueur à la fourche
  # IDL Longueur interdorsale
  # LJFL Longueur maxillaire inférieure-fourche
  # PAL Longueur pectorale-anale
  # PFL Longueur pectorale-fourche
  # SCL Longueur droite de la carapace
  # SCW Largeur droite de la carapace
  # SL Longueur standard
  # TL Longueur totale
  # UNK Inconnu
  # Demi-cm inférieur :
  # PD1 Longueur prédorsale
  # On garde les échantillons pour lesquels il y a erreur :
  # Size type différent de PD1 et longueur pas arrondie à .0
  # Size type est PD1 et longueur pas arrondie à 0.5
  sample_length_error <- sample %>%
    dplyr::filter((size_type != "PD1" & length != floor(length)) |
                    (size_type == "PD1" & length != floor(length * 2) / 2))
  ## Number of sample for which the size is wrong
  cat(
    "Number of samples with unrounded size :",
    nrow(sample_length_error),
    "(corresponding to",
    sum(sample_length_error$count),
    "individuals in total)"
  )
  # 4 - Export ----
  if (export_table == TRUE) {
    timestamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")
    if (!is.null(x = path_file)) {
      openxlsx::write.xlsx(as.data.frame(sample_length_error),
                           file = paste0(
                             path_file,
                             "/round_size_control_",
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
  return(sample_length_error)
}
