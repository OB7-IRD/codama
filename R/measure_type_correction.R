#' @name measure_type_correction
#' @title Measure type correction
#' @description This function aims to correct directly in the data base samples with incorrect measure type
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the measure_type_control
#' @param start_year {\link[base]{integer}} expected. Starting year for the correction.
#' @param end_year {\link[base]{integer}} expected. Ending year for the correction.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234"
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on wich control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param species_group {\link[base]{character}} expected. Specie group to correct.
#' @param species {\link[base]{character}} expected. Species code to correct. Examples: 'YFT', 'FAL'...etc.
#' @param sizemeasuretype_to_replace {\link[base]{character}} expected. Measure type of the sample we want to correct. Examples: 'FL', 'DW'...etc.
#' @param sizemeasuretype_new {\link[base]{character}} expected. Measure type to replace the incorrect one. Examples: 'FL', 'DW'...etc.
#' @param corrector {\link[base]{character}} expected. First letter of the corrector's first name and last name. Examples: 'JMartin' for Jeanne Martin
#' @param action {\link[base]{character}} expected. Type of action required when update queries are launched. COMMIT is used to definitively validate modifications and make them permanent in the database. ROLLBACK is used to undo changes made. Examples: 'JMartin' for Jeanne Martin
#' @param path_file {\link[base]{character}} expected. By default NULL. Path to save the final xlsx.
#' @return The function returns a xlsx table.
#' @export
measure_type_correction <- function(data_connection,
                                    start_year,
                                    end_year,
                                    program,
                                    ocean,
                                    country_code,
                                    species_group,
                                    species,
                                    sizemeasuretype_to_replace,
                                    sizemeasuretype_new,
                                    corrector,
                                    action,
                                    path_file = NULL) {
  # 0 - Global variables assignment ----
  homeid <- NULL
  observer <- NULL
  trip_start_date <- NULL
  trip_end_date <- NULL
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
    r_object = species_group,
    type = "character"
  )
  r_type_checking(
    r_object = species,
    type = "character"
  )
  r_type_checking(
    r_object = sizemeasuretype_to_replace,
    type = "character",
    allowed_value = c("PD1", "LJFL", "SCL", "TL", "FL", "DW")
  )
  r_type_checking(
    r_object = sizemeasuretype_new,
    type = "character"
  )
  r_type_checking(
    r_object = sizemeasuretype_new,
    type = "character"
  )
  r_type_checking(
    r_object = corrector,
    type = "character"
  )
  r_type_checking(
    r_object = action,
    type = "character",
    allowed_value = c("COMMIT", "ROLLBACK")
  )
  r_type_checking(
    r_object = path_file,
    type = "character"
  )
  # 2 - Data extraction ----
  if (data_connection[[1]] == "observe") {
    observe_sample_sql <- paste(readLines(con = system.file("sql",
      "observe_sample_measure_type_correction.sql",
      package = "codama"
    )), collapse = "\n")
    observe_size_measure_type_sql <- paste(readLines(con = system.file("sql",
      "observe_size_measure_type.sql",
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
    if ("%" %in% species_group) {
      observe_sample_sql <- sub(
        pattern = "AND sg.label1 in (?species_group)",
        replacement = "AND sg.label1 like (?species_group)",
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
      country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'")),
      species = DBI::SQL(paste0("'", paste0(species, collapse = "', '"), "'")),
      species_group = DBI::SQL(paste0("'", paste0(species_group, collapse = "', '"), "'")),
      sizemeasuretype_to_replace = DBI::SQL(paste0("'", paste0(sizemeasuretype_to_replace, collapse = "', '"), "'"))
    )

    sample <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = observe_sample_sql_final
    ))
    size_measure_type <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = observe_size_measure_type_sql
    ))
  }
  # 3 - Exportation of the data before modification ----
  cat(
    " Records in samples to be corrected (",
    sizemeasuretype_to_replace,
    " to ",
    sizemeasuretype_new,
    ") : ",
    nrow(sample),
    "\n",
    sep = ""
  )
  ## Fold creation for the year
  folder_year <- paste0(
    path_file,
    "/",
    start_year,
    "-",
    end_year
  )
  if (file.exists(folder_year) == FALSE) {
    dir.create(folder_year)
  }
  ## Fold creation for the samples to be corrected
  folder_sample_to_be_corrected <- paste0(
    path_file,
    "/",
    start_year,
    "-",
    end_year,
    "/corrections_",
    paste(species),
    "_samples_",
    sizemeasuretype_to_replace
  )
  if (file.exists(folder_sample_to_be_corrected) == FALSE) {
    dir.create(folder_sample_to_be_corrected)
  }
  timestamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")
  if (!is.null(x = path_file)) {
    openxlsx::write.xlsx(as.data.frame(sample),
      file = paste0(
        path_file,
        "/",
        start_year,
        "-",
        end_year,
        "/corrections_",
        species,
        "_samples_",
        sizemeasuretype_to_replace,
        "/sizesamples_observe_",
        species_group,
        "_",
        species,
        "_",
        sizemeasuretype_to_replace,
        "_",
        timestamp,
        ".xlsx"
      ),
      rowNames = FALSE
    )
  }
  # 4 - Query creation ----
  # Select the topiaid of the measuretype to be corrected and the topiaid of the new one
  sizemeasuretype_to_replace_topiaid <- size_measure_type$topiaid[size_measure_type$code == sizemeasuretype_to_replace]
  sizemeasuretype_new_topiaid <- size_measure_type$topiaid[size_measure_type$code == sizemeasuretype_new]
  # Topiaid of samples to be corrected
  to_be_corrected_samplemeasure_topiaid <- paste0("'", paste0(sample$samplemeasure_id, collapse = "','"), "'")
  # List of the samples concerned by the correction
  sample_id_list <- unique(sample$sample_id)
  # Queries for the correction
  queries <- list(NULL)
  ct <- 0
  date <- substr(timestamp, 1, 8)

  for (i in seq_len(sample_id_list)) {
    sample_i <- sample[sample$sample_id == sample_id_list[i], ]
    for (j in seq_len(nrow(sample_i))) {
      ct <- ct + 1
      queries[[ct]] <- paste("UPDATE ps_observation.samplemeasure SET sizemeasuretype = '",
        sizemeasuretype_new_topiaid,
        "', topiaversion = topiaversion+1, lastupdatedate = '",
        paste(Sys.time(),
          ".000",
          sep = ""
        ),
        "' WHERE topiaid = '",
        sample_i$samplemeasure_id[j],
        "' AND sizemeasuretype = '",
        sizemeasuretype_to_replace_topiaid,
        "';",
        sep = ""
      )
    }
    ct <- ct + 1
    queries[[ct]] <- paste("UPDATE ps_observation.sample SET comment = concat(comment,'[Correction ",
      species,
      " type de mesure ",
      sizemeasuretype_to_replace,
      " en ",
      sizemeasuretype_new,
      " - ",
      date,
      " - ",
      corrector,
      "]')",
      ", topiaversion = topiaversion+1, lastupdatedate = '",
      paste(Sys.time(),
        ".000",
        sep = ""
      ),
      "' WHERE topiaid = '",
      sample_id_list[i],
      "';",
      sep = ""
    )
  }
  for (i in seq_along(queries)) {
    cat("[[", i, "]] ", queries[[i]], collapse = "\n\n", sep = "")
  }
  # 4 - Query execution ----
  ## Connection to database
  con1 <- data_connection[[2]]
  cat("Number of update queries to be run : ", length(queries), "\n", sep = "")
  DBI::dbBegin(con1)
  ## Create variables to record query status
  all_completed <- TRUE
  error_occurred <- FALSE
  ## Loop start
  for (k in seq_along(queries)) {
    cat("Query: ", k, "\n\n", sep = "")
    tryCatch(
      {
        result_query_k <- DBI::dbSendStatement(con1, queries[[k]])
        cat(queries[[k]], "/n")
        cat("completed :", DBI::dbGetInfo(result_query_k)$completed, "\n\n", sep = "")
        ## Update the all_completed variable if necessary
        if (DBI::dbGetInfo(result_query_k)$completed != 1) {
          all_completed <- FALSE
        }
        DBI::dbClearResult(result_query_k)
      },
      error = function(e) {
        # In case of error, cancel the transaction
        DBI::dbRollback(con1)
        all_completed <- FALSE
        error_occurred <- TRUE
        print(paste("Error during query execution:", e$message))
      }
    )
    if (error_occurred) {
      break
    }
  }
  ## COMMIT or ROLLBACK the modifcations
  if (all_completed && action == "COMMIT") {
    DBI::dbCommit(con1)
  } else {
    DBI::dbRollback(con1)
  }
  ## Print the information on the operation's progress
  if (all_completed && action == "COMMIT") {
    cat("All queries successfully went through", "\n", sep = "")
  } else if (action == "ROLLBACK") {
    cat("No requests have been processed as ROLLBACK was selected", "\n", sep = "")
  } else {
    cat("At least one of the queries did not go through", "\n", sep = "")
  }
  ## To be corrected samples are now corrected
  corrected_samplemeasure_topiaid <- to_be_corrected_samplemeasure_topiaid
  # Check modified entries
  samples_updated_lastupdatedate <- RPostgreSQL::dbGetQuery(
    con1,
    paste("SELECT * FROM ps_observation.samplemeasure WHERE lastupdatedate >= '",
      Sys.Date(),
      "';",
      sep = ""
    )
  )
  View(samples_updated_lastupdatedate)
  samples_updated_topiaid <- RPostgreSQL::dbGetQuery(
    con1,
    paste("SELECT * FROM ps_observation.samplemeasure WHERE topiaid in (",
      corrected_samplemeasure_topiaid,
      ");",
      sep = ""
    )
  )
  View(samples_updated_topiaid)
  # Trips to be recalculated
  trips_to_recalculate <- sample %>%
    dplyr::group_by(
      program,
      ocean,
      homeid,
      observer,
      trip_start_date,
      trip_end_date
    ) %>%
    dplyr::summarise(.groups = "drop")
  print(trips_to_recalculate)
  # Close the connection
  DBI::dbDisconnect(con1)

  # 5 - Exportation of the final check ----
  # Data extraction to extract info on corrected samplmeasures by the topiaid
  if (data_connection[[1]] == "observe") {
    observe_sample_corrected_sql <- paste(readLines(con = system.file("sql",
      "observe_sample_measure_type_correction.sql",
      package = "codama"
    )), collapse = "\n")
    # Correction of the sql query
    observe_sample_corrected_sql <- sub(
      pattern = "extract(year from r.date) between (?start_year) and (?end_year)\nAND p.topiaid in (?program)\nAND o.label1 in (?ocean)\nAND co.iso3code in (?country_code)\nAND va.code = '6'\nAND sp.faocode in (?species)\nAND sg.label1 in (?species_group)\nAND smt.code in (?sizemeasuretype_to_replace)\n\n\n",
      replacement = paste0("sm.topiaid in (", corrected_samplemeasure_topiaid, ")"),
      x = observe_sample_corrected_sql,
      fixed = TRUE
    )
    observe_sample_corrected_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = observe_sample_corrected_sql
    ))
  }
  openxlsx::write.xlsx(as.data.frame(observe_sample_corrected_data),
    file = paste0(
      path_file,
      "/",
      start_year,
      "-",
      end_year,
      "/corrections_",
      species,
      "_samples_",
      sizemeasuretype_to_replace,
      "/sizesamples_observe_",
      species_group,
      "_",
      species,
      "_corrected_from_",
      sizemeasuretype_to_replace,
      "_to_",
      sizemeasuretype_new,
      "_",
      timestamp,
      ".xlsx"
    ),
    rowNames = FALSE
  )
}
