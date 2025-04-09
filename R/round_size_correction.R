#' @name round_size_correction
#' @title Round size correction
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description This function corrects unrounded size samples to the inferior centimeter (or half centimeter for PD1).
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the size_control.
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234".
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on which control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param corrector {\link[base]{character}} expected. First letter of the corrector's first name and last name. Examples: 'JMartin' for Jeanne Martin.
#' @param action {\link[base]{character}} expected. Type of action required when update queries are launched. COMMIT is used to definitively validate modifications and make them permanent in the database. ROLLBACK is used to undo changes made.
#' @param path_file {\link[base]{character}} expected. By default NULL. Path to save the final xlsx.
#' @return The function corrects size samples directly in the database using update queries and returns a xlsx file with the corrected samples.
#' @export
round_size_correction <- function(data_connection,
                                  start_year,
                                  end_year,
                                  program,
                                  ocean,
                                  country_code,
                                  corrector,
                                  action,
                                  path_file = NULL) {
  # 0 - Global variables assignment ----
  length_was_computed <- NULL
  set_id <- NULL
  fao_code <- NULL
  samplemeasure_id <- NULL
  home_id <- NULL
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
  samples_size_error <- round_size_control(
    data_connection = data_connection,
    start_year = start_year,
    end_year = end_year,
    program = program,
    ocean = ocean,
    country_code = country_code,
    path_file = path_file,
    export_table = FALSE
  )
  # 3 - Query creation ----
  # Select the topiaid of the sizes to be corrected and the topiaid of the new ones
  samples_to_be_corrected <- samples_size_error %>%
    dplyr::filter(length_was_computed == FALSE)
  if (nrow(samples_to_be_corrected) > 0) {
    # Topiaid of the samplemeasures to be corrected
    to_be_corrected_samplemeasure_topiaid <- paste0(
      "'",
      paste0(samples_to_be_corrected$samplemeasure_id,
             collapse = "','"
      ),
      "'"
    )
    # Topiaid of the samples to be corrected
    to_be_corrected_sample_topiaid <- paste0(
      "'",
      paste0(samples_to_be_corrected$sample_id,
             collapse = "','"
      ),
      "'"
    )
    # Queries for the correction
    queries <- list(NULL)
    ct <- 0
    timestamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")
    date <- substr(timestamp, 1, 8)
    set_id_list <- unique(samples_to_be_corrected$set_id)
    for (i in seq_len(length(set_id_list))) {
      sample_set_i <- samples_to_be_corrected %>%
        dplyr::filter(set_id == set_id_list[i])
      sp_list_i <- unique(sample_set_i$fao_code)
      for (j in seq_len(length(sp_list_i))) {
        sample_set_i_sp_j <- sample_set_i %>%
          dplyr::filter(fao_code == sp_list_i[j])
        samplemeasure_id_set_i_sp_j <- unique(sample_set_i_sp_j$samplemeasure_id)
        for (k in seq_len(length(samplemeasure_id_set_i_sp_j))) {
          ct <- ct + 1
          sample_set_i_sp_j_sm_k <- sample_set_i_sp_j %>%
            dplyr::filter(samplemeasure_id == samplemeasure_id_set_i_sp_j[k])
          # Cas 1 : Size type différent de PD1 et longueur pas arrondie à .0
          # Cas 2 : Size type est PD1 et longueur pas arrondie à 0.5
          if (sample_set_i_sp_j_sm_k$size_type != "PD1") {
            queries[[ct]] <- paste(
              "UPDATE ps_observation.samplemeasure SET length = ",
              floor(sample_set_i_sp_j_sm_k$length),
              ", topiaversion = topiaversion+1, lastupdatedate = '",
              format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
              "' WHERE topiaid = '",
              sample_set_i_sp_j_sm_k$samplemeasure_id,
              "' AND length = ",
              sample_set_i_sp_j_sm_k$length,
              ";",
              sep = ""
            )
          } else if (sample_set_i_sp_j_sm_k$size_type == "PD1") {
            queries[[ct]] <- paste(
              "UPDATE ps_observation.samplemeasure SET length = '",
              floor(sample_set_i_sp_j_sm_k$length * 2) / 2,
              "', topiaversion = topiaversion+1, lastupdatedate = '",
              format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
              "' WHERE topiaid = '",
              sample_set_i_sp_j_sm_k$samplemeasure_id,
              "' AND length = '",
              sample_set_i_sp_j_sm_k$length,
              "';",
              sep = ""
            )
          }
        }
        ct <- ct + 1
        queries[[ct]] <- paste("UPDATE ps_observation.sample SET comment = concat(comment,'[Arrondi au centimetre inferieur (ou demi-centimetre inferieur pour les PD1) de la taille des echantillons ",
                               sp_list_i[j],
                               " - ",
                               date,
                               " - ",
                               corrector,
                               "]')",
                               ", topiaversion = topiaversion+1, lastupdatedate = '",
                               format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
                               "' WHERE topiaid = '",
                               sample_set_i$sample_id[j],
                               "';",
                               sep = ""
        )
      }
    }
    for (k in seq_along(queries)) {
      cat("[[", k, "]] ", queries[[k]], collapse = "\n\n", sep = "")
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
    n <- length(queries)
    for (k in 1:n) {
      cat("Query: ", k, "\n", sep = "")
      tryCatch(
        {
          result_query_k <- DBI::dbSendStatement(con1, queries[[k]])
          cat(queries[[k]], "\n")
          cat("completed: ", DBI::dbGetInfo(result_query_k)$has.completed, "\n\n", sep = "")
          ## Update the all_completed variable if necessary
          if (DBI::dbGetInfo(result_query_k)$has.completed != 1) {
            all_completed <- FALSE
          }
          DBI::dbClearResult(result_query_k)
        },
        error = function(e) {
          # In case of error, cancel the transaction
          DBI::dbRollback(con1)
          all_completed <<- FALSE
          error_occurred <<- TRUE
          print(paste("Error during query execution:", e$message))
        }
      )
      if (error_occurred) {
        break
      }
    }
    ## COMMIT or ROLLBACK the modifications
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
    ## To be corrected samplemeasures and samples are now corrected
    corrected_samplemeasure_topiaid <- to_be_corrected_samplemeasure_topiaid
    corrected_sample_topiaid <- to_be_corrected_sample_topiaid
    ## Check modified entries by lastupdate date and topiaid
    samplemeasure_updated_lastupdatedate <- RPostgreSQL::dbGetQuery(
      con1,
      paste("SELECT * FROM ps_observation.samplemeasure WHERE lastupdatedate >= '",
            Sys.Date(),
            "';",
            sep = ""
      )
    )
    if (nrow(samplemeasure_updated_lastupdatedate) > 0) {
      utils::View(samplemeasure_updated_lastupdatedate)
    } else if (nrow(samplemeasure_updated_lastupdatedate) == 0) {
      cat("\n", "No samples were modified today (samplemeasure_updated_lastupdatedate has 0 rows)", sep = "")
    }
    samplemeasure_updated_topiaid <- RPostgreSQL::dbGetQuery(
      con1,
      paste("SELECT * FROM ps_observation.samplemeasure WHERE topiaid in (",
            corrected_samplemeasure_topiaid,
            ");",
            sep = ""
      )
    )
    utils::View(samplemeasure_updated_topiaid)
    sample_updated_topiaid <- RPostgreSQL::dbGetQuery(
      con1,
      paste("SELECT * FROM ps_observation.sample WHERE topiaid in (",
            corrected_sample_topiaid,
            ");",
            sep = ""
      )
    )
    utils::View(sample_updated_topiaid)
    ## Trips to be recalculated
    trips_to_recalculate <- samples_to_be_corrected %>%
      dplyr::group_by(
        program,
        ocean,
        home_id,
        observer,
        trip_start_date,
        trip_end_date
      ) %>%
      dplyr::reframe()
    utils::View(trips_to_recalculate)
    # 5 - Exportation of the final check ----
    ## Data extraction to see corrected samples by the topiaid
    observe_sample_corrected_sql <- paste(readLines(con = system.file("sql",
                                                                      "observe_sample.sql",
                                                                      package = "codama"
    )), collapse = "\n")
    # Correction of the sql query
    observe_sample_corrected_sql <- sub(
      pattern = "extract(year from r.date) between (?start_year) and (?end_year)\nAND p.topiaid in (?program)\nAND o.label1 in (?ocean)\nAND co.iso3code in (?country_code)",
      replacement = paste0("sm.topiaid in (", corrected_samplemeasure_topiaid, ")"),
      x = observe_sample_corrected_sql,
      fixed = TRUE
    )
    sample_corrected_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = observe_sample_corrected_sql
    ))
    # 6 - Exportation ----
    timestamp <- format(
      lubridate::now(),
      "%Y%m%d_%H%M%S"
    )
    if (!is.null(x = path_file)) {
      openxlsx::write.xlsx(sample_corrected_data,
                           file = paste0(
                             path_file,
                             "/samples_size_corrected_",
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
    ## Close the connection
    DBI::dbDisconnect(con1)
  }
}
