#' @name fate_by_species_group_correction
#' @title Fate by species group correction
#' @author Chloé Tellier, Philippe S. Sabarros
#' @note Version 1.0
#' @description The function corrects erroneous fate in catch and samples for species according to the species group.
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the measure_type_control.
#' @param start_year {\link[base]{integer}} expected. Starting year for the control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234".
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on which control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param corrector {\link[base]{character}} expected. First letter of the corrector's first name and last name. Examples: 'JMartin' for Jeanne Martin.
#' @param action {\link[base]{character}} expected. Type of action required when update queries are launched. COMMIT is used to definitively validate modifications and make them permanent in the database. ROLLBACK is used to undo changes made.
#' @param path_file {\link[base]{character}} expected. By default NULL. Path to save the final xlsx.
#' @return The function corrects fate in samples and catch directly in the database and returns two xlsx file: one for the corrected samples, one for the corrected catch.
#' @export
fate_by_species_group_correction <- function(data_connection,
                                             start_year,
                                             end_year,
                                             program,
                                             ocean,
                                             country_code,
                                             corrector,
                                             action,
                                             path_file = NULL) {
  # 0 - Global variables assignment ----
  fate_code <- NULL
  species_group <- NULL
  fao_code <- NULL
  catch_id <- NULL
  set_id <- NULL
  samplemeasure_id <- NULL

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
  tables_fate_by_species_group_control <- fate_by_species_group_control(
    data_connection = data_connection,
    start_year = start_year,
    end_year = end_year,
    program = program,
    ocean = ocean,
    country_code = country_code,
    path_file = path_file,
    export_table = FALSE
  )
  catch_fate_by_species_group_control <- tables_fate_by_species_group_control$catch
  sample_fate_by_species_group_control <- tables_fate_by_species_group_control$sample
  observe_species_fate_sql <- paste(readLines(con = system.file("sql",
                                                                "observe_species_fate.sql",
                                                                package = "codama"
  )), collapse = "\n")
  species_fate <- dplyr::tibble(DBI::dbGetQuery(
    conn = data_connection[[2]],
    statement = observe_species_fate_sql
  ))

  # 3 - Query creation ----

  # RHN / MYS et code autre que 1 / 2 / 3 / 4 / 5 : correction manuelle
  # Pas un requin et code 10 : correction manuelle
  # Cas 1 : bycatch ou thon mineur ou billfish et code 6 : correction en code 15
  # Cas 2 : espèce sensible et code 6 ou code 15 : correction en code 16
  # Cas 3 : thon majeur et code 15 : correction en code 6
  # Cas 4 : pas RHN / MYS et code 1 / 2 : correction en code 4
  # Cas 5 : pas RHN / MYS et code 3 : correction en code 5
  # Cas 6 : RHN / MYS et code 4 : correction en code 2
  # Cas 7 : RHN / MYS et code 5 : correction en code 3

  # Thon mineur : species_group = "Tunas nei" & fao_code %in% thon_mineur_sp
  thon_mineur_sp <- c("BLT", "FRI", "FRZ", "KAW", "LTA")
  # Thon majeur : species_group = "Tunas nei" & fao_code %in% thon_majeur_sp
  thon_majeur_sp <- c("ALB", "BET", "SKJ", "YFT", "TUS")
  # Espece sensible : species_group = "Sharks" ou "Turtles" ou "Rays"
  # Bycatch : species_group = "Other bony fishes"

  # Plusieurs tables à modifier
  # catch : topiaversion, lastupdatedate, comment, speciesfate
  # sample : topiaversion, lastupdatedate, comment
  # samplemeasure : topiaversion, lastupdatedate, speciesfate

  # On va d'abord travailler sur la table catch
  catch_to_be_corrected <- catch_fate_by_species_group_control %>%
    # On ne garde que ce qui peut être corrigé automatiquement
    dplyr::filter(
      # Cas 1a : bycatch et code 6
      (fate_code == 6 & species_group == "Other bony fishes") |
        # Cas 1b : thon mineur et code 6
        (fate_code == 6 & species_group == "Tunas nei" & fao_code %in% thon_mineur_sp) |
        # Cas 1c : billfish et code 6
        (fate_code == 6 & species_group == "Billfishes") |
        # Cas 2a : espèce sensible et code 6
        (fate_code == 6 & species_group %in% c("Sharks", "Turtles", "Rays")) |
        # Cas 2b : espèce sensible et code 15
        (fate_code == 15 & species_group %in% c("Sharks", "Turtles", "Rays")) |
        # Cas 3 : thon majeur et code 15
        (fate_code == 15 & species_group == "Tunas nei" & fao_code %in% thon_majeur_sp) |
        # Cas 4 : pas RHN / MYS et code 1 / 2
        (fate_code %in% c(1, 2) & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        # Cas 5 : pas RHN / MYS et code 3
        (fate_code == 3 & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        # Cas 6 : RHN / MYS et code 4
        (fate_code == 4 & species_group %in% c("Cetaceans", "Whale shark")) |
        # Cas 7 : RHN / MYS et code 5
        (fate_code == 5 & species_group %in% c("Cetaceans", "Whale shark"))
    )
  if (nrow(catch_to_be_corrected) > 0) {
    # Topiaid of the catches to be corrected
    to_be_corrected_catch_topiaid <- paste0(
      "'",
      paste0(catch_to_be_corrected$catch_id,
             collapse = "','"
      ),
      "'"
    )
    # Queries for the correction
    queries_catch <- list(NULL)
    ct_catch <- 0
    timestamp_catch <- format(lubridate::now(), "%Y%m%d_%H%M%S")
    date_catch <- substr(timestamp_catch, 1, 8)
    catch_id_list <- catch_to_be_corrected$catch_id
    for (i in seq_len(nrow(catch_to_be_corrected))) {
      catch_to_be_corrected_i <- catch_to_be_corrected %>%
        dplyr::filter(catch_id == catch_id_list[i])
      ct_catch <- ct_catch + 1
      new_fate <- NULL
      if ((catch_to_be_corrected_i$fate_code == 6 && catch_to_be_corrected_i$species_group == "Other bony fishes") ||
          (catch_to_be_corrected_i$fate_code == 6 && catch_to_be_corrected_i$species_group == "Tunas nei" && catch_to_be_corrected_i$fao_code %in% thon_mineur_sp) ||
          (catch_to_be_corrected_i$fate_code == 6 && catch_to_be_corrected_i$species_group == "Billfishes")) {
        # Cas 1 : bycatch ou thon mineur ou billfish et code 6 : correction en code 15
        new_fate <- 15
      } else if ((catch_to_be_corrected_i$fate_code == 6 || catch_to_be_corrected_i$fate_code == 15) && catch_to_be_corrected_i$species_group %in% c("Sharks", "Turtles", "Rays")) {
        # Cas 2 : espèce sensible et code 6 ou code 15 : correction en code 16
        new_fate <- 16
      } else if (catch_to_be_corrected_i$fate_code == 15 && catch_to_be_corrected_i$species_group == "Tunas nei" && catch_to_be_corrected_i$fao_code %in% thon_majeur_sp) {
        # Cas 3 : thon majeur et code 15 : correction en code 6
        new_fate <- 6
      } else if (catch_to_be_corrected_i$fate_code %in% c(1, 2) && !(catch_to_be_corrected_i$species_group %in% c("Cetaceans", "Whale shark"))) {
        # Cas 4 : pas RHN / MYS et code 1 / 2 : correction en code 4
        new_fate <- 4
      } else if (catch_to_be_corrected_i$fate_code == 3 && !(catch_to_be_corrected_i$species_group %in% c("Cetaceans", "Whale shark"))) {
        # Cas 5 : pas RHN / MYS et code 3 : correction en code 5
        new_fate <- 5
      } else if (catch_to_be_corrected_i$fate_code == 4 && catch_to_be_corrected_i$species_group %in% c("Cetaceans", "Whale shark")) {
        # Cas 6 : RHN / MYS et code 4 : correction en code 2
        new_fate <- 2
      } else if (catch_to_be_corrected_i$fate_code == 5 && catch_to_be_corrected_i$species_group %in% c("Cetaceans", "Whale shark")) {
        # Cas 7 : RHN / MYS et code 5 : correction en code 3
        new_fate <- 3
      }
      queries_catch[[ct_catch]] <- paste("UPDATE ps_observation.catch SET speciesfate = '",
                                         as.character(species_fate[species_fate$fate_code == new_fate, "fate_id"]),
                                         "', comment = concat(comment,'[Correction du devenir : ",
                                         catch_to_be_corrected_i$fate_code,
                                         " en ",
                                         new_fate,
                                         " - ",
                                         date_catch,
                                         " - ",
                                         corrector,
                                         "]')",
                                         ", topiaversion = topiaversion+1, lastupdatedate = '",
                                         format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
                                         "' WHERE topiaid = '",
                                         catch_to_be_corrected_i$catch_id,
                                         "' AND speciesfate = '",
                                         as.character(species_fate[species_fate$fate_code == catch_to_be_corrected_i$fate_code, "fate_id"]),
                                         "';",
                                         sep = ""
      )
    }
  }

  # Puis sur les tables sample et samplemeasure
  samples_to_be_corrected <- sample_fate_by_species_group_control %>%
    # On ne garde que ce qui peut être corrigé automatiquement
    dplyr::filter(
      # Cas 1a : bycatch et code 6
      (fate_code == 6 & species_group == "Other bony fishes") |
        # Cas 1b : thon mineur et code 6
        (fate_code == 6 & species_group == "Tunas nei" & fao_code %in% thon_mineur_sp) |
        # Cas 1c : billfish et code 6
        (fate_code == 6 & species_group == "Billfishes") |
        # Cas 2a : espèce sensible et code 6
        (fate_code == 6 & species_group %in% c("Sharks", "Turtles", "Rays")) |
        # Cas 2b : espèce sensible et code 15
        (fate_code == 15 & species_group %in% c("Sharks", "Turtles", "Rays")) |
        # Cas 3 : thon majeur et code 15
        (fate_code == 15 & species_group == "Tunas nei" & fao_code %in% thon_majeur_sp) |
        # Cas 4 : pas RHN / MYS et code 1 / 2
        (fate_code %in% c(1, 2) & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        # Cas 5 : pas RHN / MYS et code 3
        (fate_code == 3 & !(species_group %in% c("Cetaceans", "Whale shark"))) |
        # Cas 6 : RHN / MYS et code 4
        (fate_code == 4 & species_group %in% c("Cetaceans", "Whale shark")) |
        # Cas 7 : RHN / MYS et code 5
        (fate_code == 5 & species_group %in% c("Cetaceans", "Whale shark"))
    )
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
    queries_sample <- list(NULL)
    ct_sample <- 0
    timestamp_sample <- format(lubridate::now(), "%Y%m%d_%H%M%S")
    date_sample <- substr(timestamp_sample, 1, 8)
    set_id_list_sample <- unique(samples_to_be_corrected$set_id)
    for (i in seq_len(length(set_id_list_sample))) {
      sample_set_i <- samples_to_be_corrected %>%
        dplyr::filter(set_id == set_id_list_sample[i])
      sp_list_i <- unique(sample_set_i$fao_code)
      for (j in seq_len(length(sp_list_i))) {
        sample_set_i_sp_j <- sample_set_i %>%
          dplyr::filter(fao_code == sp_list_i[j])
        samplemeasure_id_set_i_sp_j <- unique(sample_set_i_sp_j$samplemeasure_id)
        for (k in seq_len(length(samplemeasure_id_set_i_sp_j))) {
          ct_sample <- ct_sample + 1
          new_fate <- NULL
          sample_set_i_sp_j_sm_k <- sample_set_i_sp_j %>%
            dplyr::filter(samplemeasure_id == samplemeasure_id_set_i_sp_j[k])
          if ((sample_set_i_sp_j_sm_k$fate_code == 6 && sample_set_i_sp_j_sm_k$species_group == "Other bony fishes") ||
              (sample_set_i_sp_j_sm_k$fate_code == 6 && sample_set_i_sp_j_sm_k$species_group == "Tunas nei" && sample_set_i_sp_j_sm_k$fao_code %in% thon_mineur_sp) ||
              (sample_set_i_sp_j_sm_k$fate_code == 6 && sample_set_i_sp_j_sm_k$species_group == "Billfishes")) {
            # Cas 1 : bycatch ou thon mineur ou billfish et code 6 : correction en code 15
            new_fate <- 15
          } else if ((sample_set_i_sp_j_sm_k$fate_code == 6 || sample_set_i_sp_j_sm_k$fate_code == 15) && sample_set_i_sp_j_sm_k$species_group %in% c("Sharks", "Turtles", "Rays")) {
            # Cas 2 : espèce sensible et code 6 ou code 15 : correction en code 16
            new_fate <- 16
          } else if (sample_set_i_sp_j_sm_k$fate_code == 15 && sample_set_i_sp_j_sm_k$species_group == "Tunas nei" && sample_set_i_sp_j_sm_k$fao_code %in% thon_majeur_sp) {
            # Cas 3 : thon majeur et code 15 : correction en code 6
            new_fate <- 6
          } else if (sample_set_i_sp_j_sm_k$fate_code %in% c(1, 2) && !(sample_set_i_sp_j_sm_k$species_group %in% c("Cetaceans", "Whale shark"))) {
            # Cas 4 : pas RHN / MYS et code 1 / 2 : correction en code 4
            new_fate <- 4
          } else if (sample_set_i_sp_j_sm_k$fate_code == 3 && !(sample_set_i_sp_j_sm_k$species_group %in% c("Cetaceans", "Whale shark"))) {
            # Cas 5 : pas RHN / MYS et code 3 : correction en code 5
            new_fate <- 5
          } else if (sample_set_i_sp_j_sm_k$fate_code == 4 && sample_set_i_sp_j_sm_k$species_group %in% c("Cetaceans", "Whale shark")) {
            # Cas 6 : RHN / MYS et code 4 : correction en code 2
            new_fate <- 2
          } else if (sample_set_i_sp_j_sm_k$fate_code == 5 && sample_set_i_sp_j_sm_k$species_group %in% c("Cetaceans", "Whale shark")) {
            # Cas 7 : RHN / MYS et code 5 : correction en code 3
            new_fate <- 3
          }
          queries_sample[[ct_sample]] <- paste(
            "UPDATE ps_observation.samplemeasure SET speciesfate = '",
            as.character(species_fate[species_fate$fate_code == new_fate, "fate_id"]),
            "', topiaversion = topiaversion+1, lastupdatedate = '",
            format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"),
            "' WHERE topiaid = '",
            sample_set_i_sp_j_sm_k$samplemeasure_id,
            "' AND speciesfate = '",
            as.character(species_fate[species_fate$fate_code == sample_set_i_sp_j_sm_k$fate_code, "fate_id"]),
            "';",
            sep = ""
          )
        }
        ct_sample <- ct_sample + 1
        queries_sample[[ct_sample]] <- paste("UPDATE ps_observation.sample SET comment = concat(comment,'[Correction devenir des echantillons ",
                                             sp_list_i[j],
                                             " - ",
                                             date_sample,
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
  }

  if (nrow(catch_to_be_corrected) + nrow(samples_to_be_corrected) > 0) {
    queries <- c(queries_catch, queries_sample)
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
    ## To be corrected catch, sample and samplemeasure are now corrected
    corrected_catch_topiaid <- to_be_corrected_catch_topiaid
    corrected_sample_topiaid <- to_be_corrected_sample_topiaid
    corrected_samplemeasure_topiaid <- to_be_corrected_samplemeasure_topiaid
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
    catch_updated_topiaid <- RPostgreSQL::dbGetQuery(
      con1,
      paste("SELECT * FROM ps_observation.catch WHERE topiaid in (",
            corrected_catch_topiaid,
            ");",
            sep = ""
      )
    )
    utils::View(catch_updated_topiaid)

    # 5 - Exportation of the final check ----
    ## Data extraction to see corrected catch by the topiaid
    observe_catch_corrected_sql <- paste(readLines(con = system.file("sql",
                                                                     "observe_catch.sql",
                                                                     package = "codama"
    )), collapse = "\n")
    ## Data extraction to see corrected samples by the topiaid
    observe_sample_corrected_sql <- paste(readLines(con = system.file("sql",
                                                                      "observe_sample.sql",
                                                                      package = "codama"
    )), collapse = "\n")
    # Correction of the sql queries
    observe_catch_corrected_sql <- sub(
      pattern = "extract(year from r.date) between (?start_year) and (?end_year)\nAND p.topiaid in (?program)\nAND o.label1 in (?ocean)\nAND co.iso3code in (?country_code)",
      replacement = paste0("c.topiaid in (", corrected_catch_topiaid, ")"),
      x = observe_catch_corrected_sql,
      fixed = TRUE
    )
    observe_sample_corrected_sql <- sub(
      pattern = "extract(year from r.date) between (?start_year) and (?end_year)\nAND p.topiaid in (?program)\nAND o.label1 in (?ocean)\nAND co.iso3code in (?country_code)",
      replacement = paste0("sm.topiaid in (", corrected_samplemeasure_topiaid, ")"),
      x = observe_sample_corrected_sql,
      fixed = TRUE
    )
    catch_corrected_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = observe_catch_corrected_sql
    ))
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
      openxlsx::write.xlsx(catch_corrected_data,
                           file = paste0(
                             path_file,
                             "/catch_fate_by_species_group_corrected_",
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
      openxlsx::write.xlsx(sample_corrected_data,
                           file = paste0(
                             path_file,
                             "/sample_fate_by_species_group_corrected_",
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
