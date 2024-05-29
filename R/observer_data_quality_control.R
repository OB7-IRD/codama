#' @name observer_data_quality_control
#' @title observer data quality control
#' @description 'This script aims to generate indicators for evaluating the quality of data collected by observers'
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the size_distribution_and_outliers_control
#' @param start_year {\link[base]{integer}} expected. Starting year for the samples to control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the samples to control.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234"
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on wich control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param observer_last_name {\link[base]{character}} expected. Observer first name we want to control in priority. Examples: 'Martin', 'Durand'...etc.
#' @param path_file {\link[base]{character}} expected. Path to save the final xlsx.
#' @return The function return histograms and xlsx tables.
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr tibble group_by summarise filter arrange rename select left_join desc
#' @importFrom ggplot2 ggplot aes geom_histogram geom_vline scale_x_continuous scale_color_manual labs theme_bw theme element_text element_blank ggsave
#' @importFrom lubridate now
#' @importFrom reshape untable
#' @importFrom stats quantile
#' @importFrom stringr str_detect
#' @importFrom openxlsx write.xlsx
observer_data_quality_control <- function(data_connection,
                                          start_year,
                                          end_year,
                                          program,
                                          ocean,
                                          country_code,
                                          observer_last_name,
                                          path_file = NULL) {
  # 0 - Global variables assignment ----
  trip <- NULL
  # 1 - Arguments verification ----
  # 2 - Data extraction ----
  observe_catch_sql <- paste(
    readLines(con = system.file("sql",
                                "observe_catch.sql",
                                package = "codama"
    )),
    collapse = "\n"
  )
  observe_sample_sql <- paste(
    readLines(con = system.file("sql",
                                "observe_sample.sql",
                                package = "codama"
    )),
    collapse = "\n"
  )
  observe_activity_sql <- paste(
    readLines(con = system.file("sql",
                                "observe_activity.sql",
                                package = "codama"
    )),
    collapse = "\n"
  )
  observe_trip_sql <- paste(
    readLines(con = system.file("sql",
                                "observe_trip.sql",
                                package = "codama"
    )),
    collapse = "\n"
  )
  ## Correction of the sql query if ocean or country code not selected
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
    observe_activity_sql <- sub(
      pattern = "AND o.label1 in (?ocean)",
      replacement = "AND o.label1 like (?ocean)",
      x = observe_activity_sql,
      fixed = TRUE
    )
    observe_trip_sql <- sub(
      pattern = "AND o.label1 in (?ocean)",
      replacement = "AND o.label1 like (?ocean)",
      x = observe_trip_sql,
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
    observe_activity_sql <- sub(
      pattern = "AND co.iso3code in (?country_code)",
      replacement = "AND co.iso3code like (?country_code)",
      x = observe_activity_sql,
      fixed = TRUE
    )
    observe_trip_sql <- sub(
      pattern = "AND co.iso3code in (?country_code)",
      replacement = "AND co.iso3code like (?country_code)",
      x = observe_trip_sql,
      fixed = TRUE
    )
  }
  observe_catch_sql_final <- DBI::sqlInterpolate(
    conn = data_connection[[2]],
    sql = observe_catch_sql,
    start_year = paste0(start_year, collapse = ", "),
    end_year = paste0(end_year, collapse = ", "),
    program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
    ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
    country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
  )
  observe_sample_sql_final <- DBI::sqlInterpolate(
    conn = data_connection[[2]],
    sql = observe_sample_sql,
    start_year = paste0(start_year, collapse = ", "),
    end_year = paste0(end_year, collapse = ", "),
    program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
    ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
    country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
  )
  observe_activity_sql_final <- DBI::sqlInterpolate(
    conn = data_connection[[2]],
    sql = observe_activity_sql,
    start_year = paste0(start_year, collapse = ", "),
    end_year = paste0(end_year, collapse = ", "),
    program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
    ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
    country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
  )
  observe_trip_sql_final <- DBI::sqlInterpolate(
    conn = data_connection[[2]],
    sql = observe_trip_sql,
    start_year = paste0(start_year, collapse = ", "),
    end_year = paste0(end_year, collapse = ", "),
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
  activity <- dplyr::tibble(DBI::dbGetQuery(
    conn = data_connection[[2]],
    statement = observe_activity_sql_final
  ))
  trip <- dplyr::tibble(DBI::dbGetQuery(
    conn = data_connection[[2]],
    statement = observe_trip_sql_final
  ))
  # 3 - Data design -----
  if (is.null(trip)) {
    stop("The 'trip' object was not created successfully.")
  } else {
    message("The 'trip' object was created successfully.")
  }
  observer_example <- "Bourasseau"
  observer_all <- c(observer_last_name, observer_example)
  if (!("%" %in% observer_last_name)){
    catch <- catch %>%
      dplyr::filter(observer %in% observer_all)
    sample <- sample %>%
      dplyr::filter(observer %in% observer_all)
    activity <- activity %>%
      dplyr::filter(observer %in% observer_all)
    trip <- trip %>%
      dplyr::filter(observer %in% observer_all)
  }
  set <- activity %>%
    dplyr::filter(vessel_activity_code == 6)

  ## Number of species detected by set ----
  summarise_catch<- catch %>%
    dplyr::group_by(ocean,trip_id, observer, fao_code, school_type) %>%
    dplyr::summarise(indiv = sum(count))
  summarise_catch$sp_count <- 1
  summarise_catch<- summarise_catch %>%
    dplyr::group_by(ocean,trip_id, observer, school_type) %>%
    dplyr::summarise(sp_count = sum(sp_count))

  summarise_set <- set %>%
    dplyr::distinct(observation_date, observation_time, .keep_all = TRUE)
  summarise_set$set_count <- 1
  summarise_set <- summarise_set %>%
    dplyr::group_by(ocean, trip_id, observer, school_type) %>%
    dplyr::summarise(set_count=sum(set_count))

  summarise_set_catch <- merge(summarise_catch, summarise_set, by = c("ocean","trip_id", "observer", "school_type"), all.x = TRUE, all.y = TRUE)
  timestamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")

  summarise_set_catch$observer <- factor(summarise_set_catch$observer,
                                         levels = c(observer_last_name, observer_example))
  if (ocean != "%"){
    plot_sp_set <- ggplot2::ggplot(summarise_set_catch, ggplot2::aes(set_count, sp_count, group=observer)) +
      ggplot2::geom_point(ggplot2::aes(color=observer), alpha=0.4, size = 3) +
      ggplot2::facet_grid(. ~ school_type) +
      ggplot2::scale_fill_viridis_d() +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Number of set", y = "Number of species",
                    title = "Number of species as a function of the number of set according to the observer by school type") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::geom_smooth(ggplot2::aes(color=observer), method="lm",se=FALSE)

    ggplot2::ggsave(plot_sp_set,
                    file = paste0(
                      path_file,
                      "/sp_count_vs_set_count",
                      "/sp_count_vs_set_count_",
                      start_year,
                      "-",
                      end_year,
                      "_",
                      ocean,
                      "_",
                      timestamp,
                      ".PNG"
                    ),
                    width = 10,
                    height = 6
    )
  } else {
    plot_sp_set_ocean <- ggplot2::ggplot(summarise_set_catch, ggplot2::aes(set_count, sp_count, group=observer)) +
      ggplot2::geom_point(ggplot2::aes(color=observer), alpha=0.4, size = 3) +
      ggplot2::facet_grid(school_type ~ ocean) +
      ggplot2::theme_bw() +
      ggplot2::scale_color_viridis_d()+
      ggplot2::labs(x = "Number of set", y = "Number of species",
                    title = "Number of species as a function of the number of set according to the observer by ocean and school type") +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::geom_smooth(ggplot2::aes(color=observer), method="lm",se=FALSE)+
      ggpubr::stat_regline_equation(ggplot2::aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~~"),
                                                 color=observer), size=3)

    ggplot2::ggsave(plot_sp_set_ocean,
                    file = paste0(
                      path_file,
                      "/sp_count_vs_set_count",
                      "/sp_count_vs_set_count_",
                      start_year,
                      "-",
                      end_year,
                      "_",
                      ocean,
                      "_",
                      timestamp,
                      ".PNG"
                    ),
                    width = 10,
                    height = 6)

  }

  ## Percentage of trip without tuna discards (mainly for Indian ocean) ----
  if (!("%" %in% observer_last_name)){
    catch <- catch %>%
      dplyr::filter(observer %in% observer_last_name)
    sample <- sample %>%
      dplyr::filter(observer %in% observer_last_name)
    activity <- activity %>%
      dplyr::filter(observer %in% observer_last_name)
    trip <- trip %>%
      dplyr::filter(observer %in% observer_last_name)
    set <- set %>%
      dplyr::filter(observer %in% observer_last_name)
  }

  tuna_discards <- catch %>%
    dplyr::filter(species_group == "Tunas nei" & fate_code %in% c(4,5))
  trip_with_tuna_discards <- as.character(tuna_discards$trip_id)

  trip_discards <- set %>%
    dplyr::group_by(ocean, year, trip_id, observer, school_type) %>%
    dplyr::mutate(tuna_discards_check = ifelse(trip_id %in% trip_with_tuna_discards,"Yes","No"))


  trip_discards_summary <- trip_discards %>%
    dplyr::group_by(observer, year) %>%
    dplyr::summarise(total_trips = dplyr::n(),
                     percentage_no = mean(tuna_discards_check == "No") * 100)


  plot_tuna_discards <- ggplot2::ggplot(trip_discards_summary, ggplot2::aes(x = observer, y = percentage_no, fill = factor(year))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Observer Name", y = "Percentage of trips without discards", fill = "Year") +
    ggplot2::theme_minimal()

  ggplot2::ggsave(plot_tuna_discards,
                  file = paste0(
                    path_file,
                    "/trip_without_tuna_disacards",
                    "/trip_without_tuna_disacards_",
                    start_year,
                    "-",
                    end_year,
                    "_",
                    ocean,
                    "_",
                    timestamp,
                    ".PNG"
                  ),
                  width = 10,
                  height = 6)
}

## Percentage of trips without furtif FAD's




## Percentage of trips without photos
trip_no_photo <- trip %>%
  dplyr::mutate(
    photos = dplyr::if_else(
      is.na(observations_comment) | !stringr::str_detect(observations_comment, stringr::regex("pas de photo\\w?", ignore_case = TRUE)),
      "Yes",
      "No"))

trip_no_photo_summary <- trip_no_photo %>%
  dplyr::group_by(observer, year) %>%
  dplyr::summarise(total_trips = dplyr::n(),
                   percentage_no = mean(photos == "No") * 100)

ggplot2::ggplot(trip_no_photo_summary, ggplot2::aes(x = observer, y = percentage_no, fill = factor(year))) +
  ggplot2::geom_bar(stat = "identity", position = "dodge") +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::labs(x = "Observer Name", y = "Percentage of trips without photos", fill = "Year") +
  ggplot2::theme_minimal()

