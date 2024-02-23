#' @name size distribution and outliers control
#' @title size_distribution_and_outliers_control
#' @description 'This script aims to visualise length distribtuion by species, year and ocean. It also gives for each species outliers lengths'
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the size_distribution_and_outliers_control
#' @param start_year {\link[base]{integer}} expected. Starting year for the samples to control.
#' @param end_year {\link[base]{integer}} expected. Ending year for the samples to control.
#' @param start_year_stat {\link[base]{integer}} expected. Starting year to calculate the mean median and quantiles.
#' @param end_year_stat {\link[base]{integer}} expected. Ending year to calculate the mean median and quantiles.
#' @param start_year_catch_vs_sample {\link[base]{integer}} expected. Starting year to compare sample to catch size distribution.
#' @param end_year_catch_vs_sample {\link[base]{integer}} expected. Ending year to compare sample to catch size distribution.
#' @param program {\link[base]{character}} expected. Programs to be controlled. Example of the format for a program topiaid: "fr.ird.referential.ps.common.Program#1239832686262#0.31033946454061234"
#' @param ocean {\link[base]{character}} expected. Ocean to be controlled. Examples: 'Indian', 'Atlantic'...etc.
#' @param country_code {\link[base]{character}} expected. Countries on wich control will be made. Examples: 'FRA', 'MUS'...etc.
#' @param type {\link[base]{character}} expected. type of graph you want: count or frequency.
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
size_distribution_and_outliers_control <- function(data_connection,
                                                   start_year,
                                                   end_year,
                                                   start_year_stat,
                                                   end_year_stat,
                                                   start_year_catch_vs_sample,
                                                   end_year_catch_vs_sample,
                                                   program,
                                                   ocean,
                                                   country_code,
                                                   type,
                                                   path_file = NULL) {
  # 0 - Global variables assignment ----
  is_number_computed <- NULL
  information_source <- NULL
  mean_individual_length_cm <- NULL
  length_was_computed <- NULL
  count <- NULL
  fao_code <- NULL
  size_type <- NULL
  min_length <- NULL
  year <- NULL
  species_group <- NULL
  density <- NULL
  sample_date <- NULL
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
    r_object = start_year_stat,
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
    r_object = end_year_stat,
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
    r_object = start_year_catch_vs_sample,
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
    r_object = end_year_catch_vs_sample,
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
  if (r_type_checking(
    r_object = type,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = type,
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
    observe_species_sql <- paste(
      readLines(con = system.file("sql",
                                  "observe_species.sql",
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
      start_year = paste0(start_year_stat, collapse = ", "),
      end_year = paste0(end_year_stat, collapse = ", "),
      program = DBI::SQL(paste0("'", paste0(program, collapse = "', '"), "'")),
      ocean = DBI::SQL(paste0("'", paste0(ocean, collapse = "', '"), "'")),
      country_code = DBI::SQL(paste0("'", paste0(country_code, collapse = "', '"), "'"))
    )
    observe_sample_sql_final <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = observe_sample_sql,
      start_year = paste0(start_year_stat, collapse = ", "),
      end_year = paste0(end_year_stat, collapse = ", "),
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
    species <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = observe_species_sql
    ))
  }
  # 3 - Data design ----
  ## Delete lengths that are not true length measure (not observed)
  size_catch <- catch %>%
    dplyr::filter(is_number_computed == "observed") %>%
    # dplyr::filter(is_mean_individual_weight_kg_computed == "observed") %>%
    dplyr::filter(information_source == "Observer") %>%
    dplyr::filter(!is.na(mean_individual_length_cm)) %>%
    dplyr::rename(ocean_catch = ocean)

  size_sample <- sample %>%
    dplyr::filter(length_was_computed == "FALSE") %>%
    dplyr::rename(ocean_sample = ocean) %>%
    dplyr::filter(!is.na(length))


  ## Identification of both data frames by a new column (usefull to do the ggplot after)
  size_catch$source <- "captured"
  size_sample$source <- "sampled"

  ## Merge both data frames
  colnames(size_catch) <- colnames(size_sample)
  size_sample_catch <- rbind(size_catch, size_sample) # Add it after

  ## Filter count!=NA for tunas (they are in weight)
  size_sample <- size_sample %>% dplyr::filter(count != "NA")
  size_sample_catch <- size_sample_catch %>% dplyr::filter(count != "NA")
  ## Duplicate the rows by the number of counts
  size_sample <- reshape::untable(size_sample, size_sample$count)
  size_sample_catch <- reshape::untable(size_sample_catch, size_sample_catch$count)
  ## Now one row is an individual
  size_sample$count <- 1
  size_sample_catch$count <- 1

  ## Add a data frame to calculate quantiles, median and mean for each faocode (qmm_min_max = quantiles, mean, median, min and max)
  qmm <- size_sample %>%
    dplyr::group_by(fao_code, size_type) %>%
    dplyr::summarize(
      lower = stats::quantile(length, probs = .05),
      upper = stats::quantile(length, probs = .95),
      mean = mean(length),
      median = median(length),
    )
  ## Delete duplicated species
  species <- species[!duplicated(species$fao_code), ]
  ## Add min max from db for the concerned species in qmm
  qmm_min_max <- dplyr::left_join(qmm, species, by = c("fao_code"))
  species_no_min_max <- qmm_min_max %>% dplyr::filter(is.na(min_length))
  ## Show species that don"t have min and max
  species_no_min_max <- qmm_min_max %>%
    dplyr::filter(is.na(min_length))
  list_species_no_min_max <- paste(as.character(species_no_min_max$fao_code),
                                   collapse = ", "
  )
  print(paste(
    "species with no min and max in data base are:",
    list_species_no_min_max
  ))
  ## Filter the year we are interested in
  size_sample$sample_date <- ""
  size_sample <- size_sample %>%
    dplyr::mutate(sample_date = ifelse(year >= start_year & year <= end_year,
                                       paste0(start_year, " to ", end_year),
                                       paste0(start_year_stat, " to ", end_year_stat)
    )) %>%
    dplyr::filter(species_group != "Tunas nei")
  size_sample_catch <- size_sample_catch %>%
    dplyr::filter(year >= start_year_catch_vs_sample & year <= end_year_catch_vs_sample)
  ## List of species (faocode) and list of length types
  lg_list <- sort(unique(size_sample$size_type))
  sp_list <- sort(unique(size_sample$fao_code))

  # 4 - Sample vs catch distributions ----
  for (sp in sp_list) {
    ## Filter the samples and the min max median quantiles by species
    sp <- as.character(sp)
    data_sp <- size_sample %>% dplyr::filter(fao_code == sp)
    data_sp_sample_catch <- size_sample_catch %>%
      dplyr::filter(fao_code == sp)
    qmm_min_max_sp <- qmm_min_max %>% dplyr::filter(fao_code == sp)

    mean_sample_catch <- data_sp_sample_catch %>%
      dplyr::group_by(source) %>%
      dplyr::summarize(mean = mean(length), .groups = "drop")
    mean_sample <- round(mean_sample_catch$mean[mean_sample_catch$source == "sampled"])
    mean_catch <- round(mean_sample_catch$mean[mean_sample_catch$source == "captured"])
    min <- min(data_sp_sample_catch$length)
    max <- max(data_sp_sample_catch$length)

    title <- paste0(sp, " | ", ocean, " | ", start_year_catch_vs_sample, "-", end_year_catch_vs_sample)

    counts <- data_sp_sample_catch %>%
      dplyr::group_by(length) %>%
      dplyr::summarize(count = sum(count), .groups = "drop")

    mid_counts <- (max(counts$count)) / 2
    mid_freq <- ((max(counts$count)) / sum(counts$count)) / 2

    if ("captured" %in% data_sp_sample_catch$source) {
      if (type == "count") {
        ## Plot sample vs catch count
        plot_sample_catch_counts <- ggplot2::ggplot(data_sp_sample_catch, ggplot2::aes(x = length)) +
          ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count), fill = source),
                                  position = "identity",
                                  alpha = 0.5,
                                  binwidth = 1,
                                  boundary = 0,
                                  closed = "left"
          ) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = mean_sample), color = "blue") +
          ggplot2::geom_vline(ggplot2::aes(xintercept = mean_catch), color = "red") +
          ggplot2::annotate(x = mean_catch, y = (mid_counts) * 0.5, label = paste("Mean_c =", mean_catch), vjust = 1, geom = "label", color = "blue", size = 3) +
          ggplot2::annotate(x = mean_sample, y = (mid_counts) * 0.25, label = paste("Mean_s =", mean_sample), vjust = 1, geom = "label", color = "red", size = 3) +
          ggplot2::scale_x_continuous(breaks = seq((10 * floor(min / 10)), max, 10)) +
          ggplot2::scale_fill_manual(
            values = c("#000000", "#CCCCCC"),
            name = "Source :"
          ) +
          ggplot2::labs(title = title, x = paste0(" Length (cm)")) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(color = "black", linewidth = 2),
            plot.title = ggplot2::element_text(size = 15, hjust = 0.5, face = "bold"),
            legend.position = "bottom",
            panel.grid.minor = ggplot2::element_blank()
          )
        ## Export plot sample vs catch counts
        ggplot2::ggsave(plot_sample_catch_counts,
                        file = paste0(
                          path_file,
                          "/length_dist_sample_catch",
                          "/length_dist_sample_catch_",
                          country_code,
                          "_",
                          ocean,
                          "_",
                          start_year_catch_vs_sample,
                          "-",
                          end_year_catch_vs_sample,
                          "_",
                          ocean,
                          "/count",
                          "/length_dist_sample_catch_",
                          sp,
                          "_",
                          start_year_catch_vs_sample,
                          "-",
                          end_year_catch_vs_sample,
                          "_",
                          ocean,
                          ".PNG"
                        ),
                        width = 10,
                        height = 6
        )
      }

      if (type == "frequency") {
        ## Plot sample vs catch frequency
        plot_sample_catch_frequency <- ggplot2::ggplot(data_sp_sample_catch, ggplot2::aes(x = length)) +
          ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density), fill = source),
                                  position = "identity",
                                  alpha = 0.5,
                                  binwidth = 1,
                                  boundary = 0,
                                  closed = "left"
          ) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = mean_sample), color = "blue") +
          ggplot2::geom_vline(ggplot2::aes(xintercept = mean_catch), color = "red") +
          ggplot2::annotate(x = mean_catch, y = (mid_freq) * 0.5, label = paste("Mean_c =", mean_catch), vjust = 1, geom = "label", color = "blue", size = 3) +
          ggplot2::annotate(x = mean_sample, y = (mid_freq) * 0.25, label = paste("Mean_s =", mean_sample), vjust = 1, geom = "label", color = "red", size = 3) +
          ggplot2::scale_x_continuous(breaks = seq((10 * floor(min / 10)), max, 10)) +
          ggplot2::scale_fill_manual(
            values = c("#000000", "#CCCCCC"),
            name = "Source :"
          ) +
          ggplot2::labs(title = title, x = paste0(" Length (cm)")) +
          ggplot2::theme_bw() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(color = "black", linewidth = 2),
            plot.title = ggplot2::element_text(size = 15, hjust = 0.5, face = "bold"),
            legend.position = "bottom",
            panel.grid.minor = ggplot2::element_blank()
          )

        ## Export plot sample vs catch frequency
        ggplot2::ggsave(plot_sample_catch_frequency,
                        file = paste0(
                          path_file,
                          "/length_dist_sample_catch",
                          "/length_dist_sample_catch_",
                          country_code,
                          "_",
                          ocean,
                          "_",
                          start_year_catch_vs_sample,
                          "-",
                          end_year_catch_vs_sample,
                          "_",
                          ocean,
                          "/freq",
                          "/length_dist_sample_catch_",
                          sp,
                          "_",
                          start_year,
                          "-",
                          end_year,
                          "_",
                          ocean,
                          ".PNG"
                        ),
                        width = 10,
                        height = 6
        )
      }
    }
    # 5 - Sample distributions ----
    for (lg in lg_list) {
      ## Then filter the samples and the min max median quantiles by length type
      data_lg <- data_sp %>%
        dplyr::filter(size_type == lg)
      qmm_min_max_lg <- qmm_min_max_sp %>%
        dplyr::filter(size_type == lg)

      ## Condition to avoid a saved graph if there is no sample for this species and this length type
      if (any(stringr::str_detect(data_lg$sample_date, paste0(start_year, " to ", end_year))) & length(data_lg$fao_code) != 0) {
        ## Condition to avoid plotting a min and max in the graph if we don't have the information
        if (!is.na(qmm_min_max_lg$min_length) == TRUE) {
          ## Attribute a mean/median/quantiles value from qmm_min_max data frame
          mean <- as.numeric(round(qmm_min_max_lg$mean[1]))
          median <- as.numeric(round(qmm_min_max_lg$median[1]))
          lower <- as.numeric(round(qmm_min_max_lg$lower[1]))
          upper <- as.numeric(round(qmm_min_max_lg$upper[1]))
          min <- as.numeric(round(qmm_min_max_lg$min_length[1]))
          max <- as.numeric(round(qmm_min_max_lg$max_length[1]))

          title <- paste0(sp, " | ", lg, " | ", ocean)

          counts <- data_lg %>%
            dplyr::group_by(fao_code, length) %>%
            dplyr::summarize(count = sum(count), .groups = "drop")

          mid_counts <- (max(counts$count)) / 2
          mid_freq <- ((max(counts$count)) / sum(counts$count)) / 2

          data_lg$sample_date <- factor(data_lg$sample_date,
                                        levels = c(
                                          paste0(start_year, " to ", end_year),
                                          paste0(start_year_stat, " to ", end_year_stat)
                                        )
          )
          ## Plot size distribution in count
          if (type == "count") {
            plot_size_dist_count <- ggplot2::ggplot(data_lg, ggplot2::aes(x = length)) +
              ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(count), fill = sample_date),
                                      position = "identity",
                                      alpha = 0.5,
                                      binwidth = 1,
                                      boundary = 0,
                                      closed = "left"
              ) +
              ggplot2::geom_vline(ggplot2::aes(xintercept = min), color = "black") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = max), color = "black") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = upper), color = "red") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = lower), color = "red") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = mean), color = "green") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = median), color = "blue") +
              ggplot2::annotate(x = min, y = +Inf, label = paste("Min =", min), vjust = 1, geom = "label", size = 3) +
              ggplot2::annotate(x = max, y = +Inf, label = paste("Max =", max), vjust = 1, geom = "label", size = 3) +
              ggplot2::annotate(x = upper, y = mid_counts, label = expression(Q[0.95]), vjust = 1, geom = "label", color = "red", size = 3) +
              ggplot2::annotate(x = lower, y = mid_counts, label = expression(Q[0.05]), vjust = 1, geom = "label", color = "red", size = 3) +
              ggplot2::annotate(x = mean, y = (mid_counts) * 0.5, label = paste("Mean =", mean), vjust = 1, geom = "label", color = "green", size = 3) +
              ggplot2::annotate(x = median, y = (mid_counts) * 0.25, label = paste("Median =", median), vjust = 1, geom = "label", color = "blue", size = 3) +
              ggplot2::scale_x_continuous(breaks = seq((10 * floor(min / 10)), max, 10)) +
              ggplot2::scale_fill_manual(
                values = c("#000000", "#CCCCCC"),
                name = "Samples from :"
              ) +
              ggplot2::labs(title = title, x = paste0(lg, " (cm)")) +
              ggplot2::theme_bw() +
              ggplot2::theme(
                plot.background = ggplot2::element_rect(color = "black", linewidth = 2),
                plot.title = ggplot2::element_text(size = 15, hjust = 0.5, face = "bold"),
                legend.position = "bottom",
                panel.grid.minor = ggplot2::element_blank()
              )
            ### Export plot size distribution in count
            ggplot2::ggsave(plot_size_dist_count,
                            file = paste0(
                              path_file,
                              "/length_dist_by_species",
                              "/length_dist_by_species_",
                              country_code,
                              "_",
                              ocean,
                              "_",
                              start_year,
                              "-",
                              end_year,
                              "_",
                              ocean,
                              "/count",
                              "/length_dist_",
                              sp,
                              "_",
                              lg,
                              "_",
                              start_year,
                              "-",
                              end_year,
                              "_",
                              ocean,
                              ".PNG"
                            ),
                            width = 10,
                            height = 6
            )
          }
          if (type == "frequency") {
            ## Plot size distribution in frequency
            plot_size_dist_freqeuncy <- ggplot2::ggplot(data_lg, ggplot2::aes(x = length)) +
              ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density), fill = sample_date),
                                      position = "identity",
                                      alpha = 0.5,
                                      binwidth = 1,
                                      boundary = 0,
                                      closed = "left"
              ) +
              ggplot2::geom_vline(ggplot2::aes(xintercept = min), color = "black") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = max), color = "black") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = upper), color = "red") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = lower), color = "red") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = mean), color = "green") +
              ggplot2::geom_vline(ggplot2::aes(xintercept = median), color = "blue") +
              ggplot2::annotate(x = min, y = +Inf, label = paste("Min =", min), vjust = 1, geom = "label", size = 3) +
              ggplot2::annotate(x = max, y = +Inf, label = paste("Max =", max), vjust = 1, geom = "label", size = 3) +
              ggplot2::annotate(x = upper, y = mid_freq, label = expression(Q[0.95]), vjust = 1, geom = "label", color = "red", size = 3) +
              ggplot2::annotate(x = lower, y = mid_freq, label = expression(Q[0.05]), vjust = 1, geom = "label", color = "red", size = 3) +
              ggplot2::annotate(x = mean, y = (mid_freq) * 0.5, label = paste("Mean =", mean), vjust = 1, geom = "label", color = "green", size = 3) +
              ggplot2::annotate(x = median, y = (mid_freq) * 0.25, label = paste("Median =", median), vjust = 1, geom = "label", color = "blue", size = 3) +
              ggplot2::scale_x_continuous(breaks = seq((10 * floor(min / 10)), max, 10)) +
              ggplot2::scale_fill_manual(
                values = c("#000000", "#CCCCCC"),
                name = "Samples from :"
              ) +
              ggplot2::labs(title = title, x = paste0(lg, " (cm)")) +
              ggplot2::theme_bw() +
              ggplot2::theme(
                plot.background = ggplot2::element_rect(color = "black", linewidth = 2),
                plot.title = ggplot2::element_text(size = 15, hjust = 0.5, face = "bold"),
                legend.position = "bottom",
                panel.grid.minor = ggplot2::element_blank()
              )
            ### Export plot size distribution in frequency
            ggplot2::ggsave(plot_size_dist_freqeuncy,
                            file = paste0(
                              path_file,
                              "/length_dist_by_species",
                              "/length_dist_by_species_",
                              country_code,
                              "_",
                              ocean,
                              "_",
                              start_year,
                              "-",
                              end_year,
                              "_",
                              ocean,
                              "/freq",
                              "/length_dist_",
                              sp,
                              "_",
                              lg,
                              "_",
                              start_year,
                              "-",
                              end_year,
                              "_",
                              ocean,
                              ".PNG"
                            ),
                            width = 10,
                            height = 6
            )
          }
        }
      }
      # 6 - Outliers ----
      outliers_sp_lg <- data_lg %>%
        dplyr::filter(length <= qmm_min_max_lg$lower | length >= qmm_min_max_lg$lower)
      timestamp <- format(lubridate::now(), "%Y%m%d_%H%M%S")
      ### Fold creation for all the outliers between the two selected years (start_year and end_year)
      folder_outliers_all <- paste0(
        path_file,
        "/outliers",
        "/outliers_",
        start_year,
        "-",
        end_year,
        "_",
        ocean
      )
      if (file.exists(folder_outliers_all) == FALSE) {
        dir.create(folder_outliers_all)
      }

      ### Fold creation for outliers by sp and lg
      folder_outliers_by_sp_lg <- paste0(
        path_file,
        "/outliers",
        "/outliers_",
        country_code,
        "_",
        ocean,
        "_",
        start_year,
        "-",
        end_year,
        "_",
        ocean
      )
      if (file.exists(folder_outliers_by_sp_lg) == FALSE & nrow(outliers_sp_lg) != 0) {
        dir.create(folder_outliers_by_sp_lg)
      }
      ### Write the xlsx file into the corresponding folder
      outliers_sp_lg <- outliers_sp_lg %>%
        dplyr::arrange(length, desc(length))
      if (nrow(outliers_sp_lg) != 0) {
        openxlsx::write.xlsx(as.data.frame(outliers_sp_lg),
                             file = paste0(
                               path_file,
                               "/outliers",
                               "/outliers_",
                               country_code,
                               "_",
                               ocean,
                               "_",
                               start_year,
                               "-",
                               end_year,
                               "_",
                               ocean,
                               "/outliers_",
                               sp,
                               "_",
                               lg,
                               "_",
                               country_code,
                               "_",
                               ocean,
                               "_",
                               start_year,
                               "-",
                               end_year,
                               "_",
                               ocean,
                               "_",
                               timestamp,
                               ".xlsx"
                             ),
                             rowNames = FALSE,
                             append = TRUE
        )
      }
    }
  }
}
