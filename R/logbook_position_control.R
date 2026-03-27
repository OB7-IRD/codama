#' @name logbook_position_control
#' @title Gives the inconsistencies between the ocean declared for the trip and the position of the activity
#' @description The purpose of the logbook_position_control function is to provide a table of data that contains an inconsistency with ocean declaration and activity position
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_position_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_position_control () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Layer to containing the seas shapefile (example cf : cf : Flanders Marine Institute. IHO Sea Areas, version 1. Available online at https://www.marineregions.org/)
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param activity_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position activity
#' @param harbour_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position harbour
#' @param buffer_harbour {\link[base]{numeric}} expected. Default values: 11100. Buffer to be used for harbour, in meter
#' @param buffer_sea {\link[base]{numeric}} expected. Default values: 925. Buffer to be used for seas, in meter
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first without geographical location and the second with geographical location), a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  ocean_label}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  harbour_position_departure}}
#'  \item{\code{  harbour_position_landing}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  ID}}
#'  \item{\code{  geometry}}
#' }
#' @doctest
#' #Activity 1, 3 and 4 are ok,
#' #Activity 2 has different ocean,
#' #Activity 5 is outside ocean and harbour,
#' #Activity 6 has no position
#' #Activity 7 has no ocean label
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          ocean_label = c("ocean_1", "ocean_2", "ocean_2", "ocean_2", "ocean_2",
#'                                          "ocean_1", NA),
#'                          activity_position = c("POINT (0 0)", "POINT (0 0)", "POINT (-1 -1)",
#'                                                "POINT (1 1)", "POINT (3 3)", NA, "POINT (1 1)"),
#'                          trip_id = c("1", "2", "2", "2", "2", "2", "2"))
#' dataframe2 <- data.frame(trip_id = c("1", "2"),
#'                          harbour_position_departure = c("POINT (20 20)", "POINT (-1.05 -1)"),
#'                          harbour_position_landing = c("POINT (20 20)", "POINT (20 20)"))
#' dataframe3 <- sf::st_sf(data.frame(ID = c("ocean_1", "ocean_2"),
#'                                    geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(2,0),
#'                                                                                    c(2,2), c(0,2),
#'                                                                                    c(0,0)))),
#'                                                          sf::st_polygon(list(rbind(c(0,1), c(3,1),
#'                                                                                    c(3,2), c(0,2),
#'                                                                                    c(0,1)))),
#'                                                          crs = 4326)))
#' @expect equal(., list(structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE), type = c("Sea", "Sea", "Harbour", "Sea", "Excluding shapes oceans", "No position", "Sea"), ocean_label = c("ocean_1", "ocean_2", "ocean_2", "ocean_2", "ocean_2", "ocean_1", NA), ocean_calculate = c("ocean_1", "ocean_1", NA, "ocean_1 ocean_2", NA, NA, "ocean_1 ocean_2")), row.names = c(NA, 7L), class = "data.frame"), structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7"), activity_position = c("POINT (0 0)", "POINT (0 0)", "POINT (-1 -1)", "POINT (1 1)", "POINT (3 3)", NA, "POINT (1 1)"), logical_harbour = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE), logical_ocean = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), logical = c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE), type = c("Sea", "Sea", "Harbour", "Sea", "Excluding shapes oceans", "No position", "Sea"), ocean_label = c("ocean_1", "ocean_2", "ocean_2", "ocean_2", "ocean_2", "ocean_1", NA), ocean_calculate = c("ocean_1", "ocean_1", NA, "ocean_1 ocean_2", NA, NA, "ocean_1 ocean_2"), activity_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326)), row.names = c(NA, 7L), class = "data.frame")))
#' logbook_position_control(dataframe1, dataframe2, dataframe3, output = "report")
#' @export
logbook_position_control <- function(dataframe1,
                                     dataframe2,
                                     dataframe3,
                                     output,
                                     activity_crs = 4326,
                                     harbour_crs = 4326,
                                     buffer_harbour = 11100,
                                     buffer_sea = 925) {
  # 0 - Global variables assignement ----
  . <- NULL
  type <- NULL
  ocean_label <- NULL
  activity_position <- NULL
  harbour_position_departure <- NULL
  harbour_buffer_departure <- NULL
  harbour_position_landing <- NULL
  harbour_buffer_landing <- NULL
  logical_ocean <- NULL
  logical_harbour <- NULL
  logical_harbourdeparture <- NULL
  logical_harbourlanding <- NULL
  ocean_calculate <- NULL
  trip_id <- NULL
  activity_id <- NULL
  X <- NULL
  Y <- NULL
  logical_bounding <- NULL
  intersect_indice <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "ocean_label", "activity_position", "trip_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "ocean_label", "activity_position", "trip_id"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "ocean_label", "activity_position", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "harbour_position_departure", "harbour_position_landing"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "harbour_position_departure", "harbour_position_landing"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "harbour_position_departure", "harbour_position_landing")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("ID", "geometry"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("ID", "geometry"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("ID", "geometry")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = activity_crs,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = activity_crs,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = harbour_crs,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = harbour_crs,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = buffer_harbour,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = buffer_harbour,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = buffer_sea,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = buffer_sea,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(select)
  # 2 - Data design ----
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  # Indicates whether in land harbour
  if (nrow(dataframe1) > 0) {
    dataframe1$logical_harbour <- FALSE
  } else {
    dataframe1$logical_harbour <- logical()
  }
  # Formats spatial data activity
  data_geo_activity <- dataframe1 %>%
    dplyr::filter(!is.na(activity_position)) %>%
    sf::st_as_sf(., wkt = "activity_position", crs = activity_crs) %>%
    sf::st_transform(activity_position, crs = 4326)
  # Retrieves point coordinates
  if (nrow(data_geo_activity) > 0) {
    data_geo_activity <- data_geo_activity %>%
      dplyr::mutate(dplyr::as_tibble(sf::st_coordinates(.)))
  } else {
    data_geo_activity$X <- numeric()
    data_geo_activity$Y <- numeric()
  }
  # Checks whether the point is within the bounds of CRS 4326
  data_geo_activity <- data_geo_activity %>%
    dplyr::mutate(logical_bounding = (X >= -180 & X <= 180 & Y >= -90 & Y <= 90)) %>%
    dplyr::filter(logical_bounding)
  # If harbour departure position exists
  if (any(!is.na(dataframe1$activity_position) & !is.na(dataframe1$harbour_position_departure))) {
    # Formats spatial data harbourdeparture, add buffer in meter
    data_geo_harbourdeparture <- dataframe1 %>%
      dplyr::filter(!is.na(activity_position) & !is.na(harbour_position_departure)) %>%
      dplyr::select(harbour_position_departure) %>%
      dplyr::distinct() %>%
      dplyr::mutate(harbour_buffer_departure = harbour_position_departure) %>%
      sf::st_as_sf(., wkt = "harbour_buffer_departure", crs = harbour_crs) %>%
      sf::st_transform(harbour_buffer_departure, crs = 4326) %>%
      terra::vect() %>%
      terra::buffer(width = buffer_harbour) %>%
      sf::st_as_sf()
    # Calculates the intersection between activity and harbourdeparture
    data_geo_activity_harbourdeparture <- data_geo_activity %>% dplyr::filter(!is.na(harbour_position_departure))
    intersect_harbourdeparture <- sapply(seq_len(nrow(data_geo_harbourdeparture)), function(x) lengths(sf::st_intersects(data_geo_activity_harbourdeparture[data_geo_activity_harbourdeparture$harbour_position_departure == data_geo_harbourdeparture$harbour_position_departure[x], 1], data_geo_harbourdeparture[x, 1])))
    intersect_harbourdeparture_index <- sapply(seq_len(nrow(data_geo_harbourdeparture)), function(x) which(data_geo_activity_harbourdeparture$harbour_position_departure == data_geo_harbourdeparture$harbour_position_departure[x]))
    data_geo_activity_harbourdeparture$nb_port_intersect <- 0
    data_geo_activity_harbourdeparture$nb_port_intersect[unlist(intersect_harbourdeparture_index)] <- unlist(intersect_harbourdeparture)
    data_geo_activity_harbourdeparture$logical_harbourdeparture <- data_geo_activity_harbourdeparture$nb_port_intersect != 0
    # Logical harbourdeparture
    dataframe1 <- dplyr::left_join(dataframe1, data.frame(data_geo_activity_harbourdeparture)[, c("activity_id", "logical_harbourdeparture")], by = dplyr::join_by(activity_id))
    dataframe1$logical_harbourdeparture[is.na(dataframe1$logical_harbourdeparture)] <- FALSE
  }else {
    if (nrow(dataframe1) > 0) {
      dataframe1$logical_harbourdeparture <- FALSE
    } else {
      dataframe1$logical_harbourdeparture <- logical()
    }
  }
  # If harbour landing position exists
  if (any(!is.na(dataframe1$activity_position) & !is.na(dataframe1$harbour_position_landing))) {
    # Formats spatial data harbourlanding, add buffer in meter
    data_geo_harbourlanding <- dataframe1 %>%
      dplyr::filter(!is.na(activity_position) & !is.na(harbour_position_landing)) %>%
      dplyr::select(harbour_position_landing) %>%
      dplyr::distinct() %>%
      dplyr::mutate(harbour_buffer_landing = harbour_position_landing) %>%
      sf::st_as_sf(., wkt = "harbour_buffer_landing", crs = harbour_crs) %>%
      sf::st_transform(harbour_buffer_landing, crs = 4326) %>%
      terra::vect() %>%
      terra::buffer(width = buffer_harbour) %>%
      sf::st_as_sf()
    # Calculates the intersection between activity and harbourlanding
    data_geo_activity_harbourlanding <- data_geo_activity %>% dplyr::filter(!is.na(harbour_position_landing))
    intersect_harbourlanding <- sapply(seq_len(nrow(data_geo_harbourlanding)), function(x) lengths(sf::st_intersects(data_geo_activity_harbourlanding[data_geo_activity_harbourlanding$harbour_position_landing == data_geo_harbourlanding$harbour_position_landing[x], 1], data_geo_harbourlanding[x, 1])))
    intersect_harbourlanding_index <- sapply(seq_len(nrow(data_geo_harbourlanding)), function(x) which(data_geo_activity_harbourlanding$harbour_position_landing == data_geo_harbourlanding$harbour_position_landing[x]))
    data_geo_activity_harbourlanding$nb_port_intersect <- 0
    data_geo_activity_harbourlanding$nb_port_intersect[unlist(intersect_harbourlanding_index)] <- unlist(intersect_harbourlanding)
    data_geo_activity_harbourlanding$logical_harbourlanding <- data_geo_activity_harbourlanding$nb_port_intersect != 0
    # Logical in harbourlanding
    dataframe1 <- dplyr::left_join(dataframe1, data.frame(data_geo_activity_harbourlanding)[, c("activity_id", "logical_harbourlanding")], by = dplyr::join_by(activity_id))
    dataframe1$logical_harbourlanding[is.na(dataframe1$logical_harbourlanding)] <- FALSE
  }else {
    if (nrow(dataframe1) > 0) {
      dataframe1$logical_harbourlanding <- FALSE
    } else {
      dataframe1$logical_harbourlanding <- logical()
    }
  }
  # Logical in harbour
  dataframe1$logical_harbour <- dataframe1$logical_harbour | dataframe1$logical_harbourdeparture | dataframe1$logical_harbourlanding
  # Buffer sea in degrees
  shape_sea_buffer <- dataframe3 %>%
    terra::vect() %>%
    terra::buffer(width = buffer_sea) %>%
    sf::st_as_sf()
  # Calculates the intersection between activity and sea and logical in sea, indicates whether the ocean is the same
  intersect <- data_geo_activity %>%
    dplyr::mutate(intersect_indice = sf::st_intersects(data_geo_activity, shape_sea_buffer)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(logical_ocean = ocean_label %in% shape_sea_buffer$ID[unlist(intersect_indice)],
                  ocean_calculate = ifelse(length(shape_sea_buffer$ID[unlist(intersect_indice)]) > 0, paste0(shape_sea_buffer$ID[unlist(intersect_indice)], collapse = " "), NA)) %>%
    dplyr::ungroup()
  dataframe1 <- dplyr::left_join(dataframe1, data.frame(intersect)[, c("activity_id", "logical_ocean", "ocean_calculate")], by = dplyr::join_by(activity_id))
  dataframe1$logical_ocean[is.na(dataframe1$logical_ocean)] <- FALSE
  # Case of harbour in sea : not in harbour
  dataframe1$logical_harbour[!is.na(dataframe1$ocean_calculate)] <- FALSE
  dataframe1$logical <- dataframe1$logical_ocean | dataframe1$logical_harbour
  # Gives the type of location
  if (nrow(dataframe1) > 0) {
    dataframe1$type <- "Sea"
  } else {
    dataframe1$type <- character()
  }
  dataframe1$type[is.na(dataframe1$ocean_calculate)] <- "Excluding shapes oceans"
  dataframe1$type[is.na(dataframe1$activity_position)] <- "No position"
  dataframe1$type[dataframe1$logical_harbour] <- "Harbour"
  dataframe1 <- dplyr::relocate(.data = dataframe1, type, ocean_label, ocean_calculate, .after = logical)
  dataframe1 <- subset(dataframe1, select = -c(trip_id, harbour_position_departure, harbour_position_landing, logical_harbourdeparture, logical_harbourlanding))
  activity_sea_land_data_detail <- dataframe1 %>%
    dplyr::mutate(activity_crs = activity_crs)
  dataframe1 <- subset(dataframe1, select = -c(activity_position, logical_ocean, logical_harbour))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$activity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$activity_id[is.na(dataframe1$logical)], collapse = ", "))
    }
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      text,
      sep = ""
    )
  }
  # 3 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with position is inconsistent, outside the ocean declared for the trip")))
  }
  if (output == "report") {
    return(list(dataframe1, activity_sea_land_data_detail))
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
