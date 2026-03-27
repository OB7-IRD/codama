#' @name logbook_anapo_control
#' @title Gives the inconsistencies activity position and VMS position
#' @description The purpose of the logbook_anapo_control function is to provide a table of data that contains an inconsistency between activity position and VMS position
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_anapo_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_anapo_control () function.
#' @param dataframe3 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_anapo_control () function.
#' @param activity_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position activity
#' @param harbour_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position harbour
#' @param vms_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position VMS
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param threshold_geographical {\link[base]{numeric}} expected. Default values: 10. Maximum valid distance threshold (Nautical miles) between position and nearest VMS point.
#' @param threshold_time {\link[base]{numeric}} expected. Default values: 7200000. Maximum valid distance threshold (milliseconds) between position and VMS point.
#' @param threshold_score {\link[base]{numeric}} expected. Default values: 0.5. Minimum valid score between position and VMS point.
#' @param buffer_harbour {\link[base]{numeric}} expected. Default values: 11100. Buffer to be used for harbour, in meter
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  activity_date}}
#'  \item{\code{  activity_time}}
#'  \item{\code{  activity_position}}
#'  \item{\code{  trip_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  trip_id}}
#'  \item{\code{  vessel_code}}
#'  \item{\code{  harbour_position_departure}}
#'  \item{\code{  harbour_position_landing}}
#' }
#' \itemize{
#' Dataframe 3:
#'  \item{\code{  vms_date}}
#'  \item{\code{  vms_time}}
#'  \item{\code{  vms_position}}
#'  \item{\code{  vessel_code}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first without geographical location and the second with geographical location), a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Activity 1, 2 and 3 are ok,
#' #Activity 4 has a geographical distance above the threshold (threshold_geographical) and
#' #           a score below the threshold (threshold_score),
#' #Activity 5 has no position,
#' #Activity 6 has has a geographical distance above the threshold (threshold_geographical) and
#' #           a score below the threshold (threshold_score)
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6"),
#'                          activity_date = as.Date(c("2020/01/01", "2020/01/12", "2020/01/12",
#'                                                    "2020/01/13", "2020/01/12", "2020/01/12")),
#'                          activity_time = c("05:26:01", "10:41:15", "16:41:15", "03:12:34",
#'                                            "05:56:12", "23:26:47"),
#'                          activity_position = c("POINT (1 1)", "POINT (0 0)", "POINT (3 0)",
#'                                                "POINT (4 4)", NA, "POINT (3 0.6)"),
#'                          trip_id = c("1", "2", "2", "2", "2", "2"))
#' dataframe2 <- data.frame(trip_id = c("1", "2"),
#'                          vessel_code = c("1", "1"),
#'                          harbour_position_departure = c("POINT (1 1.1)", "POINT (3 3)"),
#'                          harbour_position_landing = c("POINT (3 3)", "POINT (3 3)"))
#' dataframe3 <- data.frame(vms_date = as.Date(c("2020/01/01", "2020/01/12", "2020/01/12")),
#'                          vms_time = c("15:26:01", "10:55:15", "22:32:17"),
#'                          vms_position = c("POINT (4 4)", "POINT (0 0.1)", "POINT (3 0.3)"),
#'                          vessel_code = c("1", "1", "1"))
#' @expect equal(., list(structure(list(activity_id = c("1", "2", "3", "4", "5", "6"), logical = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), nb_vms = c(1L, 2L, 2L, NA, 2L, 2L), min_distance = structure(c(NA, 6.004055139173, 18.012165417519, 230.106883933216, NA, 18.012165417519), units = structure(list(numerator = "NM", denominator = character(0)), class = "symbolic_units"), class = "units"), max_score = c(NA, NA, 2.17959673670807, 0, NA, 0.209441144555651)), row.names = c(NA, 6L), class = "data.frame"), structure(list(activity_id = c("3", "3", "4", "4", "6", "6", "1", "2", "2", "5", "5"), activity_date = structure(c(18273, 18273, 18274, 18274, 18273, 18273, 18262, 18273, 18273, 18273, 18273), class = "Date"), activity_time = c("16:41:15", "16:41:15", "03:12:34", "03:12:34", "23:26:47", "23:26:47", "05:26:01", "10:41:15", "10:41:15", "05:56:12", "05:56:12"), activity_position = c("POINT (3 0)", "POINT (3 0)", "POINT (4 4)", "POINT (4 4)", "POINT (3 0.6)", "POINT (3 0.6)", "POINT (1 1)", "POINT (0 0)", "POINT (0 0)", NA, NA), vms_date = structure(c(18273, 18273, 18273, 18273, 18273, 18273, 18262, 18273, 18273, 18273, 18273), class = "Date"), vms_time = c("10:55:15", "22:32:17", "10:55:15", "22:32:17", "10:55:15", "22:32:17", "15:26:01", "10:55:15", "22:32:17", "10:55:15", "22:32:17"), vms_position = c("POINT (0 0.1)", "POINT (3 0.3)", "POINT (0 0.1)", "POINT (3 0.3)", "POINT (0 0.1)", "POINT (3 0.3)", "POINT (4 4)", "POINT (0 0.1)", "POINT (3 0.3)", "POINT (0 0.1)", "POINT (3 0.3)"), distance = structure(c(180.221602566745, 18.012165417519, 335.278629168604, 230.106883933216, 182.602328607533, 18.012165417519, NA, 6.004055139173, 181.019203021658, NA, NA), units = structure(list(numerator = "NM", denominator = character(0)), class = "symbolic_units"), class = "units"), duration = structure(c(20760000, -21062000, -27761000, -69583000, 45092000, 3270000, NA, NA, NA, NA, NA), units = structure(list(numerator = "ms", denominator = character(0)), class = "symbolic_units"), class = "units"), score = c(0, 2.17959673670807, 0, 0, 0, 0.209441144555651, NA, NA, NA, NA, NA), vms_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326), activity_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326)), row.names = c(NA, -11L), class = "data.frame")))
#' logbook_anapo_control(dataframe1,dataframe2, dataframe3, output = "report")
#' @export
logbook_anapo_control <- function(dataframe1,
                                  dataframe2,
                                  dataframe3,
                                  activity_crs = 4326,
                                  harbour_crs = 4326,
                                  vms_crs = 4326,
                                  output,
                                  threshold_geographical = 10,
                                  threshold_time = 7200000,
                                  threshold_score = 0.5,
                                  buffer_harbour = 11100) {
  # 0 - Global variables assignement ----
  . <- NULL
  trip_id <- NULL
  harbour_position_departure <- NULL
  harbour_buffer_departure <- NULL
  harbour_position_landing <- NULL
  harbour_buffer_landing <- NULL
  logical_harbourdeparture <- NULL
  logical_harbourlanding <- NULL
  vms_date <- NULL
  vessel_code <- NULL
  nb_vms <- NULL
  activity_position_geometry <- NULL
  vms_position_geometry <- NULL
  activity_id <- NULL
  distance <- NULL
  min_distance <- NULL
  activity_time_bis <- NULL
  vms_time <- NULL
  activity_date_time <- NULL
  vms_date_time <- NULL
  duration <- NULL
  score <- NULL
  nb_vms_bis <- NULL
  activity_date <- NULL
  activity_time <- NULL
  activity_position <- NULL
  date_group <- NULL
  vms_position <- NULL
  NM <- NULL
  ms <- NULL
  X <- NULL
  Y <- NULL
  logical_bounding <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "activity_date", "activity_time", "activity_position", "trip_id"),
    column_type = c("character", "Date", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "activity_date", "activity_time", "activity_position", "trip_id"),
      column_type = c("character", "Date", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "activity_date", "activity_time", "activity_position", "trip_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("trip_id", "vessel_code", "harbour_position_departure", "harbour_position_landing"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("trip_id", "vessel_code", "harbour_position_departure", "harbour_position_landing"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("trip_id", "vessel_code", "harbour_position_departure", "harbour_position_landing")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe3,
    type = "data.frame",
    column_name = c("vms_date", "vms_time", "vms_position", "vessel_code"),
    column_type = c("Date", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe3,
      type = "data.frame",
      column_name = c("vms_date", "vms_time", "vms_position", "vessel_code"),
      column_type = c("Date", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe3 <- dataframe3[, c("vms_date", "vms_time", "vms_position", "vessel_code")]
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
    r_object = vms_crs,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vms_crs,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
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
    r_object = threshold_geographical,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_geographical,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_time,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_time,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = threshold_score,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = threshold_score,
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
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Indicates activity whether in harbour
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(trip_id))
  # Indicates whether in land harbour
  if (nrow(dataframe1) > 0) {
    dataframe1$logical <- FALSE
  } else {
    dataframe1$logical <- logical()
  }
  # Formats spatial data activity
  dataframe_activity_geo <- dataframe1 %>%
    dplyr::filter(!is.na(activity_position)) %>%
    sf::st_as_sf(wkt = "activity_position", crs = activity_crs, remove = FALSE) %>%
    sf::st_transform(activity_position, crs = 4326)
  sf::st_geometry(dataframe_activity_geo) <- "activity_position_geometry"
  # Retrieves point coordinates
  if (nrow(dataframe_activity_geo) > 0) {
    dataframe_activity_geo <- dataframe_activity_geo %>%
      dplyr::mutate(dplyr::as_tibble(sf::st_coordinates(.)))
  } else {
    dataframe_activity_geo$X <- numeric()
    dataframe_activity_geo$Y <- numeric()
  }
  # Checks whether the point is within the bounds of CRS 4326
  dataframe_activity_geo <- dataframe_activity_geo %>%
    dplyr::mutate(logical_bounding = (X >= -180 & X <= 180 & Y >= -90 & Y <= 90))
  dataframe_activity_geo <- dataframe_activity_geo %>%
    dplyr::filter(logical_bounding)
  # If harbour departure position exists
  if (any(!is.na(dataframe1$activity_position) & !is.na(dataframe1$harbour_position_departure))) {
    # Formats spatial data harbourdeparture, add buffer in meter
    data_geo_harbourdeparture <- dataframe1 %>%
      dplyr::filter(!is.na(activity_position) & !is.na(harbour_position_departure)) %>%
      dplyr::select(harbour_position_departure) %>%
      dplyr::distinct() %>%
      dplyr::mutate(harbour_buffer_departure = harbour_position_departure) %>%
      sf::st_as_sf(wkt = "harbour_buffer_departure", crs = harbour_crs) %>%
      sf::st_transform(harbour_buffer_departure, crs = 4326) %>%
      terra::vect() %>%
      terra::buffer(width = buffer_harbour) %>%
      sf::st_as_sf()
    # Calculates the intersection between activity and harbourdeparture
    dataframe_activity_geo_harbourdeparture <- dataframe_activity_geo %>% dplyr::filter(!is.na(harbour_position_departure))
    intersect_harbourdeparture <- sapply(seq_len(nrow(data_geo_harbourdeparture)), function(x) lengths(sf::st_intersects(dataframe_activity_geo_harbourdeparture[dataframe_activity_geo_harbourdeparture$harbour_position_departure == data_geo_harbourdeparture$harbour_position_departure[x], 1], data_geo_harbourdeparture[x, 1])))
    intersect_harbourdeparture_index <- sapply(seq_len(nrow(data_geo_harbourdeparture)), function(x) which(dataframe_activity_geo_harbourdeparture$harbour_position_departure == data_geo_harbourdeparture$harbour_position_departure[x]))
    dataframe_activity_geo_harbourdeparture$nb_port_intersect <- 0
    dataframe_activity_geo_harbourdeparture$nb_port_intersect[unlist(intersect_harbourdeparture_index)] <- unlist(intersect_harbourdeparture)
    dataframe_activity_geo_harbourdeparture$logical_harbourdeparture <- dataframe_activity_geo_harbourdeparture$nb_port_intersect != 0
    # Logical harbourdeparture
    dataframe1 <- dplyr::left_join(dataframe1, data.frame(dataframe_activity_geo_harbourdeparture)[, c("activity_id", "logical_harbourdeparture")], by = dplyr::join_by(activity_id))
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
      sf::st_as_sf(wkt = "harbour_buffer_landing", crs = harbour_crs) %>%
      sf::st_transform(harbour_buffer_landing, crs = 4326) %>%
      terra::vect() %>%
      terra::buffer(width = buffer_harbour) %>%
      sf::st_as_sf()
    # Calculates the intersection between activity and harbourlanding
    dataframe_activity_geo_harbourlanding <- dataframe_activity_geo %>% dplyr::filter(!is.na(harbour_position_landing))
    intersect_harbourlanding <- sapply(seq_len(nrow(data_geo_harbourlanding)), function(x) lengths(sf::st_intersects(dataframe_activity_geo_harbourlanding[dataframe_activity_geo_harbourlanding$harbour_position_landing == data_geo_harbourlanding$harbour_position_landing[x], 1], data_geo_harbourlanding[x, 1])))
    intersect_harbourlanding_index <- sapply(seq_len(nrow(data_geo_harbourlanding)), function(x) which(dataframe_activity_geo_harbourlanding$harbour_position_landing == data_geo_harbourlanding$harbour_position_landing[x]))
    dataframe_activity_geo_harbourlanding$nb_port_intersect <- 0
    dataframe_activity_geo_harbourlanding$nb_port_intersect[unlist(intersect_harbourlanding_index)] <- unlist(intersect_harbourlanding)
    dataframe_activity_geo_harbourlanding$logical_harbourlanding <- dataframe_activity_geo_harbourlanding$nb_port_intersect != 0
    # Logical in harbourlanding
    dataframe1 <- dplyr::left_join(dataframe1, data.frame(dataframe_activity_geo_harbourlanding)[, c("activity_id", "logical_harbourlanding")], by = dplyr::join_by(activity_id))
    dataframe1$logical_harbourlanding[is.na(dataframe1$logical_harbourlanding)] <- FALSE
  }else {
    if (nrow(dataframe1) > 0) {
      dataframe1$logical_harbourlanding <- FALSE
    } else {
      dataframe1$logical_harbourlanding <- logical()
    }
  }
  # Logical in harbour
  dataframe1$logical <- dataframe1$logical | dataframe1$logical_harbourdeparture | dataframe1$logical_harbourlanding
  # Remove VMS without position
  dataframe3 <- dataframe3 %>% dplyr::filter(!is.na(vms_position))
  # Calculation number vms
  dataframe3_nb_vms <- dataframe3 %>%
    dplyr::group_by(vms_date, vessel_code) %>%
    dplyr::summarise(nb_vms = dplyr::n(), .groups = "drop")
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe3_nb_vms, by = dplyr::join_by(activity_date == vms_date, vessel_code == vessel_code))
  # Case of NA nb_vms
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      nb_vms_bis = dplyr::coalesce(nb_vms, 0),
    )
  # Retrieves VMS positions for the previous, current and next day
  dataframe3$date_group <- dataframe3$vms_date
  dataframe3_prior <- dataframe3 %>% dplyr::mutate(date_group = vms_date - 1)
  dataframe3_post <- dataframe3 %>% dplyr::mutate(date_group = vms_date + 1)
  dataframe3 <- dplyr::bind_rows(dataframe3, dataframe3_prior, dataframe3_post) %>%
    dplyr::group_by(date_group, vms_position) %>%
    dplyr::distinct()
  dataframe3 <- dplyr::inner_join(dataframe1[, c("activity_id", "activity_date", "activity_time", "vessel_code", "activity_position", "logical")], dataframe3, by = dplyr::join_by(activity_date == date_group, vessel_code == vessel_code), relationship = "many-to-many")
  # Formats spatial data vms
  dataframe_vms_geo <- dataframe3 %>%
    dplyr::filter(!logical & !is.na(activity_position)) %>%
    dplyr::select(vms_position) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(wkt = "vms_position", crs = vms_crs, remove = FALSE) %>%
    sf::st_transform(vms_position, crs = 4326)
  sf::st_geometry(dataframe_vms_geo) <- "vms_position_geometry"
  # Select unique pair vms/activity
  dataframe_activity_geo <- dataframe_activity_geo %>%
    dplyr::select(activity_position, activity_position_geometry) %>%
    dplyr::distinct()
  pair_position <- dataframe3 %>%
    dplyr::filter(!logical & !is.na(activity_position)) %>%
    dplyr::select(vms_position, activity_position) %>%
    dplyr::distinct() %>%
    dplyr::mutate(pair_position = paste0(activity_position, "_", vms_position))
  pair_position <- dplyr::left_join(pair_position, dataframe_vms_geo, by = dplyr::join_by(vms_position))
  pair_position <- dplyr::left_join(pair_position, dataframe_activity_geo, by = dplyr::join_by(activity_position))
  # Calculation of the minimum distance between the activity and the nearest day's VMS in nautical mile
  # Define nautical miles
  units::install_unit("NM", "1852 m", "Nautical mile")
  threshold_geographical <- units::set_units(threshold_geographical, NM)
  pair_position <- pair_position %>%
    dplyr::mutate(distance = sf::st_distance(x = activity_position_geometry, y = vms_position_geometry, by_element = TRUE))
  if (nrow(pair_position) > 0) {
    units(pair_position$distance) <- units::make_units(NM)
  }else {
    units(pair_position$distance) <- units::drop_units(pair_position$distance)
    units(pair_position$distance) <- units::make_units(NM)
  }
  # Remove formats spatial data
  pair_position <- pair_position %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-c(activity_position_geometry, vms_position_geometry))
  dataframe3 <- dplyr::left_join(dataframe3, pair_position[, c("distance", "vms_position", "activity_position")], by = dplyr::join_by(vms_position, activity_position))
  rm(pair_position)
  dataframe3 <- dataframe3 %>%
    dplyr::group_by(activity_id) %>%
    dplyr::mutate(min_distance = ifelse(length(distance) > 0, min(distance), Inf)) %>%
    dplyr::ungroup()
  units(dataframe3$min_distance) <- units::make_units(NM)
  dataframe_calcul_min <- dataframe3 %>%
    dplyr::select(activity_id, min_distance) %>%
    dplyr::distinct()
  dataframe1 <- dplyr::left_join(dataframe1, dataframe_calcul_min, by = dplyr::join_by(activity_id))
  # Check if distance between activity and nearest VMS point below threshold
  if (nrow(dataframe1) > 0) {
    dataframe1[!is.na(dataframe1$min_distance) & dataframe1$min_distance < threshold_geographical, "logical"] <- TRUE
  } else {
    dataframe1[!is.na(dataframe1$min_distance) & units::drop_units(dataframe1$min_distance) < units::drop_units(threshold_geographical), "logical"] <- logical()
  }
  dataframe_calcul <- dataframe3 %>%
    dplyr::filter(!logical & !is.na(activity_position)) %>%
    subset(select = -c(logical)) %>%
    dplyr::filter(units::drop_units(min_distance) >= units::drop_units(threshold_geographical))
  # Gives a temporary hour for activities that are missing an hour
  dataframe_calcul$activity_time_bis <- dataframe_calcul$activity_time
  dataframe_calcul[is.na(dataframe_calcul$activity_time), "activity_time_bis"] <- "00:00:00"
  # Calculates time between activity and VMS point in milliseconds
  dataframe_calcul <- dataframe_calcul %>%
    dplyr::mutate(activity_date_time = as.POSIXct(paste(vms_date, activity_time_bis)), vms_date_time = as.POSIXct(paste(vms_date, vms_time)))
  units::install_unit("ms", "1000 secs", "Milliseconds")
  dataframe_calcul <- dataframe_calcul %>%
    dplyr::mutate(duration = units::set_units(as.numeric(difftime(activity_date_time, vms_date_time, units = "secs") * 1000), ms))
  # Gives a duration for activities that are missing an hour
  dataframe_calcul[is.na(dataframe_calcul$activity_time), "duration"] <- units::set_units(1, ms)
  # Score calculation
  dataframe_calcul <- dataframe_calcul %>%
    dplyr::mutate(score = (2^(units::drop_units(-distance / threshold_geographical))) * (2^(-units::drop_units(duration) / threshold_time)))
  dataframe_calcul[units::drop_units(dataframe_calcul$distance) > units::drop_units(threshold_geographical * 2), "score"] <- 0
  dataframe_calcul[as.numeric(dataframe_calcul$duration) > threshold_time * 2, "score"] <- 0
  dataframe_score_max <- dataframe_calcul %>%
    dplyr::group_by(activity_id) %>%
    dplyr::summarise(max_score = ifelse(length(score) > 0, max(score), -Inf))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe_score_max, by = dplyr::join_by(activity_id))
  # Check the maximum score between activity and VMS
  dataframe1[!is.na(dataframe1$max_score) & dataframe1$max_score >= threshold_score, "logical"] <- TRUE
  # Recovers all activity positions for the detailed table
  # Data with calcul VMS
  dataframe_detail <- dplyr::bind_rows(dataframe_calcul, dplyr::anti_join(subset(dataframe3, select = -c(logical)), dataframe_calcul, by = c("activity_id", "activity_date", "vms_date", "vessel_code", "vms_time", "vms_position", "activity_time", "activity_position")))
  dataframe_detail <- dplyr::bind_rows(dataframe_detail, dplyr::anti_join(dataframe1[, c("activity_id", "activity_date", "activity_time", "vessel_code", "activity_position")], dataframe3, by = c("activity_date" = "activity_date", "vessel_code" = "vessel_code")))
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(trip_id, harbour_position_departure, harbour_position_landing, logical_harbourdeparture, logical_harbourlanding, nb_vms_bis, activity_date, vessel_code, activity_time, activity_position))
  dataframe_detail <- subset(dataframe_detail, select = -c(vessel_code, min_distance, activity_time_bis, activity_date_time, vms_date_time))
  dataframe_detail <- dataframe_detail %>%
    dplyr::mutate(vms_crs = vms_crs, activity_crs = activity_crs) %>%
    data.frame()
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with missing VMS", collapse = ", ")))
  }
  if (output == "report") {
    return(list(dataframe1, dataframe_detail))
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
