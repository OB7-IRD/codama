#' @name logbook_eez_control
#' @title Gives the inconsistencies between the fishing area declared and calculated for the activity
#' @description The purpose of the logbook_eez_control function is to provide a table of data that contains an inconsistency between the fishing area declared and calculated with position for the activity fishing
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_eez_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Layer to containing the eez shapefile (example cf : Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632)
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @param activity_crs {\link[base]{numeric}} expected. Default values: 4326. Coordinate Reference Systems for the position activity
#' @param international_waters_code {\link[base]{character}} expected. Default values: "XIN". iso3 code corresponding to international waters
#' @param vessel_activity {\link[base]{character}} expected. Default values: c("6"). Vector of inventory of codes for activities that must have a zee zone
#' @return The function returns a {\link[base]{character}} with output is "message", two {\link[base]{data.frame}} with output is "report" (the first without geographical location and the second with geographical location), a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  activity_id}}
#'  \item{\code{  vesselactivity_code}}
#'  \item{\code{  fpazone_code}}
#'  \item{\code{  fpazone_country_iso3}}
#'  \item{\code{  activity_position}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  ISO_TER1}}
#'  \item{\code{  ISO_TER2}}
#'  \item{\code{  ISO_TER3}}
#'  \item{\code{  geometry}}
#' }
#' @doctest
#' #Activity 1, 2, 3 and 5 are ok,
#' #Activity 4 is outside the EEZ delimited in the shapefile,
#' #Activity 6 has a missing EEZ zone and the vessel's activity is in vessel_activity,
#' #Activity 7 has different EEZ,
#' #Activity 8 has an EEZ zone unknown to the shapefile and which also does not exist in
#' #           international_waters_code
#' dataframe1 <- data.frame(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"),
#'                          vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"),
#'                          fpazone_code = c("SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"),
#'                          fpazone_country_iso3 = c("SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"),
#'                          activity_position = c("POINT (1 1)", "POINT (4 3)", "POINT (-1 -1)",
#'                                                "POINT (-1 -1)", "POINT (1 1)", "POINT (1 1)",
#'                                                "POINT (1 1)", "POINT (6 6)"))
#' dataframe2 <- sf::st_sf(data.frame(ISO_TER1 = c("SYC", "XSG"),
#'                                    ISO_TER2 = c(NA, NA),
#'                                    ISO_TER3 = c(NA, NA),
#'                                    geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(2,0),
#'                                                                                    c(2,2), c(0,2),
#'                                                                                    c(0,0)))),
#'                                                          sf::st_polygon(list(rbind(c(3,3), c(3,5),
#'                                                                                    c(5,5), c(5,3),
#'                                                                                    c(3,3)))),
#'                                                          crs = 4326)))
#' @expect equal(., list(structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"), vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"), fpazone_code = c("SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"), fpazone_country_iso3 = c("SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"), eez_calculated = c("SYC", "XSG", NA, "On land or in international waters", NA, NA, "SYC", "On land or in international waters"), logical = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)), row.names = c(NA, 8L), class = "data.frame"), structure(list(activity_id = c("1", "2", "3", "4", "5", "6", "7", "8"), vesselactivity_code = c("6", "6", "6", "6", "1", "6", "6", "6"), fpazone_code = c("SYC", "XSG", "XIN", "SYC", NA, NA, "AZE", "AZE"), fpazone_country_iso3 = c("SYC", "XXX", "XIN", "SYC", NA, NA, "AZE", "AZE"), activity_position = c("POINT (1 1)", "POINT (4 3)", "POINT (-1 -1)", "POINT (-1 -1)", "POINT (1 1)", "POINT (1 1)", "POINT (1 1)", "POINT (6 6)"), eez_calculated = c("SYC", "XSG", NA, "On land or in international waters", NA, NA, "SYC", "On land or in international waters"), logical = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE), activity_crs = c(4326, 4326, 4326, 4326, 4326, 4326, 4326, 4326)), row.names = c(NA, 8L), class = "data.frame")))
#' logbook_eez_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_eez_control <- function(dataframe1,
                                dataframe2,
                                output,
                                activity_crs = 4326,
                                international_waters_code = "XIN",
                                vessel_activity = c("6")) {
  # 0 - Global variables assignement ----
  activity_position <- NULL
  activity_id <- NULL
  fpazone_code <- NULL
  fpazone_country_iso3 <- NULL
  ISO_TER1 <- NULL
  ISO_TER2 <- NULL
  ISO_TER3 <- NULL
  logical_eez <- NULL
  filter_activity_geo <- NULL
  eez_calculated <- NULL
  filter_international_waters <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("activity_id", "vesselactivity_code", "fpazone_code", "fpazone_country_iso3", "activity_position"),
    column_type = c("character", "character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("activity_id", "vesselactivity_code", "fpazone_code", "fpazone_country_iso3", "activity_position"),
      column_type = c("character", "character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("activity_id", "vesselactivity_code", "fpazone_code", "fpazone_country_iso3", "activity_position")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("ISO_TER1", "ISO_TER2", "ISO_TER3", "geometry"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("ISO_TER1", "ISO_TER2", "ISO_TER3", "geometry"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("ISO_TER1", "ISO_TER2", "ISO_TER3", "geometry")]
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
    r_object = international_waters_code,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = international_waters_code,
      type = "character",
      output = "error"
    ))
  }
  if (!codama::r_type_checking(
    r_object = vessel_activity,
    type = "character",
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = vessel_activity,
      type = "character",
      output = "error"
    ))
  }
  select <- dataframe1$activity_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Checks for the presence of a declared fishing zone
  dataframe1$logical <- !c(is.na(dataframe1$fpazone_code) & is.na(dataframe1$fpazone_country_iso3))
  # Formats spatial data activity
  dataframe1$filter_activity_geo <- !is.na(dataframe1$activity_position) & !c(is.na(dataframe1$fpazone_code) & is.na(dataframe1$fpazone_country_iso3))
  data_geo_activity <- dataframe1 %>%
    dplyr::filter(filter_activity_geo) %>%
    sf::st_as_sf(wkt = "activity_position", crs = activity_crs) %>%
    sf::st_transform(activity_position, crs = 4326)
  # Calculates the intersection between activity and eez
  if (nrow(data_geo_activity) > 0) {
    intersect_eez <- terra::intersect(data_geo_activity %>%
                                        terra::vect(), dataframe2 %>%
                                        terra::vect()) %>%
      sf::st_as_sf() %>%
      sf::st_drop_geometry()
    # Compares declared country with calculated country
    intersect_eez <- intersect_eez %>%
      dplyr::group_by(activity_id) %>%
      dplyr::summarise(logical_eez = any(c(fpazone_code, fpazone_country_iso3) %in% c(ISO_TER1, ISO_TER2, ISO_TER3)),
                       eez_calculated = paste0(unique(c(ISO_TER1, ISO_TER2, ISO_TER3))[!is.na(unique(c(ISO_TER1, ISO_TER2, ISO_TER3)))], collapse = ", "))
    # Merge
    dataframe1 <- dplyr::left_join(dataframe1, intersect_eez, by = dplyr::join_by(activity_id))
  }else {
    if (nrow(dataframe1) > 0) {
      dataframe1$logical_eez <- NA
      dataframe1$eez_calculated <- NA
    } else {
      dataframe1$logical_eez <- logical()
      dataframe1$eez_calculated <- character()
    }
  }
  # Case of international waters : the calculated country must be missing
  dataframe1$filter_international_waters <- ((dataframe1$fpazone_code %in% international_waters_code & dataframe1$fpazone_country_iso3 %in% international_waters_code) | (is.na(dataframe1$fpazone_code) & dataframe1$fpazone_country_iso3 %in% international_waters_code) | (dataframe1$fpazone_code %in% international_waters_code & is.na(dataframe1$fpazone_country_iso3)))
  dataframe1[is.na(dataframe1$logical_eez) & dataframe1$filter_international_waters, "logical_eez"] <- TRUE
  # Case of no calculated country
  dataframe1[is.na(dataframe1$logical_eez), "logical_eez"] <- FALSE
  dataframe1$logical <- dataframe1$logical & dataframe1$logical_eez
  dataframe1[is.na(dataframe1$eez_calculated) & dataframe1$filter_activity_geo & !dataframe1$filter_international_waters, "eez_calculated"] <- "On land or in international waters"
  # Case of vessel_activity with no constraints if missing declared fishing zone
  dataframe1$logical[!(dataframe1$vesselactivity_code %in% vessel_activity) & is.na(dataframe1$fpazone_code) & is.na(dataframe1$fpazone_country_iso3)] <- TRUE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- dplyr::relocate(.data = dataframe1, eez_calculated, .before = logical)
  dataframe1 <- subset(dataframe1, select = -c(logical_eez, filter_activity_geo, filter_international_waters))
  activity_data_detail <- dataframe1
  activity_data_detail <- activity_data_detail %>%
    dplyr::mutate(activity_crs = activity_crs)
  dataframe1 <- subset(dataframe1, select = -c(activity_position))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " activity with declared eez that do not correspond to calculated eez")))
  }
  if (output == "report") {
    return(list(dataframe1, activity_data_detail))
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
