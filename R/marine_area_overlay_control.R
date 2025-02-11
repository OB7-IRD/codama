#' @name marine_area_overlay_control
#' @title Marine area overlay control function
#' @description Control processes associated to the function {\link[furdeb]{marine_area_overlay}} (consistent spatial marine area overlay for points, grids and polygons).
#' @param data_longitude {\link[base]{character}} expected. Vector of longitude values.
#' @param data_latitude {\link[base]{character}} expected. Vector of latitude values.
#' @param overlay_expected {\link[base]{character}} expected. Type of overlay output. You can choose between "fao_area", "eez_area" or "ices_area".
#' @param area_file_path {\link[base]{character}} expected. File path of the area shape. You can provide a .shp or a .RData file.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @examples
#' \dontrun{
#' marine_area_overlay_control(data_longitude = as.character(c(1.09, 8.37, 19.35, -4.04, -4.54)),
#'                             data_latitude = as.character(c(2.47, -3.19, 24.55, 5.22, 49.73)),
#'                             overlay_expected = "ices_area",
#'                             area_file_path = "ICES_Statistical_Rectangles_Eco.RData",
#'                             output = "report")
#' }
#' @export
marine_area_overlay_control <- function(data_longitude,
                                        data_latitude,
                                        overlay_expected,
                                        area_file_path,
                                        output) {
  # global variables assignement ----
  longitude <- latitude <- marine_area_overlay_control_output <- NULL
  # arguments verifications ----
  if (r_type_checking(r_object = data_longitude,
                      type = "character",
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = data_longitude,
                           type = "character",
                           output = "message"))
  }
  if (r_type_checking(r_object = data_latitude,
                      type = "character",
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = data_latitude,
                           type = "character",
                           output = "message"))
  }
  if (length(x = data_longitude) != length(x = data_latitude)) {
    cat(format(x = Sys.time(),
               "%Y-%m-%d %H:%M:%S"),
        "- invalid data, length of the two inputs are not egual.\n")
    stop()
  }
  if (r_type_checking(r_object = overlay_expected,
                      type = "character",
                      length = 1L,
                      allowed_value = c("fao_area",
                                        "eez_area",
                                        "ices_area"),
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = overlay_expected,
                           type = "character",
                           length = 1L,
                           allowed_value = c("fao_area",
                                             "eez_area",
                                             "ices_area"),
                           output = "message"))
  }
  if (r_type_checking(r_object = area_file_path,
                      type = "character",
                      length = 1L,
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = area_file_path,
                           type = "character",
                           length = 1L,
                           output = "message"))
  }
  if (r_type_checking(r_object = output,
                      type = "character",
                      length = 1L,
                      allowed_value = c("message",
                                        "report",
                                        "logical"),
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = output,
                           type = "character",
                           length = 1L,
                           allowed_value = c("message",
                                             "report",
                                             "logical"),
                           output = "message"))
  }
  # data design ----
  data <- dplyr::tibble("longitude" = data_longitude,
                        "latitude" = data_latitude)
  # process ----
  if (overlay_expected == "fao_area") {
    data_final <- furdeb::marine_area_overlay(data = data,
                                              longitude_name = "longitude",
                                              latitude_name = "latitude",
                                              overlay_expected = overlay_expected,
                                              fao_area_file_path = area_file_path,
                                              fao_overlay_level = "subunit",
                                              auto_selection_fao = TRUE,
                                              silent = TRUE) %>%
      dplyr::mutate(marine_area_overlay_control_output = dplyr::case_when(
        is.na(best_fao_area) ~ "no matching",
        TRUE ~ paste0("matching with fao area ",
                      best_fao_area)
      )) %>%
      dplyr::select(longitude,
                    latitude,
                    marine_area_overlay_control_output)
  } else if (overlay_expected == "eez_area") {
    data_final <- furdeb::marine_area_overlay(data = data,
                                              longitude_name = "longitude",
                                              latitude_name = "latitude",
                                              overlay_expected = overlay_expected,
                                              eez_area_file_path = area_file_path,
                                              silent = TRUE) %>%
      dplyr::mutate(marine_area_overlay_control_output = dplyr::case_when(
        is.na(eez) ~ "no matching",
        TRUE ~ paste0("matching with ",
                      eez)
      )) %>%
      dplyr::select(longitude,
                    latitude,
                    marine_area_overlay_control_output)
  } else if (overlay_expected == "ices_area") {
    data_final <- furdeb::marine_area_overlay(data = data,
                                              longitude_name = "longitude",
                                              latitude_name = "latitude",
                                              overlay_expected = overlay_expected,
                                              ices_area_file_path = area_file_path,
                                              silent = TRUE) %>%
      dplyr::mutate(marine_area_overlay_control_output = dplyr::case_when(
        is.na(ices_area) ~ "no matching",
        TRUE ~ paste0("matching with ices area ",
                      ices_area)
      )) %>%
      dplyr::select(longitude,
                    latitude,
                    marine_area_overlay_control_output)
  }
  if (output == "message") {
    if (any(data_final$marine_area_overlay_control_output == "no matching")) {
      number_no_matching <- length(x = which(x = data_final$marine_area_overlay_control_output == "no matching"))
      cat(format(x = Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          "- Failure,",
          number_no_matching,
          ifelse(test = number_no_matching == 1,
                 yes = "element not matching with the area referential.\n",
                 no = "elements not matching with the area referential.\n"))
    } else {
      cat(format(x = Sys.time(),
                 "%Y-%m-%d %H:%M:%S"),
          "- Success, all elements match with the area referential.\n")
    }
  } else if (output == "logical") {
    if (any(data_final$marine_area_overlay_control_output == "no matching")) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else if (output == "report") {
    return(data_final)
  }
}
