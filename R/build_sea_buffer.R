#' @name build_sea_buffer
#' @title Build a buffered sea/ocean polygon layer for logbook_position_control()
#' @description Precomputes the simplified, buffered sea polygon layer used internally by {\link[codama]{logbook_position_control}} (its dataframe3 argument). Extracted so that callers running logbook_position_control() several times in a row against the same reference geometry (typical of an interactive session) can compute it once and reuse it via the sea_buffer argument, instead of paying for terra::buffer() on every call.
#' @param dataframe3 {\link[base]{data.frame}} expected. Layer containing the seas shapefile, same as the dataframe3 argument of {\link[codama]{logbook_position_control}}.
#' @param buffer_sea {\link[base]{numeric}} expected. Default values: 925. Buffer to be used for seas, in meter. Must match the buffer_sea later passed to {\link[codama]{logbook_position_control}}.
#' @param simplify_tolerance {\link[base]{numeric}} expected. Default values: 50. Tolerance (in meter) used to simplify dataframe3 before buffering. Must match the simplify_tolerance later passed to {\link[codama]{logbook_position_control}}.
#' @return An {\link[sf]{sf}} object, ready to be passed as the sea_buffer argument of {\link[codama]{logbook_position_control}}.
#' @doctest
#' dataframe3 <- sf::st_sf(data.frame(ID = c("ocean_1"),
#'                                    geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(2,0),
#'                                                                                    c(2,2), c(0,2),
#'                                                                                    c(0,0)))),
#'                                                          crs = 4326)))
#' @expect equal(names(.), c("ID", "geometry"))
#' build_sea_buffer(dataframe3)
#' @export
build_sea_buffer <- function(dataframe3,
                              buffer_sea = 925,
                              simplify_tolerance = 50) {
  # 0 - Global variables assignement ----
  . <- NULL
  # 1 - Arguments verification ----
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
  if (!codama::r_type_checking(
    r_object = simplify_tolerance,
    type = "numeric",
    length = 1L,
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = simplify_tolerance,
      type = "numeric",
      length = 1L,
      output = "error"
    ))
  }
  # 2 - Data design ----
  dataframe3 %>%
    sf::st_simplify(dTolerance = simplify_tolerance) %>%
    terra::vect() %>%
    terra::buffer(width = buffer_sea) %>%
    sf::st_as_sf()
}
