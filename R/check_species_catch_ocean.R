#' @name check_species_catch_ocean
#' @title Gives the inconsistencies between the species fished and the ocean reported for the trip
#' @description The purpose of the check_species_catch_ocean function is to provide a table of data that contains an inconsistency between the species caught during the trip and the ocean reported for the trip with the ocean for which the species are associated
#' @param data_connection {\link[base]{list}} expected. Either output of the function {\link[furdeb]{postgresql_dbconnection}}, which must be done before using the check_species_catch_ocean function or the two data sets used (the first one is the species-ocean associations and the second one is the species caught with the associated ocean)
#' @param type_select {\link[base]{character}} expected. Choose the study unit, you can choose between: "trip" or "year".
#' @param select {\link[base]{character}} expected with type_select is "trip". Then give unique identifier for the trip. {\link[base]{numeric}} expected with type_select is "year".Then give year for the trip.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @export
#' @importFrom DBI dbGetQuery sqlInterpolate SQL
#' @importFrom dplyr tibble
check_species_catch_ocean <- function(data_connection,
                                      type_select,
                                      select,
                                      output) {
  # 0 - Global variables assignement ----
  ocean_idspecie_id <- NULL
  first_vector <- NULL
  logical <- NULL
  # 1 - Arguments verification ----
  if (r_type_checking(
    r_object = data_connection,
    type = "list",
    length = 2L,
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = data_connection,
      type = "list",
      length = 2L,
      output = "message"
    ))
  } else {
    if (!is.data.frame(data_connection[[1]]) && r_type_checking(
      r_object = data_connection[[2]],
      type = "PostgreSQLConnection",
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = data_connection[[2]],
        type = "PostgreSQLConnection",
        output = "message"
      ))
    }
  }
  # Checks the type and values of output
  if (r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "message"
    ))
  }
  if (data_connection[1] == "observe_9a") {
    # Checks the type and values of type_select
    if (r_type_checking(
      r_object = type_select,
      type = "character",
      allowed_value = c("trip", "year"),
      output = "logical"
    ) != TRUE) {
      return(r_type_checking(
        r_object = type_select,
        type = "character",
        allowed_value = c("trip", "year"),
        output = "message"
      ))
    }
    # Checks the type of select according to type_select
    if (type_select == "trip" && r_type_checking(r_object = select,
                                                 type = "character",
                                                 output = "logical") != TRUE) {
      return(r_type_checking(
        r_object = select,
        type = "character",
        output = "message"
      ))
    }
    if (type_select == "year" && r_type_checking(r_object = select,
                                                 type = "numeric",
                                                 output = "logical") != TRUE) {
      return(r_type_checking(
        r_object = select,
        type = "numeric",
        output = "message"
      ))
    }
    # 2 - Data extraction ----
    # Species and their associated ocean
    specie_ocean_sql <- paste(
      readLines(con = system.file("sql",
                                  "specie_ocean.sql",
                                  package = "codama"
      )),
      collapse = "\n"
    )
    # Species caught during the trip, ocean declared
    specie_catch_ocean_sql <- paste(readLines(con = system.file("sql",
                                                                "specie_catch_ocean.sql",
                                                                package = "codama")), collapse = "\n")
    # Trip selection in the SQL query
    if (type_select == "trip") {
      specie_catch_ocean_sql <- sub(
        pattern = "OR\n    EXTRACT(year FROM t.startdate) IN (?select_item) OR EXTRACT(year FROM t.enddate) IN (?select_item)",
        replacement = "",
        x = specie_catch_ocean_sql,
        fixed = TRUE
      )
      select <- paste0("'", select, "'")
    }
    # Year selection in the SQL query
    if (type_select == "year") {
      specie_catch_ocean_sql <- sub(
        pattern = "\n    t.topiaid IN (?select_item) OR",
        replacement = "",
        x = specie_catch_ocean_sql,
        fixed = TRUE
      )
    }
    specie_ocean_sql <- DBI::SQL(
      x = specie_ocean_sql
    )
    specie_catch_ocean_sql <- DBI::sqlInterpolate(
      conn = data_connection[[2]],
      sql = specie_catch_ocean_sql,
      select_item = DBI::SQL(paste(select,
                                   collapse = ", "
      ))
    )
    specie_ocean_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = specie_ocean_sql
    ))
    specie_catch_ocean_data <- dplyr::tibble(DBI::dbGetQuery(
      conn = data_connection[[2]],
      statement = specie_catch_ocean_sql
    ))
  } else {
    if (is.data.frame(data_connection[[1]]) == TRUE && is.data.frame(data_connection[[2]]) == TRUE) {
      specie_ocean_data <- data_connection[[1]]
      specie_catch_ocean_data <- data_connection[[2]]
    } else {
      stop(
        format(
          x = Sys.time(),
          format = "%Y-%m-%d %H:%M:%S"
        ),
        " - Consistency check not developed yet for this \"data_connection\" argument, you can provide both sets of data instead.\n ",
        sep = ""
      )
    }
  }
  # 3 - Data design ----
  nrow_first <- nrow(specie_catch_ocean_data)
  # Groups the pair of identifiers
  specie_catch_ocean_data$ocean_idspecie_id <- paste0(specie_catch_ocean_data$ocean_id, specie_catch_ocean_data$specie_id)
  specie_ocean_data$ocean_idspecie_id <- paste0(specie_ocean_data$ocean_id, specie_ocean_data$specie_id)
  # Search for the pair species, ocean of capture in the associations species, ocean possible
  comparison <- vector_comparison(specie_catch_ocean_data$ocean_idspecie_id,
                                  specie_ocean_data$ocean_idspecie_id,
                                  comparison_type = "difference",
                                  output = "report"
  )
  comparison$logical <- FALSE
  colnames_comparison <- grep("vectors_comparisons_", colnames(comparison))
  comparison$logical[comparison[, colnames_comparison] == "no difference"] <- TRUE
  specie_catch_ocean_data <- cbind(specie_catch_ocean_data, comparison)
  if ((sum(specie_catch_ocean_data$logical) + sum(!specie_catch_ocean_data$logical)) != nrow_first) {
    stop(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      sep = ""
    )
  }
  
  # 4 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!specie_catch_ocean_data$logical), " catches with species that are not associated with the ocean of the trip")))
  }
  if (output == "report") {
    specie_catch_ocean_data <- subset(specie_catch_ocean_data, select = -c(ocean_idspecie_id, first_vector, logical))
    return(specie_catch_ocean_data)
  }
  if (output == "logical") {
    return(specie_catch_ocean_data$logical)
  }
}
