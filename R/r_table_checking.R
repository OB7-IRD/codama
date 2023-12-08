#' @name r_table_checking
#' @title Checking for table object
#' @description Type, name and type of mandatory columns are controlled
#' @param r_table Table object expected.
#' @param type Object of class \code{\link[base]{character}} expected. Type or class expected of the r table You can choose between arguments "data.frame", or "tbl".
#' @param column_name \code{\link[base]{vector}} of {\link[base]{character}} expected. Be default NULL. Mandatory column names in r_table.
#' @param column_type \code{\link[base]{vector}} of {\link[base]{character}} expected. Be default NULL. Mandatory column type in r_table.
#' @param output {\link[base]{character}} expected. Kind of expected output. By default "error". You can choose between "error", "message" or "logical".
#' @return The function returns error with output is "error" and if the parameters are not respected, a {\link[base]{character}} with output is "message", a {\link[base]{logical}} with output is "logical"
#' @export
r_table_checking <- function(r_table,
                             type,
                             column_name = NULL,
                             column_type = NULL,
                             output = "error") {
  # 0 - Global variables assignement ----
  # 1 - Arguments verification ----
  # Checks the type and values of type
  r_type_checking(
    r_object = type,
    type = "character",
    allowed_value = c("data.frame", "tbl"),
    output = "error"
  )
  # Checks the type of column_name
  if (!is.null(column_name) && r_type_checking(
    r_object = column_name,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = column_name,
      type = "character",
      output = "error"
    ))
  }
  # Checks the type of column_type
  if (!is.null(column_type) && r_type_checking(
    r_object = column_type,
    type = "character",
    output = "logical"
  ) != TRUE) {
    return(r_type_checking(
      r_object = column_type,
      type = "character",
      output = "error"
    ))
  }
  # Checks the type and values of output
  r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("error", "message", "logical"),
    output = "error"
  )
  # Checks the length of column_name and column_type
  if (length(x = column_name) != length(x = column_type) && !is.null(column_type)) {
    stop(cat(
      format(
        x = Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      " Error - invalid data, different sizes between \"column_name\" and \"column_type\"\n"
    ))
  }
  # 2 - global process ----
  # Checks the type of r_table
    if (r_type_checking(
      r_object = r_table,
      type = type,
      output = "logical"
    ) != TRUE) {
      message_failure <- paste0(format(
                              x = Sys.time(),
                              "%Y-%m-%d %H:%M:%S"
                              ),
                              " - Failure,",
                              " invalid \"r_table\" argument: ",
                              deparse(expr = substitute(expr = r_table)),
                              "\n",
                              "Argument of type \"",
                              type,
                              "\" is expected.\n"
                              )
      if (output == "error") {
        return(stop(message_failure))
      }
      if (output == "message") {
        return(cat(message_failure))
      }
      if (output == "logical") {
        return(FALSE)
      }
    }
  # Checks column names
  checks_column_names <- column_name %in% colnames(r_table)
  if (sum(checks_column_names) != length(column_name)) {
    message_failure <- paste0(format(
                              x = Sys.time(),
                              "%Y-%m-%d %H:%M:%S"),
                              paste0(" Error - invalid data, the following column names in \"column_name\" were not found in \"r_table\" ", deparse(expr = substitute(expr = r_table)), " : ", paste0(column_name[!checks_column_names], collapse = ", "), "\n"))
    if (output == "error") {
      return(stop(message_failure))
    }
    if (output == "message") {
      return(cat(message_failure))
    }
    if (output == "logical") {
      return(FALSE)
    }
  }
  # Checks column type
  checks_column_type <- sapply(r_table[, column_name], class) == column_type
  if (sum(checks_column_type) != length(column_type)) {
    message_failure <- paste0(format(
                             x = Sys.time(),
                             "%Y-%m-%d %H:%M:%S"),
                              paste0("Error - invalid data, the following column names in \"column_name\" are not of the type of \"column_type\" in \"r_table\" ", deparse(expr = substitute(expr = r_table)), ". \n", paste0(paste0("column names :", colnames(r_table[, column_name, drop = FALSE])[!checks_column_type], " ; type in \"r_table\" :", sapply(r_table[, colnames(r_table[, column_name, drop = FALSE])[!checks_column_type], drop = FALSE], class), " ; type in \"column_type\" :", column_type[!checks_column_type], collapse = ",\n"), "\n", collapse = ", ")))
    if (output == "error") {
      return(stop(message_failure))
    }
    if (output == "message") {
      return(cat(message_failure))
    }
    if (output == "logical") {
      return(FALSE)
    }
  }
  if (output == "message") {
    return(cat(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - Process \"r_table_checking\" ran successfully.\n",
      sep = ""
    ))
  }
  if (output == "logical") {
    return(TRUE)
  }
}
