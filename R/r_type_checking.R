#' @name r_type_checking
#' @title Checking for r object type
#' @description Typing and structure checking for r object.
#' @param r_object R object expected.
#' @param type Object of class \code{\link[base]{character}} expected. Type or class expected of the r object. you can choose between arguments "character", "list", "numeric", "integer" or "logical".
#' @param length Object of class \code{\link[base]{integer}} expected. Specify the vector length expected.
#' @param allowed_values Object of class \code{\link[base]{vector}} (excepted a list) expected. Be default NULL. Allowed value(s) in the r object.
#' @param silent Object of class \code{\link[base]{logical}} expected. By default TRUE. Display output message when process ran  Display output message when process ran  Display output message when process ran sucessfully.
#' @export
r_type_checking <- function(r_object,
                            type,
                            length,
                            allowed_values = NULL,
                            silent = TRUE) {
  # arguments verification ----
  if (missing(x = r_object)) {
    stop("missing \"r_object\" argument.\n")
  }
  if (missing(x = type)
      || class(x = type) != "character"
      || length(x = type) != 1
      || ! type %in% c("character",
                       "list",
                       "numeric",
                       "integer",
                       "logical")) {
    stop("invalid \"type\" argument.\n",
         "you can choose between arguments",
         " \"character\"",
         ", \"list\"",
         ", \"numeric\"",
         ", \"integer\"",
         " or \"logical\".\n")
  } else if (missing(x = length)
             || class(x = length) != "integer"
             || length(x = length) != 1) {
    stop("invalid \"length\" argument.\n",
         "argument of type \"integer\" with a length 1 is expected.\n")
  } else if (! is.null(x = allowed_values)
             & length(x = allowed_values) == 0) {
    stop("invalid \"allowed_values\" argument.\n",
         "NULL argument expected or vector of type ",
         " \"character\"",
         ", \"numeric\"",
         ", \"integer\"",
         " or \"logical\" with at least a length of 1.\n")
  } else {
    # global process ----
    if (! is.null(x = r_object)) {
      if (class(x = r_object) != type
          || length(x = r_object) != length) {
        stop("invalid \"r_object\" argument.\n",
             "argument of type \"",
             type,
             "\" with a length ",
             length,
             " is expected.\n")
      }
      if (! is.null(x = allowed_values)) {
        if (! all(r_object %in% allowed_values)) {
          stop("invalid \"r_object\" argument.\n",
               ifelse(test = length(x = allowed_values) == 1,
                      yes = "value expected is ",
                      no = "values expected are "),
               paste0("\"",
                      allowed_values,
                      "\"",
                      collapse = ", "),
               ".\n")
        }
      }
    }
    if (silent == FALSE) {
      cat(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Process \"r_type_checking\" ran successfully.\n",
          sep = "")
    }
  }
}
