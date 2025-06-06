#' @name r_type_checking
#' @title Checking for r object type
#' @description Typing and structure checking for r object.
#' @param r_object R object expected.
#' @param type Object of class \code{\link[base]{character}} expected. Type or class expected of the r object. you can choose between arguments "character", "list", "PostgreSQLConnection", "PqConnection", "numeric", "integer", "logical", "data.frame", "tbl" or "NULL".
#' @param length Object of class \code{\link[base]{integer}} expected. By default NULL. Specify the vector length expected. Fill with NULL with you don't know the length expected.
#' @param allowed_value Object of class \code{\link[base]{vector}} (excepted a list) expected. Be default NULL. Allowed value(s) in the r object.
#' @param output {\link[base]{character}} expected. Kind of expected output. By default "error". You can choose between "error", "message" or "logical".
#' @return The function returns error with output is "error" and if the parameters are not respected, a {\link[base]{character}} with output is "message", a {\link[base]{logical}} with output is "logical"
#' @export
r_type_checking <- function(r_object,
                            type,
                            length = NULL,
                            allowed_value = NULL,
                            output = "error") {
  # arguments verification ----
  if (missing(x = r_object)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, missing \"r_object\" argument.\n")
  } else if (missing(x = type)
             || !inherits(x = type, what = "character")
             || length(x = type) != 1
             || ! type %in% c("character",
                              "list",
                              "PostgreSQLConnection",
                              "PqConnection",
                              "numeric",
                              "integer",
                              "logical",
                              "data.frame",
                              "tbl",
                              "NULL")) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, invalid \"type\" argument.\n",
         "You can choose between arguments",
         " \"character\"",
         ", \"list\"",
         ", \"PostgreSQLConnection\"",
         ", \"PqConnection\"",
         ", \"numeric\"",
         ", \"integer\"",
         ", \"logical\"",
         ", \"data.frame\"",
         ", \"tbl\"",
         " or \"NULL\".\n")
  } else if (! is.null(x = length)
             && !(inherits(x = length, what = "integer")
                || length(x = length) != 1
                || length < 0L)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, invalid \"length\" argument.\n",
         "Argument of type \"integer\" with a length 1 and a value equal or superior to 0 is expected.\n")
  } else if (! is.null(x = allowed_value)
             && ((length(x = allowed_value) == 0)
                || ! class(x = allowed_value) %in% c("character",
                                                      "numeric",
                                                      "integer",
                                                      "logical"))) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, invalid \"allowed_value\" argument.\n",
         "NULL argument expected or vector of type ",
         " \"character\"",
         ", \"numeric\"",
         ", \"integer\"",
         " or \"logical\" with at least a length of 1.\n")
  } else if (!inherits(x = output, what = "character")
             || length(x = output) != 1
             || ! output %in% c("error",
                                "message",
                                "logical")) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, invalid \"output\" argument.\n",
         "You can choose between arguments",
         " \"message\"",
         " or \"logical\".\n")
  } else {
    if (!is.null(r_object)) {
      # global process ----
      if (!inherits(x = r_object, what = type)) {
        message_failure <- paste0(format(x = Sys.time(),
                                       "%Y-%m-%d %H:%M:%S"),
                                " - Failure,",
                                " invalid \"r_object\" argument: ",
                                deparse(expr = substitute(expr = r_object)),
                                "\n",
                                "Argument of type \"",
                                type,
                                "\" is expected.\n")
        if (output == "message") {
          return(cat(message_failure))
        } else if (output == "error") {
          return(stop(message_failure))
        } else if (output == "logical") {
          return(FALSE)
        }
      }
      if (! is.null(x = length)) {
        if (length(x = r_object) != length) {
          message_failure <- paste0(format(x = Sys.time(),
                                           "%Y-%m-%d %H:%M:%S"),
                                    " - Failure,",
                                    " invalid \"r_object\" argument: ",
                                    deparse(expr = substitute(expr = r_object)),
                                    "\n",
                                    "argument with a length ",
                                    length,
                                    " is expected.\n")
          if (output == "message") {
            return(cat(message_failure))
          } else if (output == "error") {
            return(stop(message_failure))
          } else if (output == "logical") {
            return(FALSE)
          }
        }
      }
      if (! is.null(x = allowed_value)) {
        if (! all(r_object %in% allowed_value)) {
          message_failure <- paste0(format(x = Sys.time(),
                                          "%Y-%m-%d %H:%M:%S"),
                                   " - Failure,",
                                   " invalid \"r_object\" argument: ",
                                   deparse(expr = substitute(expr = r_object)),
                                   "\n",
                                   ifelse(test = length(x = allowed_value) == 1,
                                          yes = "value expected is ",
                                          no = "values expected are "),
                                   paste0("\"",
                                          allowed_value,
                                          "\"",
                                          collapse = ", "),
                                   ".\n")
          if (output == "message") {
            return(cat(message_failure))
          } else if (output == "error") {
            return(stop(message_failure))
          } else if (output == "logical") {
            return(FALSE)
          }
        }
      }
    }
    if (output == "message") {
      return(cat(format(x = Sys.time(),
                        format = "%Y-%m-%d %H:%M:%S"),
                 " - Process \"r_type_checking\" ran successfully.\n",
                 sep = ""))
    } else if (output == "logical") {
      return(TRUE)
    }
  }
}
