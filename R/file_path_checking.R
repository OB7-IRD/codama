#' @name file_path_checking
#' @title Checking for file path
#' @description Checking for file path before import in R.
#' @param file_path Object of class \code{\link[base]{character}} expected. File location on your system.
#' @param extension Object of class \code{\link[base]{vector}} of {\link[base]{character}} expected. Expected extension type set (avoid the . before the extension).
#' @param output {\link[base]{character}} expected. Kind of expected output. By default "error". You can choose between "error", "message" or "logical".
#' @return The function returns error with output is "error" and if the parameters are not respected, a {\link[base]{character}} with output is "message", a {\link[base]{logical}} with output is "logical"
#' @export
file_path_checking <- function(file_path,
                               extension,
                               output = "error") {
  # global process ----
  if (r_type_checking(r_object = file_path,
                      type = "character",
                      length = 1L,
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = file_path,
                           type = "character",
                           length = 1L,
                           output = "message"))
  }
  if (r_type_checking(r_object = extension,
                      type = "character",
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = extension,
                           type = "character",
                           output = "message"))
  }
  if (r_type_checking(r_object = output,
                      type = "character",
                      length = 1L,
                      allowed_value = c("error",
                                        "message",
                                        "logical"),
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = output,
                           type = "character",
                           length = 1L,
                           allowed_value = c("error",
                                             "message",
                                             "logical"),
                           output = "message"))
  }
  if (!is.null(file_path)) {
    file_path_extension <- dplyr::last(x = unlist(x = strsplit(
      x = file_path,
      split = "[.]"
    )))
    if (!file_path_extension %in% extension) {
      message_failure <- paste0(format(x = Sys.time(),
                                       "%Y-%m-%d %H:%M:%S"),
                                " - Failure,",
                                " invalid \"extension\" argument.\n",
                                "File extension expected should be ", paste0("\"", extension, collapse = "\", "), "\" (extension provides is \"", file_path_extension, "\").\n")
      if (output == "message") {
        return(cat(message_failure))
      } else if (output == "error") {
        return(stop(message_failure))
      } else if (output == "logical") {
        return(FALSE)
      }
    }
    if (!file.exists(file_path)) {
      message_failure <- paste0(format(x = Sys.time(),
                                       "%Y-%m-%d %H:%M:%S"),
                                " - Failure,",
                                " invalid \"file_path\" argument.\n",
                                "No file located at the file path.\n")
      if (output == "message") {
        return(cat(message_failure))
      } else if (output == "error") {
        return(stop(message_failure))
      } else if (output == "logical") {
        return(FALSE)
      }
    }
  }
  if (output == "message") {
    return(cat(format(x = Sys.time(),
                      format = "%Y-%m-%d %H:%M:%S"),
               " - Process \"file_path_checking\" ran successfully.\n",
               sep = ""))
  } else if (output == "logical") {
    return(TRUE)
  }
}
