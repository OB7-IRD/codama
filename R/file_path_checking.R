#' @name file_path_checking
#' @title Checking for file path
#' @description Checking for file path before import in R.
#' @param file_path Object of class \code{\link[base]{character}} expected. File location on your system.
#' @param extension Object of class \code{\link[base]{vector}} of {\link[base]{character}} expected. Expected extension type set (avoid the . before the extension).
#' @param output {\link[base]{character}} expected. Kind of expected output. By default "message". You can choose between "message" or "logical".
#' @export
file_path_checking <- function(file_path,
                               extension,
                               output = "message") {
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
                      allowed_value = c("message",
                                         "logical"),
                      output = "logical") != TRUE) {
    return(r_type_checking(r_object = output,
                           type = "character",
                           length = 1L,
                           allowed_value = c("message",
                                              "logical"),
                           output = "message"))
  }
  file_path_extension <- dplyr::last(x = unlist(x = strsplit(
    x = file_path,
    split = "[.]"
  )))
  if (! file_path_extension %in% extension) {
    if (output == "message") {
      return(cat(paste0(format(x = Sys.time(),
                               "%Y-%m-%d %H:%M:%S"),
                        " - Failure,",
                        " invalid \"extension\" argument.\n",
                        "File extension expected should be ", paste0("\"", extension, collapse = "\", "), "\" (extension provides is \"", file_path_extension, "\").\n")))
    } else if (output == "logical") {
      return(FALSE)
    }
  }
  if (! file.exists(file_path)) {
    if (output == "message") {
      return(cat(paste0(format(x = Sys.time(),
                               "%Y-%m-%d %H:%M:%S"),
                        " - Failure,",
                        " invalid \"file_path\" argument.\n",
                        "No file located at the file path.\n")))
    } else if (output == "logical") {
      return(FALSE)
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
