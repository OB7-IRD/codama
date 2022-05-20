#' @name file_path_checking
#' @title Checking for file path
#' @description Checking for file path before import in R.
#' @param file_path Object of class \code{\link[base]{character}} expected. File location on your system.
#' @param extension Object of class \code{\link[base]{character}} expected. Type of extension expected (avoid the . before the extension).
#' @param silent Object of class \code{\link[base]{logical}} expected. By default TRUE. Display output message when process ran  Display output message when process ran  Display output message when process ran sucessfully.
#' @export
file_path_checking <- function(file_path,
                               extension,
                               silent = TRUE) {
  # global process ----
  r_type_checking(r_object = file_path,
                  type = "character",
                  length = 1L)
  r_type_checking(r_object = extension,
                  type = "character",
                  length = 1L)
  if (extension != dplyr::last(x = unlist(x = strsplit(x = file_path,
                                                       split = "[.]")))) {
    stop("invalid \"extension\" argument.\n",
         "file extension expected is not identical to the \"file_path\" argument extension.\n")
  } else if (! file.exists(file_path)) {
    stop("invalid \"file_path\" argument.\n",
         "no file located at the file path.\n")
  }
  if (silent == FALSE) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Process \"file_path_checking\" ran successfully.\n",
        sep = "")
  }
}
