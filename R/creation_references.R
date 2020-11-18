#' @name creation_references
#' @title Creation references
#' @param path_references Unique object of type \code{\link[base]{character}} with xlsx extension expected. A préciser...
#' @param output_path Unique object of type \code{\link[base]{character}} expected. Chemin d'enregistrement des tables de references.
#' @importFrom dplyr last
#' @importFrom readxl read_xlsx
#' @export
creation_references <- function(path_references,
                                output_path) {
  if (missing(path_references)
      || class(path_references) != "character"
      || length(path_references) != 1
      || dplyr::last(unlist(strsplit(x = path_references,
                         split = "[.]"))) != "xlsx") {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        "- Error: invalid \"path_references\" argument\n",
        "One element of class character with xlsx extension expected.\n",
        sep = "")
  } else if (missing(output_path)
             || class(output_path) != "character"
             || length(output_path) != 1) {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        "- Error: invalid \"output_path\" argument\n",
        "One element of class character expected.\n",
        sep = "")
  } else {
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - Start function process",
        ".\n",
        sep = "")
    tables_references <- vector(mode = "list")
    for (a in c("OCEAN",
                "QUADRANT",
                "VESSEL",
                "GEAR",
                "LANDING",
                "FISHING_MODE",
                "WELL",
                "VESSEL_STORAGE",
                "SAMPLING_PLATFORM",
                "FISH_SAMPLING_STATUS",
                "PERSON",
                "SPECIES",
                "PROJECT",
                "SEX",
                "MEASURING_DEVICE",
                "MACRO_MATURITY",
                "STOMACH_FULLNESS",
                "STOMACH_PREY_GROUP",
                "TISSUE",
                "ANALYSIS",
                "SAMPLE_STORAGE",
                "MICRO_MATURITY",
                "POF",
                "OOCYTE_STAGE",
                "ATRESIA_TYPE",
                "ATRESIA_SIGN",
                "ATRESIA_STAGE")) {
      cat(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Start process on table ",
          a,
          ".\n",
          sep = "")
      tmp_tables_references <- readxl::read_xlsx(path = path_references,
                                                 sheet = a,
                                                 col_names = TRUE)
      tables_references <- append(tables_references,
                                  list(tmp_tables_references))
      names(tables_references)[length(tables_references)] <- a
      cat(format(x = Sys.time(),
                 format = "%Y-%m-%d %H:%M:%S"),
          " - Successful process on table ",
          a,
          ".\n",
          sep = "")
    }
    save(tables_references,
         file = paste0(output_path,
                       "\\",
                       format(x = Sys.time(),
                              format = "%Y%m%d_%H%M%S"),
                       "_tables_references.RData"))
    cat(format(x = Sys.time(),
               format = "%Y-%m-%d %H:%M:%S"),
        " - End of function process",
        ".\n",
        sep = "")
  }
}
