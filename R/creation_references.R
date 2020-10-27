#' @name creation_references
#' @title Creation references
#' @param path_references Unique object of type \code{\link[base]{character}} expected. A préciser...
#' @param output_path Chemin d'enregistrement des tables de references
#' @importFrom dplyr last
#' @export #Lafonction est visible par l'utilisateur
creation_references <- function(path_references,
                                output_path) {
  if (missing(path_references)
      || class(path_references) != "character"
      || length(path_references) != 1
      || dplyr::last(unlist(strsplit(x = path_references,
                         split = "[.]"))) != "xlsx") { # || pour vérifier de maniere sequentielle (ne verifie apres si valide -> STOP. Missing pour verifier si argument path_references existe. Separe ma chaine de caracteres en groupe entre les points ".". Dplyr pour prendre la derniere chaine de carateres.

  } else {
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
      tmp_tables_references <- readxl::read_xlsx(path = path_references,
                                                 sheet = a,
                                                 col_names = TRUE)
      tables_references <- append(tables_references,
                                  list(tmp_tables_references))
      names(tables_references)[length(tables_references)] <- a
    }
    save(tables_references,
         file = paste0(output_path,
                       "\\",
                       format(x = Sys.time(), format = "%Y%m%d_%H%M%S"),
                       "_tables_references.RData"))
  }
}
