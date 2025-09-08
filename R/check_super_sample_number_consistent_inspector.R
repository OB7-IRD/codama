#' @name check_super_sample_number_consistent_inspector
#' @title Gives the inconsistencies between the sample and the subsample number
#' @description The purpose of the check_super_sample_number_consistent_inspector function is to provide a table of data that contains an inconsistency between the sample and the subsample number
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_super_sample_number_consistent_inspector() function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_super_sample_number_consistent_inspector() function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_supersample}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  samplespecies_subsamplenumber}}
#'  \item{\code{  sample_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1 and 2 are ok,
#' #Sample 3 is not a super sample but the numbering starts at 1 instead of 0,
#' #Sample 4 is a super sample but the same sub-sample number appears twice,
#' #Sample 5 has no sample species,
#' #Sample 6 is a super sample but the numbering starts at 0 instead of 1,
#' #Sample 7 is a super sample but has only one sub-sample species
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4", "5", "6", "7"),
#'                          sample_supersample = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE))
#' dataframe2 <- data.frame(samplespecies_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
#'                          samplespecies_subsamplenumber = c(0, 1, 2, 1, 1, 1, 0, 1, 1),
#'                          sample_id = c("1", "2", "2", "3", "4", "4", "6", "6", "7"))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4", "5", "6", "7"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE), sample_supersample = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE), count_subsamplenumber_n0 = c(0L, 2L, 1L, 2L, NA, 1L, 1L), count_subsamplenumber_0 = c(1L, 0L, 0L, 0L, NA, 1L, 0L), count_subsamplenumber_1 = c(0L, 1L, 1L, 2L, NA, 1L, 1L), count_subsamplenumber = c(1L, 2L, 1L, 1L, NA, 2L, 1L)), row.names = c(NA, 7L), class = "data.frame"))
#' check_super_sample_number_consistent_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_super_sample_number_consistent_inspector <- function(dataframe1,
                                                           dataframe2,
                                                           output) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  samplespecies_subsamplenumber <- NULL
  samplespecies_id <- NULL
  count_subsamplenumber_n0 <- NULL
  count_subsamplenumber_0 <- NULL
  count_samplespecies <- NULL
  count_subsamplenumber_1 <- NULL
  only_one_subsampling <- NULL
  many_subsampling <- NULL
  count_samplespecies_bis <- NULL
  count_subsamplenumber_n0_bis <- NULL
  count_subsamplenumber_0_bis <- NULL
  count_subsamplenumber_1_bis <- NULL
  sample_supersample <- NULL
  count_subsamplenumber <- NULL
  count_subsamplenumber_bis <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_supersample"),
    column_type = c("character", "logical"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_supersample"),
      column_type = c("character", "logical"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_supersample")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespecies_id", "samplespecies_subsamplenumber", "sample_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespecies_id", "samplespecies_subsamplenumber", "sample_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("samplespecies_id", "samplespecies_subsamplenumber", "sample_id")]
  }
  # Checks the type and values of output
  if (!codama::r_type_checking(
    r_object = output,
    type = "character",
    allowed_value = c("message", "report", "logical"),
    output = "logical"
  )) {
    return(codama::r_type_checking(
      r_object = output,
      type = "character",
      allowed_value = c("message", "report", "logical"),
      output = "error"
    ))
  }
  select <- dataframe1$sample_id
  nrow_first <- length(unique(select))
  # 2 - Data design ----
  # Search subsample number in the associations samples ID
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(count_subsamplenumber_n0 = sum(samplespecies_subsamplenumber != 0), count_subsamplenumber_0 = sum(samplespecies_subsamplenumber == 0), count_samplespecies = sum(!is.na(unique(samplespecies_id))), count_subsamplenumber = sum(!is.na(unique(samplespecies_subsamplenumber))), count_subsamplenumber_1 = sum(samplespecies_subsamplenumber == 1))
  # Merge
  if (nrow(dataframe1) > 0) {
    dataframe1$logical <- TRUE
  } else {
    dataframe1$logical <- logical()
  }
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(sample_id))
  # Case of NA count_subsamplenumber_n0, count_subsamplenumber_0, count_samplespecies or count_subsamplenumber_1
  dataframe1 <- dataframe1 %>%
    dplyr::mutate(
      count_subsamplenumber_n0_bis = dplyr::coalesce(count_subsamplenumber_n0, 0),
      count_subsamplenumber_0_bis = dplyr::coalesce(count_subsamplenumber_0, 0),
      count_samplespecies_bis = dplyr::coalesce(count_samplespecies, 0),
      count_subsamplenumber_bis = dplyr::coalesce(count_subsamplenumber, 0),
      count_subsamplenumber_1_bis = dplyr::coalesce(count_subsamplenumber_1, 0),
    )
  dataframe1[dataframe1$count_samplespecies_bis == 0, "logical"] <- FALSE
  dataframe1$only_one_subsampling <- dataframe1$sample_supersample == FALSE & dataframe1$count_subsamplenumber_n0_bis == 0
  dataframe1$many_subsampling <- dataframe1$sample_supersample == TRUE & dataframe1$count_subsamplenumber_0_bis == 0 & dataframe1$count_subsamplenumber_bis > 1
  dataframe1[!(dataframe1$only_one_subsampling | dataframe1$many_subsampling), "logical"] <- FALSE
  dataframe1[dataframe1$count_subsamplenumber_bis == 1 & dataframe1$count_subsamplenumber_1_bis > 0, "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(only_one_subsampling, many_subsampling, count_samplespecies_bis, count_subsamplenumber_bis, count_subsamplenumber_n0_bis, count_subsamplenumber_0_bis, count_subsamplenumber_1_bis, count_samplespecies))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_supersample, count_subsamplenumber_n0, count_subsamplenumber_0, count_subsamplenumber_1, count_subsamplenumber, .after = logical)
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$sample_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$sample_id[is.na(dataframe1$logical)], collapse = ", "))
    }
    warning(
      format(
        x = Sys.time(),
        format = "%Y-%m-%d %H:%M:%S"
      ),
      " - your data has some peculiarities that prevent the verification of inconsistencies.\n",
      text,
      sep = ""
    )
  }
  # 3 - Export ----
  if (output == "message") {
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples inconsistency with subsample number", collapse = ", ")))
  }
  if (output == "report") {
    return(dataframe1)
  }
  if (output == "logical") {
    if (sum(!dataframe1$logical) == 0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
