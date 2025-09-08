#' @name check_measure_inspector
#' @title Gives the inconsistencies between the total number of individuals measured per sample and the sum of individuals per sample, species and size class
#' @description The purpose of the check_measure_inspector function is to provide a table of data that contains an inconsistency between the total number of individuals measured per sample and the sum of individuals per sample, species and size class
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_measure_inspector () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the check_measure_inspector () function.
#' @param output {\link[base]{character}} expected.Kind of expected output. You can choose between "message", "report" or "logical".
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  samplespecies_id}}
#'  \item{\code{  samplespecies_measuredcount}}
#'  \item{\code{  sample_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  samplespeciesmeasure_id}}
#'  \item{\code{  samplespeciesmeasure_count}}
#'  \item{\code{  samplespecies_id}}
#' }
#' @doctest
#' #Sample 1 and 2 are ok,
#' #Sample 3 has different count,
#' #Sample 4 has sample species measure count is missing in dataframe2,
#' #Sample 5 has sample species measured count is missing in dataframe1
#' dataframe1 <- data.frame(samplespecies_id = c("1", "2", "3", "4", "5", "6"),
#'                          samplespecies_measuredcount = c(4, 6, 15, 6, 7, NA),
#'                          sample_id = c("1", "1", "2", "3", "4", "5"))
#' dataframe2 <- data.frame(samplespeciesmeasure_id = c("1", "2", "3", "4", "5", "6"),
#'                          samplespeciesmeasure_count = c(10, 10, 5, 3, 2, 8),
#'                          samplespecies_id = c("1", "3", "3", "4", "4", "6"))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4", "5"), logical = c(TRUE, TRUE, FALSE, FALSE, FALSE), sum_measuredcount = c(10, 15, 6, 7, NA), sum_count = c(10, 15, 5, NA, 8)), row.names = c(NA, -5L), class = "data.frame"))
#' check_measure_inspector(dataframe1, dataframe2, output = "report")
#' @export
check_measure_inspector <- function(dataframe1,
                                    dataframe2,
                                    output) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  samplespecies_id <- NULL
  samplespecies_measuredcount <- NULL
  samplespeciesmeasure_count <- NULL
  sum_measuredcount <- NULL
  sum_count <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("samplespecies_id", "samplespecies_measuredcount", "sample_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("samplespecies_id", "samplespecies_measuredcount", "sample_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("samplespecies_id", "samplespecies_measuredcount", "sample_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_count", "samplespecies_id"),
    column_type = c("character", "numeric", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("samplespeciesmeasure_id", "samplespeciesmeasure_count", "samplespecies_id"),
      column_type = c("character", "numeric", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("samplespeciesmeasure_id", "samplespeciesmeasure_count", "samplespecies_id")]
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
  # Calculates the total sum of individuals by sample (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe2 <- dataframe2 %>%
    dplyr::group_by(samplespecies_id) %>%
    dplyr::summarise(sum_count = ifelse(all(is.na(samplespeciesmeasure_count)), samplespeciesmeasure_count[NA_integer_], sum(samplespeciesmeasure_count, na.rm = TRUE)))
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(samplespecies_id))
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(sum_measuredcount = ifelse(all(is.na(samplespecies_measuredcount)), samplespecies_measuredcount[NA_integer_], sum(samplespecies_measuredcount, na.rm = TRUE)),
                     sum_count = ifelse(all(is.na(sum_count)), sum_count[NA_integer_], sum(sum_count, na.rm = TRUE)))
  # Compare the two sums
  comparison <- codama::vector_comparison(
    first_vector = dataframe1$sum_measuredcount,
    second_vector = dataframe1$sum_count,
    comparison_type = "equal",
    output = "report"
  )
  dataframe1$logical <- comparison$logical
  dataframe1 <- dplyr::relocate(.data = dataframe1, sum_measuredcount, sum_count, .after = logical) %>%
    data.frame()
  # Management of missing count measurements by sample and by species
  dataframe1[is.na(dataframe1$sum_measuredcount), "logical"] <- FALSE
  # Management of missing count measurements by sample and by species and by size class
  dataframe1[is.na(dataframe1$sum_count), "logical"] <- FALSE
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " samples with number of individuals measured per species different from number measured per species and size class")))
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
