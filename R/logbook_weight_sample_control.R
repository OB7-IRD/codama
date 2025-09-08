#' @name logbook_weight_sample_control
#' @title Gives the inconsistencies between the sample weight (m10 and p10) and the global sample weight
#' @description The purpose of the logbook_weight_sample_control  function is to provide a table of data that contains an inconsistency between the sample weight (m10 and p10) and the global weight
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_weight_sample_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  sample_id}}
#'  \item{\code{  sample_smallsweight}}
#'  \item{\code{  sample_bigsweight}}
#'  \item{\code{  sample_totalweight}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Sample 1, and 2 are ok,
#' #Sample 3 has total weight and smalls weight,
#' #Sample 4 has not total weight nor smalls weight nor bigs weight
#' dataframe1 <- data.frame(sample_id = c("1", "2", "3", "4"),
#'                          sample_smallsweight = c(10, NA, 12, NA),
#'                          sample_bigsweight = c(50, NA, NA, 0),
#'                          sample_totalweight = c(NA, 9, 5, 0))
#' @expect equal(., structure(list(sample_id = c("1", "2", "3", "4"), logical = c(TRUE, TRUE, FALSE, FALSE), sample_totalweight = c(NA, 9, 5, 0), sample_smallsweight = c(10, NA, 12, NA), sample_bigsweight = c(50, NA, NA, 0)), row.names = c(NA, -4L), class = "data.frame"))
#' logbook_weight_sample_control(dataframe1, output = "report")
#' @export
logbook_weight_sample_control <- function(dataframe1,
                                          output) {
  # 0 - Global variables assignement ----
  sample_id <- NULL
  sample_smallsweight <- NULL
  sample_bigsweight <- NULL
  weight_calculation <- NULL
  sample_totalweight <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
    column_type = c("character", "numeric", "numeric", "numeric"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight"),
      column_type = c("character", "numeric", "numeric", "numeric"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("sample_id", "sample_smallsweight", "sample_bigsweight", "sample_totalweight")]
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
  # Calculation weight (Management of NA: if known value performs the sum of the values and ignores the NA, if no known value indicates NA)
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(sample_id) %>%
    dplyr::mutate(weight_calculation = ifelse(all(is.na(c(sample_smallsweight, sample_bigsweight))), NaN, sum(c(sample_smallsweight, sample_bigsweight), na.rm = TRUE))) %>%
    dplyr::ungroup()
  # Check
  comparison_weight_calculation <- codama::vector_comparison(
    first_vector = dataframe1$weight_calculation,
    second_vector = c(0),
    comparison_type = "difference",
    output = "report"
  )
  comparison_totalweight <- codama::vector_comparison(
    first_vector = dataframe1$sample_totalweight,
    second_vector = c(0),
    comparison_type = "difference",
    output = "report"
  )
  # Checks that a weight has been indicated
  dataframe1$logical <- !(comparison_weight_calculation$logical & comparison_totalweight$logical) & !(is.na(dataframe1$weight_calculation) & is.na(dataframe1$sample_totalweight))
  # Checks that a weight is indicated either for the total weight or for one of the two weight categories
  dataframe1$logical[!is.na(dataframe1$weight_calculation) & dataframe1$weight_calculation > 0 & !is.na(dataframe1$sample_totalweight) & dataframe1$sample_totalweight > 0] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(weight_calculation))
  dataframe1 <- dplyr::relocate(.data = dataframe1, sample_totalweight, sample_smallsweight, sample_bigsweight, .after = logical) %>%
    data.frame()
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " sample weight (m10 and p10) inconsistency with the global weight", collapse = ", ")))
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
