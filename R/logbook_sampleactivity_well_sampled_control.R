#' @name logbook_sampleactivity_well_sampled_control
#' @title Gives the inconsistencies between the sampling activity and well sampled plan trip
#' @description The purpose of the logbook_sampleactivity_well_sampled_control  function is to provide a table of data that contains an inconsistency between the sampling activity and well sampled plan trip
#' @param dataframe1 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_sampleactivity_well_sampled_control () function.
#' @param dataframe2 {\link[base]{data.frame}} expected. Csv or output of the function {\link[furdeb]{data_extraction}}, which must be done before using the logbook_sampleactivity_well_sampled_control () function.
#' @param output {\link[base]{character}} expected. Kind of expected output. You can choose between "message", "report" or "logical".
#' @details
#' The input dataframe must contain all these columns for the function to work :
#' \itemize{
#' Dataframe 1:
#'  \item{\code{  wellactivity_id}}
#'  \item{\code{  well_label}}
#'  \item{\code{  activity_id}}
#'  \item{\code{  well_id}}
#' }
#' \itemize{
#' Dataframe 2:
#'  \item{\code{  sampleactivity_id}}
#'  \item{\code{  sample_well}}
#'  \item{\code{  activity_id}}
#' }
#' @return The function returns a {\link[base]{character}} with output is "message", a {\link[base]{data.frame}} with output is "report", a {\link[base]{logical}} with output is "logical"
#' @doctest
#' #Well activity 1, 2 and 3 are ok,
#' #Well activity 4 is not linked to a sample, even though the well is sampled
#' dataframe1 <- data.frame(wellactivity_id = c("1", "2", "3", "4"),
#'                          well_label = c("well_1", "well_2", "well_3", "well_3"),
#'                          activity_id = c("1", "2", "3", "4"),
#'                          well_id = c("1", "2", "3", "3"))
#' dataframe2 <- data.frame(sampleactivity_id = c("1", "2"),
#'                          sample_well = c("well_1", "well_3"),
#'                          activity_id = c("1", "3"))
#' @expect equal(., structure(list(wellactivity_id = c("1", "2", "3", "4"), logical = c(TRUE, TRUE, TRUE, FALSE)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -4L)))
#' logbook_sampleactivity_well_sampled_control(dataframe1, dataframe2, output = "report")
#' @export
logbook_sampleactivity_well_sampled_control <- function(dataframe1,
                                                        dataframe2,
                                                        output) {
  # 0 - Global variables assignement ----
  well_id <- NULL
  sample_well <- NULL
  activity_id <- NULL
  sampleactivity_id <- NULL
  nb_sampleactivity <- NULL
  well_label <- NULL
  # 1 - Arguments verification ----
  if (!codama::r_table_checking(
    r_table = dataframe1,
    type = "data.frame",
    column_name = c("wellactivity_id", "well_label", "activity_id", "well_id"),
    column_type = c("character", "character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe1,
      type = "data.frame",
      column_name = c("wellactivity_id", "well_label", "activity_id", "well_id"),
      column_type = c("character", "character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe1 <- dataframe1[, c("wellactivity_id", "well_label", "activity_id", "well_id")]
  }
  if (!codama::r_table_checking(
    r_table = dataframe2,
    type = "data.frame",
    column_name = c("sampleactivity_id", "sample_well", "activity_id"),
    column_type = c("character", "character", "character"),
    output = "logical"
  )) {
    codama::r_table_checking(
      r_table = dataframe2,
      type = "data.frame",
      column_name = c("sampleactivity_id", "sample_well", "activity_id"),
      column_type = c("character", "character", "character"),
      output = "error"
    )
  } else {
    dataframe2 <- dataframe2[, c("sampleactivity_id", "sample_well", "activity_id")]
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
  select <- dataframe1$wellactivity_id
  nrow_first <- length(select)
  # 2 - Data design ----
  # Merge
  dataframe1 <- dplyr::left_join(dataframe1, dataframe2, by = dplyr::join_by(activity_id, well_label == sample_well))

  # Count number activity sampled by well
  dataframe1 <- dataframe1 %>%
    dplyr::group_by(well_id) %>%
    dplyr::mutate(nb_sampleactivity = sum(!is.na(sampleactivity_id)))
  dataframe1$logical <- FALSE
  # Search well activity with activity in sampleactivity
  dataframe1[!is.na(dataframe1$sampleactivity_id) & dataframe1$nb_sampleactivity > 0, "logical"] <- TRUE
  # Search well activity with no activity in sampleactivity
  dataframe1[is.na(dataframe1$sampleactivity_id) & dataframe1$nb_sampleactivity == 0, "logical"] <- TRUE
  # Case the well number is empty
  dataframe1[is.na(dataframe1$well_label), "logical"] <- FALSE
  # Modify the table for display purposes: add, remove and order column
  dataframe1 <- subset(dataframe1, select = -c(sampleactivity_id, activity_id, well_label, well_id, nb_sampleactivity))
  if ((sum(dataframe1$logical, na.rm = TRUE) + sum(!dataframe1$logical, na.rm = TRUE)) != nrow_first || any(is.na(dataframe1$logical))) {
    all <- c(select, dataframe1$wellactivity_id)
    number_occurrences <- table(all)
    text <- ""
    if (any(number_occurrences == 1)) {
      text <- paste0(text, "Missing item ", "(", sum(number_occurrences == 1), "):", paste0(names(number_occurrences[number_occurrences == 1]), collapse = ", "), "\n")
    }
    if (any(number_occurrences > 2)) {
      text <- paste0(text, "Too many item ", "(", sum(number_occurrences > 2), "):", paste0(names(number_occurrences[number_occurrences > 2]), collapse = ", "))
    }
    if (any(is.na(dataframe1$logical))) {
      text <- paste0(text, "Unknown control result", "(", sum(is.na(dataframe1$logical)), "):", paste0(dataframe1$wellactivity_id[is.na(dataframe1$logical)], collapse = ", "))
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
    return(print(paste0("There are ", sum(!dataframe1$logical), " well activity inconsistency with sample activity", collapse = ", ")))
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
