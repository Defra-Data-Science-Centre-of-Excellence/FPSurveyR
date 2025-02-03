#' @title  FPS: Pre-processing: Data to binary
#' @author Tom Pearson
#' @description This function dynamically converts specified columns in a
#'   dataset to binary indicator columns based on a named list of question
#'   values. For each column, a new binary column is created for each value in
#'   the corresponding vector of options.
#'
#' @param .data A data frame containing the input dataset.
#' @param question_values A named list where the names are column names in the
#'   dataset, and the values are numeric vectors of all possible options for
#'   those columns. If the question is non-binary, then an empty vector or `NULL`
#'   must be provided.
#' @param nonbinary_questions A character vector of questions to exclude from
#'   processing to binary (e.g. `"Q1_TEXT"`). Default is `NULL`.
#'
#' @return A data frame with the original data and additional binary indicator
#'   columns (e.g. `"Q1_v1"`).
#'
#' @details For character columns, the function uses regular expressions to
#'   identify matches. For numeric columns, direct equality checks are
#'   performed. The function assumes all options provided in `question_values`
#'   are exhaustive for the corresponding columns.
#'
#' @examples
#' # s8_data <- data.frame(
#' #   Q56 = c(1, 2, 3, 4, 5),
#' #   Q57 = c("1,2", "3,4", "6", "1,3", "6,4"),
#' #   Q57_TEXT = c("example1", "example2", "example3", "example4", "example5"),
#' #   Q58 = c(10, 25, 50, 1, 10)
#' # )
#' #
#' # question_values <- list(
#' #   Q56 = 1:5, # Exhaustive options for Q56
#' #   Q57 = 1:6,  # Exhaustive options for Q57
#' #   Q57_TEXT = c(),
#' #   Q58 = c()
#' # )
#' #
#' # fps_data_to_binary(s8_data, question_values, nonbinary_questions = c("Q57_TEXT", "Q58"))
#'
#' @importFrom magrittr %>%
#' @export
fps_data_to_binary <- function(.data, question_values, nonbinary_questions = NULL) {

  #testing======================================================================

  # .data = s8_data
  # question_values = s8_questions_cols

  # .data = testing_df
  # question_values = testing_qvals
  # nonbinary_questions = "Q2"

  #validation===================================================================
  if (!is.data.frame(.data)) {
    cli::cli_abort("`.data` must be a valid data frame.")
  }
  if (!is.list(question_values) || length(question_values) == 0) {
    cli::cli_abort("`questions` must be a non-empty list")
  }
  if (any(!(names(question_values) %in% names(.data)))) {
    missing_qs <- names(question_values[!(names(question_values) %in% names(.data))])
    cli::cli_abort("`question_values` names must be found in `.data`. Check {.val {missing_qs}} is in `.data`.")
  }
  if (!is.null(nonbinary_questions) && !is.character(nonbinary_questions)) {
    cli::cli_abort("`nonbinary_questions` must be a character vector")
  }
  if  (any(!(names(nonbinary_questions) %in% names(.data)))) {
    cli::cli_abort("`nonbinary_questions` must be found in the `.data`")
  }

  #filter out nonbinary questions===============================================

  question_values <- question_values[!(names(question_values) %in% nonbinary_questions)]

  all_valid <- all(sapply(question_values, function(x) is.numeric(x) || is.null(x)))
  if (!all_valid) {
    cli::cli_abort("all value vectors in `question_values` must be numeric (or `NULL` if non-binary)")
  }

  #data to binary===============================================================

  for(q_name in names(question_values)) {

    options <- question_values[[q_name]]

    # Dynamically create binary columns for each value option
    for(option in options) {

      binary_column_name <- paste0(q_name, "_v", option)

      if(binary_column_name %in% colnames(.data)) {
        cli::cli_abort("New binary column name {.val {binary_column_name}} already exists in the dataset")
      }

      if(is.character(.data[[q_name]])) {

        .data[[binary_column_name]] <- ifelse(grepl(paste0("\\b", option, "\\b"), as.character(.data[[q_name]])), 1, 0)

      } else if(is.numeric(.data[[q_name]])) {

        .data[[binary_column_name]] <- ifelse(.data[[q_name]] == option, 1, 0)

      }
    }
  }

  return(.data)

}





#' @title FPS: Pre-processing: Format factors
#' @author Tom Pearson
#' @description This function formats the factor variables in a dataset based on
#'   standard FPS conventions. It renames specific columns for region, farm
#'   size, farm type, and stratification to their standard names, applies
#'   specific transformations to values in these columns, and ensures consistent
#'   formatting.
#'
#' @param .data A data frame containing the data to format.
#' @param region_col A character string specifying the column name for the
#'   region. Default is `"fps_nuts_name"`.
#' @param farm_size_col A character string specifying the column name for the
#'   farm size. Default is `"fps_slr_name"`.
#' @param farm_type_col A character string specifying the column name for the
#'   farm type. Default is `"fps_robust"`.
#' @param strat_col A numeric or character string specifying the column name for
#'   the stratification. Default is `"post_strat"`.
#'
#' @return A data frame with formatted factor variables and renamed columns.
#'
#' @examples
#' #data <- data.frame(
#' #  fps_gor = c("North West", "South East (England)", "Yorkshire"),
#' #  fps_slr_name = c("small", "medium", "large"),
#' #  fps_robust = c("Pigspoultry", "Dairy", "Arable"),
#' #  post_strat = c("1", "2", "3"))
#' #formatted_data <- fps_format_factors(data)
#'
#' @importFrom magrittr %>%
#' @export
fps_format_factors <- function(.data,
                               region_col = "fps_nuts_name", #should be fps_gor
                               farm_size_col = "fps_slr_name",
                               farm_type_col = "fps_robust",
                               strat_col = "post_strat") {

  #testing======================================================================
  # region_col = "fps_nuts_name" #should be fps_gor
  # farm_size_col = "fps_slr_name"
  # farm_type_col = "fps_robust"
  # strat_col = "post_strat"

  #validation===================================================================
  if (!is.data.frame(.data)) {
    cli::cli_abort("`.data` must be a data frame.")
  }

  # Validate column names
  cols_to_check <- c(region_col, farm_size_col, farm_type_col, strat_col)
  missing_cols <- setdiff(cols_to_check, colnames(.data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("The following columns are missing from `.data`: {.var {missing_cols}}")
  }

  if (!is.character(.data[[region_col]])) {
    cli::cli_abort("{.var {region_col}} must be a character vector.")
  }

  if (!is.character(.data[[farm_size_col]])) {
    cli::cli_abort("{.var {farm_size_col}} must be a character vector.")
  }

  if (!is.character(.data[[farm_type_col]])) {
    cli::cli_abort("{.var {farm_type_col}} must be a character vector.")
  }

  #format factors===============================================================
  cli::cli_alert_info("Analysis: standard factor columns are always renamed to: `fps_gor` (region), `fps_slr_name` (farm size), `fps_robust` (farm type); stratification column to: `post_strat`.")

  .data <-
    .data %>%
    dplyr::rename(fps_gor = {{region_col}},
                  fps_slr_name = {{farm_size_col}},
                  fps_robust = {{farm_type_col}},
                  post_strat = {{strat_col}}) %>%
    dplyr::mutate(fps_gor = gsub(" \\(England\\)", "", fps_gor, ignore.case = TRUE),
                  fps_gor = ifelse(grepl("North West", fps_gor, ignore.case = TRUE), "North West and Merseyside", fps_gor),

                  fps_slr_name = snakecase::to_sentence_case(fps_slr_name),

                  fps_robust = ifelse(grepl("pig|poultry", fps_robust, ignore.case = TRUE), "Pigs and poultry", fps_robust),

                  post_strat = as.character(post_strat))

  return(.data)

}


#' @title FPS: Pre-processing: Calculate weights
#' @author Tom Pearson
#' @description This function calculates survey weights for Farm Practices
#'   Survey (FPS) data by determining the number of observations per stratum,
#'   joining it to the population data, and calculating weights as the ratio of
#'   the population size to the number of observations. The population column is
#'   then joined back into the dataset, and the strata are converted to factors.
#'
#' @param .data A data frame containing survey data. Must include the columns
#'   specified by `unique_ref_col` and `join_col`.
#' @param pop_df A data frame containing population data by stratum. Must
#'   include columns specified by `join_col` and `pop_col`.
#' @param pop_col A string specifying the column in `pop_df` containing
#'   population sizes for each stratum. Default is `"npop"`.
#' @param unique_ref_col A string specifying the column in `.data` containing
#'   unique identifiers for observations. Default is `"cph_no"`.
#' @param join_col A string specifying the column in `.data` and `pop_df` used
#'   to join the datasets (strata column). Default is `"post_strat"`.
#' @return A data frame with the population column (`num_pop`) added to the
#'   input data, and strata (`join_col`) converted to factors. Weights are
#'   calculated but not included in the output, as the `svyr` package
#'   automatically computes these when using `fpc`.
#'
#' @examples
#' #survey_data <- data.frame(
#' #  cph_no = 1:10,
#' #  post_strat = rep(c("A", "B"), each = 5))
#' #
#' #population_data <- data.frame(
#' #  post_strat = c("A", "B"),
#' #  npop = c(100, 200))
#'
#' @importFrom magrittr %>%
#' @export
fps_calc_weights <- function(.data,
                             pop_df,
                             pop_col = "npop",
                             unique_ref_col = "cph_no",
                             join_col = "post_strat") {

  #testing======================================================================
  # .data = s1_data
  # unique_ref_col = "cph_no"
  # join_col = "post_strat"
  # pop_df = pop
  # pop_col = "npop"


  #validation ==================================================================

  # Check .data is a data frame and contains the required columns
  if (!is.data.frame(.data)) {
    cli::cli_abort("`.data` must be a data frame.")
  }

  if (!all(c(unique_ref_col, join_col) %in% names(.data))) {
    cli::cli_abort("`.data` must include the columns {.var {unique_ref_col}}, and {.var {join_col}}.")
  }

  # Check pop_df is a data frame and contains the required columns
  if (!is.data.frame(pop_df)) {
    cli::cli_abort("`pop_df` must be a data frame.")
  }

  if (!all(c(join_col, pop_col) %in% names(pop_df))) {
    cli::cli_abort("`pop_df` must include the columns  {.var {join_col}} and  {.var {pop_col}}.")
  }

  if (!is.numeric(pop_df[[pop_col]])) {
    cli::cli_abort("{.var {pop_col}}` must be numeric.")
  }

  if (!is.character(pop_df[[join_col]])) {
    cli::cli_abort("{.var {join_col}}` must be character class.")
  }

  # Ensure the join column has compatible levels between .data and pop_df
  if (!all(unique(.data[[join_col]]) %in% pop_df[[join_col]])) {
    cli::cli_warn("Some levels of  {.var {join_col}} in `.data` are not present in `pop_df`.")
  }

  #processing===================================================================

  unique_ref_col <- rlang::sym(unique_ref_col)
  join_col <- rlang::sym(join_col)
  pop_col <- rlang::sym(pop_col)

  # Calculate the number of unique observations per stratum
  samp_tot <-
    .data %>%
    dplyr::group_by({{join_col}}) %>%
    dplyr::summarise(no_rows = length({{unique_ref_col}})) %>%
    dplyr::ungroup()

  # Join population data and calculate weights
  tmp_pop <-
    dplyr::left_join(pop_df,
                     samp_tot,
                     by = as.character(join_col)) %>%
    dplyr::mutate(weight = {{pop_col}} / no_rows) %>%
    dplyr::select({{join_col}}, {{pop_col}}, weight)

  # Add population data back to the input data and convert strata to factors
  data <-
    dplyr::left_join(.data,
                     tmp_pop,
                     by = as.character(join_col)) %>%
    dplyr::rename(num_pop = {{pop_col}}) %>%
    dplyr::mutate({{join_col}} := as.factor({{join_col}}))

  return(data)
}
