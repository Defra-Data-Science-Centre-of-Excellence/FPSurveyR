#' @title FPS: Name responses: Response lookup as data frame
#' @author Tom Pearson
#' @description Converts a list of questions containing responses (coded and
#'   English-readable) into a structured dataframe. The dataframe is designed
#'   for later use in operations such as `left_join` to match response codes
#'   with their corresponding English-readable response names.
#'
#' @param .data A named list or similar object where each element represents a
#'   question containing named vectors pairing a response option code to its
#'   English-readable response.
#' @param ratio A logical value indicating whether to use "mean" or "ratio" as a
#'   suffix for the response column. Defaults to `FALSE`, which appends "_mean".
#'
#' @return A dataframe with columns:
#' \describe{
#'   \item{response_names}{The original response values extracted from `.data`.}
#'   \item{question}{The question names derived from `.data`.}
#'   \item{response}{The response codes with a suffix ("_mean" or "_ratio").}
#' }
#'
#' @examples
#' #s1_response_lookup_list <- list("Q1" = c("Q1_1" = "Holdings with a nutrient management plan",
#' #                                         "Q1_2" = "Holdings without a nutrient management plan",
#' #                                         "Q1_3" = "Not applicable"))
#' #fps_response_lookup_as_df(s1_response_lookup_list, ratio = TRUE)
#'
#' @export
fps_response_lookup_as_df <- function(.data, ratio = FALSE) {

  #validation===================================================================

  if (!is.list(.data)) {
    stop("`.data` must be a list of questions each containing named vectors pairing a response option code to its English-readable response")
  }

  if (!is.logical(ratio) || length(ratio) != 1) {
    stop("`ratio` must be a single logical value (TRUE or FALSE).")
  }

  #transform data===============================================================

  method <- ifelse(ratio == FALSE, "mean", "ratio")

  tmp_response_df <-
    .data %>%
    unlist() %>%
    as.data.frame(row.names = names(.)) %>%
    dplyr::rename(response_names = 1) %>%
    dplyr::mutate(tmp_col = rownames(.)) %>%
    tidyr::separate(tmp_col, c("question", "response"), sep = "\\.") %>%
    dplyr::mutate(response = paste0(response, "_", method))
  rownames(tmp_response_df) <- 1:nrow(tmp_response_df)

  return(tmp_response_df)

}


#' @title FPS: Name responses: Label the coded responses in the analysis results
#' @author Tom Pearson
#' @description Joins coded variable names from analysis outputs to
#'   human-readable names manually specified in a lookup dataframe, optionally
#'   renames the tables, and processes them for narrative automation later on.
#'
#' @param table_list A list of data frames containing analysis outputs.
#' @param analysis_questions A list or vector of questions analysed (must match
#'   table names in `table_list`).
#' @param response_lookup_df A data frame mapping response codes to
#'   human-readable names, including a "question" column.
#' @param ratio Logical. If `TRUE`, use "ratio" instead of "mean" as the
#'   aggregation method assumed to be used in the analysis. Default is `FALSE`.
#' @param category Character. The category to filter by (e.g. "All farms").
#'   Default is `"All farms"`.
#' @param factor The factor or grouping variable to extract from `table_list`.
#'   Default is `1` (values are the same across all factors of "All farms").
#' @param rename Logical or character. If `TRUE`, attempts to rename the tables
#'   based on extracted response patterns. If a character string, uses it as the
#'   new name. Default is `FALSE`.
#'
#' @return A list of processed data frames with human-readable response names.
#'
#' @examples
#' #result <- fps_name_responses(
#' #  table_list = s8_livestock_table_list,
#' #  analysis_questions = s8_livestock_questions_list,
#' #  response_lookup_df = s8_livestock_response_lookup_df,
#' #  ratio = TRUE,
#' #  category = "dairy",
#' #  factor = "fps_robust",
#' #  rename = TRUE)
#'
#' @export
fps_name_responses <- function(table_list,
                               analysis_questions,
                               response_lookup_df,
                               ratio = FALSE,
                               category = "All farms",
                               factor = 1,
                               rename = FALSE) {

  #testing======================================================================
  # table_list = s8_livestock_table_list
  # analysis_questions = s8_livestock_questions_list
  # response_lookup_df = s8_livestock_response_lookup_df
  # ratio = TRUE
  # category = "All farms"
  # factor = 1
  # rename = TRUE

  # table_list = s8_livestock_table_list
  # analysis_questions = s8_livestock_questions_list[["Q58"]]
  # response_lookup_df = s8_livestock_response_lookup_df[s8_livestock_response_lookup_df$question == "Q58",]
  # ratio = TRUE
  # factor = "fps_robust"
  # category = "dairy"
  # rename = FALSE

  #validation===================================================================

  if (!is.list(table_list)) {
    cli::cli_abort("`table_list` must be a list of data frames.")
  }

  if (!is.data.frame(response_lookup_df)) {
    cli::cli_abort("`response_lookup_df` must be a data frame.")
  }

  if (!("question" %in% names(response_lookup_df))) {
    cli::cli_abort("`response_lookup_df` must contain a column named 'question'.")
  }

  if (!is.logical(ratio)) {
    cli::cli_abort("`ratio` must be a logical value (TRUE or FALSE).")
  }

  if (!is.character(category) || length(category) != 1) {
    cli::cli_abort("`category` must be a single character string.")
  }

  if (!is.numeric(factor) && !is.character(factor)) {
    cli::cli_abort("`factor` must be a numeric or character value.")
  }

  if (!is.logical(rename) && !(is.character(rename) && length(rename) == 1)) {
    cli::cli_abort("`rename` must be a logical value or a single character string.")
  }

  #warning for potential misuse of `factor` and `category`
  if(factor != 1 && category == "All farms") {
    cli::cli_alert_info("If you are trying to grab the table for a specific factor, you likely also want to filter the table by a more specific category (the default is currently 'All farms' for which values are the same across factors)")
  }

  tmp_table_list <- table_list
  method <- ifelse(ratio == FALSE, "mean", "ratio")

  #create new list to store the results which will be used for the narrative
  tmp_tbl_list <- list()
  #for all questions that were put through the analysis (i.e. those in the SX_questions_list which is made in the analysis script)
  for(i in seq_along(analysis_questions)) {

    q_table <- names(tmp_table_list[i])
    #below uses q_table instead of i as the list of response names might be smaller than the list fed into the analysis (avoids having to write out lookup for all questions - only need to do for those we need)
    q_lookup <-
      response_lookup_df %>%
      dplyr::filter(question == q_table) %>%
      dplyr::pull(question) %>%
      unique()
    q_lookup <- ifelse(length(q_lookup) == 0, NA, q_lookup)

    #only make changes if the questions are the same between lists (avoids having to write out lookup for all questions - only need to do for those we need)
    if(q_table == q_lookup &
       !is.na(q_table) & !is.na(q_lookup)) {

      #grab the table associated with the factor input
      #IMPORTANT: the default factor is 1 which grabs the table for the 1st factor
      #IMPORTANT: the default category is "All farms"
      tmp_table <- tmp_table_list[[q_table]][[factor]]

      #filter by category
      tmp_table <-
        tmp_table %>%
        dplyr::filter(grepl(category, cat, ignore.case = TRUE))

      if(nrow(tmp_table) == 0) {
        cli::cli_abort("The table is empty, try inputting a different category or factor")
      }

      #pivot data longer for easier access
      tmp_table <-
        tmp_table %>%
        dplyr::mutate_at(vars(-cat), as.numeric) %>%
        tidyr::pivot_longer(-cat, names_to = "var", values_to = "val") %>%
        dplyr::filter(!grepl("nobs", var)) %>%
        tidyr::separate(var, into = c("response", "names"), sep = "_(?=[^_]+$)") %>% #"_(?=[^_]+$)" only matches last underscore in string (i.e. the _ before "mean" or "ratio)
        dplyr::mutate(year = survey_year) %>%
        #converting value from proportion to percentage for use with narrative functions
        dplyr::mutate(val = val * 100) %>%
        dplyr::mutate(names = tolower(names),
                      names = ifelse(!grepl("ci", names), "value", names),
                      response = paste0(response, "_", method)) %>%
        tidyr::pivot_wider(names_from = "names", values_from = "val")



      if(rename == TRUE) {

        #rename questions in the list of tables supplied based on the string pattern in the responses within the table in question...
        #NB: rename default is FALSE
        new_name <-
          tmp_table %>%
          #below extracts "dairy_num_ratio" or "beef_num_mean" from the responses in the table for the Q in question (e.g. from  Q58_1_dairy_num_ratio or Q59_1_2_beef_num_mean)
          dplyr::mutate(q_name = stringr::str_extract(response, paste0("([_a-z_A-Z]+)_", method))) %>%
          dplyr::pull(q_name) %>%
          unique()

        if(length(new_name) > 1) {
          cli::cli_abort(paste0("There responses from different questions in the table for ", q, " - the table cannot be renamed"))

        }

        #below replaces "Q" with the new name extracted using the string matching, above
        new_name <- paste0(q_table, new_name)

      } else if(rename != TRUE && rename != FALSE && length(rename) == 1 &&
                is.character(rename) && rename != "") {

        #...else if rename is given a character argument, make new_name equal that instead of extracting new_name from the response
        new_name <- rename

      } else {

        new_name <- q_table

      }



      #create lookup to match code responses to named responses using the dataframe we created earlier
      tmp_names_lookup <-
        response_lookup_df %>%
        dplyr::filter(question == q_table) %>%
        dplyr::select(response, response_names)

      #match response codes to response names
      tmp_table <-
        dplyr::left_join(
          tmp_table,
          tmp_names_lookup,
          by = "response"
        )

      #format table to contain only response names
      tmp_table <-
        tmp_table %>%
        dplyr::select(-response) %>%
        dplyr::rename(response = response_names)


    } else {

      #skip to next question if questions don't align between lists (permits analysis to continue - avoids having to write out lookup for all questions - only need to do for those we need)
      next

    }

    #add table to new list with response codes now named using english-readable responses
    tmp_tbl_list[[new_name]] <- tmp_table

  }

  cli::cli_alert_success("Responses have been named! Outputs can be found in R within `tbl_list`")

  return(tmp_tbl_list)

}
