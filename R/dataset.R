#' @title FPS: Remove "No" Column
#' @author Tom Pearson
#' @description This function processes a list of tables (`table_list`) by
#'   removing columns corresponding to "No" responses from specified questions,
#'   ensuring data consistency for further analysis. Optionally, data for
#'   questions with "No" responses removed can be combined with similarly
#'   formatted questions
#'
#' @param table_list A named list of lists, where each list element corresponds
#'   to a question and each list element within each question corresponds to a
#'   table of factor-based results.
#' @param questions A character vector specifying question names to process
#'   which should match the names of at least one of the tables in `table_list`.
#' @param questions_to_combine_list An optional named list. The names represent
#'   the parent question to which a related question will be combined to. The
#'   value is a single character vector containing the related question to
#'   combine to the parent question.
#'
#' @return A modified version of `table_list` with processed columns and
#'   optionally combined questions.
#'
#' @examples
#' #table_list <- list(Q9_exclNA = ..., Q18_exclNA = ...)
#' #questions <- c("Q9_exclNA", "Q18_exclNA")
#' #questions_to_combine_list <- list("Q9_exclNA" = "Q13_exclNA")
#' #result <- fps_remove_no_column(table_list, questions, questions_to_combine_list)
#'
#' @export
fps_remove_no_column <- function(table_list, questions, questions_to_combine_list = NULL) {

  #testing======================================================================
  # table_list = s1_table_list
  # questions = c("Q9_exclNA", "Q18_exclNA", "Q20", "Q22")
  # questions_to_combine_list = list("Q9_exclNA" = "Q13_exclNA")

  # table_list = s1_land_table_list
  # questions = s1_land_remove_no_q
  # questions_to_combine_list = list("Q9_exclNA" = "Q13_exclNA")


  #validation===================================================================
  if (!is.list(table_list)) {
    cli::cli_abort("`table_list` must be a named list of tables.")
  }

  if (length(table_list) == 0) {
    cli::cli_abort("`table_list` is empty. Provide at least one table.")
  }

  if (!all(sapply(table_list, is.list))) {
    cli::cli_abort("All elements in `table_list` must be lists.")
  }

  if (!is.character(questions) || length(questions) == 0) {
    cli::cli_abort("`questions` must be a non-empty character vector.")
  }

  if (!all(questions %in% names(table_list))) {
    missing_questions <- questions[!questions %in% names(table_list)]
    cli::cli_abort("The following questions are not in `table_list`: {.var {missing_questions}}")
  }

  if (!is.null(questions_to_combine_list)) {
    if (!is.list(questions_to_combine_list) || length(questions_to_combine_list) == 0) {
      cli::cli_abort("`questions_to_combine_list`, if provided, must be a non-empty named list.")
    }

    if (!all(names(questions_to_combine_list) %in% questions)) {
      invalid_combinations <- names(questions_to_combine_list)[!names(questions_to_combine_list) %in% questions]
      cli::cli_abort("The following questions in `questions_to_combine_list` are not in `questions`: {.var {missing_questions}}")
    }

    if (!all(unlist(questions_to_combine_list) %in% names(table_list))) {
      invalid_combinations <- unlist(questions_to_combine_list)[!unlist(questions_to_combine_list) %in% names(table_list)]
      cli::cli_abort("The following related questions in `questions_to_combine_list` are not in `table_list`: {.var {missing_questions}}")
    }
  }

  #set tmp vars=================================================================
  tmp_table_list <- table_list
  tmp_questions <- questions

  for(i in seq_along(tmp_questions)) {

    q <- tmp_questions[i]
    q_to_rm <- NULL
    q2_to_rm <- NULL

    #do for all factors in question
    tmp_factors <- names(tmp_table_list[[q]])

    for(j in seq_along(tmp_factors)) {

      f <- tmp_factors[j]

      tmp_data <-
        tmp_table_list[[q]][[f]] %>%
        #removes second response for responses cols only (should be No always)
        dplyr::select(cat,
                      dplyr::matches("_1_([a-zA-Z]*_)?(mean|ratio)$"), #matches cols ending with _1 followed by an _[optional alpha-string] and finally _ratio or _mean
                      dplyr::ends_with("_nobs"), #matches cols ending with _nobs
                      dplyr::matches("_1_([a-zA-Z]*_)?ci$")) %>% #matches cols ending with _1 followed by an _[optional alpha-string] and finally _ci
        dplyr::relocate(dplyr::ends_with("_ci"), .after = dplyr::last_col())

      if(ncol(tmp_data) > 4) {
        cli::cli_abort("Resulting data frame has too many columns - you must ensure you only use this function on questions that have Yes/No responses only")
      }

      if(!is.null(questions_to_combine_list) & q %in% names(questions_to_combine_list)) {

        if(length(questions_to_combine_list[[names(questions_to_combine_list == q)]]) > 1) {
          cli::cli_abort("Please only combine a maximum of 2 questions")
        }

        q2 <- questions_to_combine_list[[names(questions_to_combine_list == q)]]

        tmp_data_q2 <-
          tmp_table_list[[q2]][[f]] %>%
          #removes second response for responses cols only (should be No always)
          dplyr::select(cat,
                        dplyr::matches("_1_([a-zA-Z]*_)?(mean|ratio)$"),
                        dplyr::ends_with("_nobs"),
                        dplyr::matches("_1_([a-zA-Z]*_)?ci$")) %>%
          dplyr::relocate(dplyr::ends_with("_ci"), .after = dplyr::last_col())

        if(ncol(tmp_data_q2) > 4) {
          cli::cli_abort("Resulting data frame (for combining question) has too many columns - you must ensure you only use this function on questions that have Yes/No responses only")
        }

        yes_ans_col <-
          tmp_data %>%
          dplyr::select(dplyr::matches("_1_([a-zA-Z]*_)?(mean|ratio)$")) %>%
          names() %>%
          sym()
        yes_ans_col_q2 <-
          tmp_data_q2 %>%
          dplyr::select(dplyr::matches("_1_([a-zA-Z]*_)?(mean|ratio)$")) %>%
          names() %>%
          sym()

        yes_ci_col <-
          tmp_data %>%
          dplyr::select(dplyr::ends_with("_ci")) %>%
          names() %>%
          sym()
        yes_ci_col_q2 <-
          tmp_data_q2 %>%
          dplyr::select(dplyr::ends_with("_ci")) %>%
          names() %>%
          sym()

        tmp_data <-
          dplyr::left_join(tmp_data,
                           tmp_data_q2,
                           by = "cat") %>%
          dplyr::relocate({{yes_ans_col_q2}}, .after = {{yes_ans_col}}) %>%
          dplyr::relocate({{yes_ci_col}}, .before = {{yes_ci_col_q2}})

        q_to_rm <- q
        q2_to_rm <- q2
        new_q <- paste0(gsub("_exclNA", "", q),
                        "x",
                        gsub("_exclNA", "", q2))


      }

      if(!is.null(questions_to_combine_list) & q %in% names(questions_to_combine_list)) {

        tmp_table_list[[new_q]][[f]] <- tmp_data

      } else {

        tmp_table_list[[q]][[f]] <- tmp_data

      }


    }

    tmp_table_list <- tmp_table_list[!names(tmp_table_list) %in% c(q_to_rm, q2_to_rm)]

  }

  return(tmp_table_list)

}

#' @title FPS: make "All farms" table
#' @author Tom Pearson
#' @description This function processes a list of tables and integrates them
#'   with a corresponding lookup list to create "All Farms" summary tables. It
#'   is primarily used for % questions as in section 5 (e.g. Q35; Q39) where a
#'   comparison of the overall results across related questions is required
#'
#' @param table_list A named list of data frames containing the tables to be
#'   processed.
#' @param allfarms_lookup_list A named list where names are the names of the
#'   resulting "All farms" table, and values are named vectors mapping
#'   sub-questions to their descriptive names (response options).
#' @param ratio Logical, defaults to \code{FALSE}. If \code{TRUE}, columns are
#'   referenced assuming the analysis method was "ratio", otherwise "mean" is
#'   used.
#'
#' @return A modified list of tables, including aggregated rows for the "All
#'   Farms" category based on the lookup list.
#'
#' @examples
#' #table_list <- example_table_list
#' #allfarms_lookup_list <-
#' #list("Q35_allfarms" = c("Q35_1" = "Stored under cover",
#' #                        "Q35_2" = "Stored in the open on a concret (impereable) base: covered",
#' #                        "Q35_3" = "Stored in the open on a concret (impereable) base: uncovered",
#' #                        "Q35_4" = "Stored in the open on a field site (no construction base): covered",
#' #                        "Q35_5" = "Stored in the open on a field site (no construction base): uncovered",
#' #                        "Q35_6" = "Store in any other type of facility"))
#' #result <- fps_make_allfarms_table(table_list, allfarms_lookup_list, ratio = FALSE)
#'
#' @export
fps_make_allfarms_table <- function(table_list, allfarms_lookup_list, ratio = FALSE) {

  #testing======================================================================
  # table_list = s5_table_list
  # allfarms_lookup_list = s5_allfarms_list
  # ratio = FALSE

  #validation===================================================================

  # Input validation ==========================================================
  if (!is.list(table_list)) {
    cli::cli_abort("`table_list` must be a list of data frames")
  }

  if (!is.list(allfarms_lookup_list)) {
    cli::cli_abort("`allfarms_lookup_list` must be a named list.")
  }

  if (!is.logical(ratio) || length(ratio) != 1) {
    cli::cli_abort("`ratio` must be a single logical value (TRUE or FALSE).")
  }

  for (q in names(allfarms_lookup_list)) {
    if (!all(names(allfarms_lookup_list[[q]]) %in% names(table_list))) {
      cli::cli_abort("Sub-question(s) in {.val {q}} not found in `table_list`.")
    }
  }

  #processing===================================================================

  tmp_table_list <- table_list
  method <- ifelse(ratio == FALSE, "mean", "ratio")

  for (i in seq_along(allfarms_lookup_list)) {

    q <- names(allfarms_lookup_list[i])
    subqs <- names(allfarms_lookup_list[[q]])
    subq_names <- unname(allfarms_lookup_list[[q]])

    for (j in seq_along(subqs)) {

      subq <- subqs[j]
      subq_name <- subq_names[j]

      tmp <- dplyr::filter(tmp_table_list[[subq]][[1]], cat == "All farms")
      tmp$cat <- subq_name

      n_options <- sum(grepl(method, names(tmp)))

      names(tmp) <- c("cat",
                      paste0(method, "_", q, "_", 1:n_options),
                      paste0("nobs_answered_", q),
                      paste0("CI_", q, "_", 1:n_options))

      tmp_table_list[[q]][["All farms"]] <- rbind(tmp_table_list[[q]][["All farms"]],
                                                  tmp)
    }

  }

  return(tmp_table_list)
}

#' @title FPS: Update Dataset
#' @author Tom Pearson
#' @description This function processes survey results into a workbook,
#'   formatting data and performing validation checks before inserting data into
#'   a specific worksheet at specific row positions in the workbook. Ad-hoc
#'   factors can be added to customize the processing for specific questions.
#'
#' @param table_list A named list of data tables. Each table contains the
#'   results for a specific question and factor.
#' @param questions A character vector of question names corresponding to tables
#'   in `table_list`.
#' @param standard_factors_list A named list of factors and their corresponding
#'   levels for standard questions.
#' @param workbook An `openxlsx` workbook object where the data will be written.
#' @param sheet A character string specifying the name of the worksheet where
#'   the data will be written.
#' @param rownum_list A list of row numbers where each question’s data should be
#'   inserted in the worksheet.
#' @param special_qs A character vector of question names that require special
#'   validation handling.
#' @param adhoc_factors_list An optional named list of ad-hoc factors to be
#'   included for specific questions.
#' @param adhoc_factors_levels_list An optional named list of factor levels
#'   corresponding to `adhoc_factors_list`.
#' @param ratio Logical. If TRUE, calculates ratios; if FALSE, calculates means
#'   (default is FALSE).
#'
#' @return None. Updates the workbook in memory. The workbook must be saved
#'   separately using \link[openxlsx]{saveWorkbook}.
#'
#' @examples
#' #fps_update_dataset(table_list, questions, standard_factors_list, workbook, "Sheet1", rownum_list, special_qs)
#'
#' @seealso \link[openxlsx]{saveWorkbook}
#'
#' @export
fps_update_dataset <- function(table_list, questions, standard_factors_list,
                               workbook, sheet, rownum_list,
                               special_qs, #multiple choice questions and special questions
                               adhoc_factors_list = NULL, adhoc_factors_levels_list = NULL,
                               ratio = FALSE) {

  #testing======================================================================
  # table_list = s2_table_list
  # questions = s2_dataset_q
  # standard_factors_list = factors_list
  # # adhoc_factors_list = list("Q7" = c("Q4"))
  # # adhoc_factors_levels_list = list("Q4" = c("1" = "I created the plan myself without advice",
  # #                                            "2" = "I created the plan myself with advice",
  # #                                            "3" = "The plan was created by an adviser"))
  # special_qs = irregular_qs
  # workbook = wb
  # sheet = s2_sheet
  # rownum_list = s2_rownum_list
  # ratio = FALSE
  # adhoc_factors_list = NULL
  # adhoc_factors_levels_list = NULL

  # table_list = s1_table_list
  # questions = s1_dataset_q
  # standard_factors_list = factors_list
  # adhoc_factors_list = s1_adhoc_factors_list
  # adhoc_factors_levels_list = s1_adhoc_factors_levels_list
  # workbook = wb
  # sheet = s1_sheet
  # rownum_list = s1_rownum_list
  # special_qs = irregular_qs

  #validation (input args)======================================================
  if (!is.list(table_list) || length(table_list) == 0) {
    cli::cli_abort("`table_list` must be a non-empty named list of data tables.")
  }
  if (!is.character(questions) || length(questions) == 0) {
    cli::cli_abort("`questions` must be a non-empty character vector.")
  }
  if (!is.list(standard_factors_list) || length(standard_factors_list) == 0) {
    cli::cli_abort("`standard_factors_list` must be a non-empty named list of factors.")
  }
  if (!inherits(workbook, "Workbook")) {
    cli::cli_abort("`workbook` must be a valid `openxlsx` workbook object.")
  }
  if (!is.character(sheet) || length(sheet) != 1) {
    cli::cli_abort("`sheet` must be a single character string specifying the worksheet name.")
  }
  if (!is.list(rownum_list) || length(rownum_list) != length(questions)) {
    cli::cli_abort("`rownum_list` must be a list of row numbers matching the length of `questions`.")
  }
  if (!is.character(special_qs)) {
    cli::cli_abort("`special_qs` must be a character vector.")
  }
  if (!is.null(adhoc_factors_list) && (!is.list(adhoc_factors_list) || !is.list(adhoc_factors_levels_list))) {
    cli::cli_abort("If provided, `adhoc_factors_list` and `adhoc_factors_levels_list` must be named lists.")
  }
  if (!is.logical(ratio) || length(ratio) != 1) {
    cli::cli_abort("`ratio` must be a single logical value (TRUE or FALSE).")
  }

  #update workbook==============================================================

  #set the data output type (specify which analysis method was used)
  method <- ifelse(ratio == FALSE, "mean", "ratio")

  #set progress bar
  cli::cli_progress_bar("Update dataset progress", total = length(questions))
  for (i in seq_along(questions)) {

    q <- questions[i]
    tmp_rownums <- rownum_list[[i]]
    tmp_factors <- standard_factors_list

    #adding adhoc factors conditional on question
    if((!is.null(adhoc_factors_list) & !is.null(adhoc_factors_levels_list)) && q %in% names(adhoc_factors_list)) {

      for(j in seq_along(adhoc_factors_list[[q]])) {

        #get new factor
        new_factor <- adhoc_factors_list[[q]][[j]]

        #get new factor levels
        new_factor_levels <- unname(adhoc_factors_levels_list[[new_factor]])

        if(new_factor == "All farms") {

          tmp_factors <- list("All farms")
          names(tmp_factors) <- "All farms"

        } else {

          #adding new factor to list of standard (hard-coded) factors
          tmp_factors[[new_factor]] <- new_factor_levels

        }

      }

    }

    for(j in seq_along(tmp_factors)) {

      #getting table for each factor along with equivalent rownumbers
      f <- names(tmp_factors[j])
      tmp_table <- table_list[[q]][[f]]
      tmp_rownum <- tmp_rownums[[j]]






      ###perform final validation checks and label the data if these fail#########
      for(k in 1:nrow(tmp_table)) {

        c <- tmp_table$cat[k]

        #Test 1) if the sum of the means/ratios across all responses doesn't equal 1, print warning and append check to dataset
        tmp_test1 <- tmp_table[k, grepl(method, colnames(tmp_table))]

        if(!(q %in% special_qs | length(tmp_test1) == 1)) {

          tmp_test1 <- rowSums(tmp_test1, na.rm = TRUE)
          tmp_test1 <- abs(unname(tmp_test1))

          if(tmp_test1 < 0.99 | tmp_test1 > 1.01) {
            cli::cli_alert_danger(paste0("Dataset validation: Test 1: sum of row ", method, "s != 1 for {.val {c}} in {.val {f}} for {.val {q}}."))
            tmp_table[k, "test1"] <- paste0("Check - sum of row ", method, "s is ", tmp_test1)

          }

        }


        #Test 2) if the number of observations is fewer than 5, print warning and append check to dataset

        #count how many nobs columns there are, then change the validation method if there are more than one (should only apply to combined tables like Q9xQ13)
        n_nobs_cols <- sum(grepl("nobs", colnames(tmp_table)))

        if(n_nobs_cols == 2) {

          tmp_test2a <- tmp_table[k, grepl("nobs", colnames(tmp_table))][1]
          tmp_test2b <- tmp_table[k, grepl("nobs", colnames(tmp_table))][2]

          if(tmp_test2a < 5 & !is.na(tmp_test2a)) {
            cli::cli_alert_warning("Dataset validation: Test 2: number of observations < 5 for {.val {c}} in {.val {f}} for {.val {q}}.")
            tmp_table[k, "test2a"] <- paste0("Check - number of observations is ", tmp_test2a, " - suppression needed")
          }

          if(tmp_test2b < 5 & !is.na(tmp_test2b)) {
            cli::cli_alert_warning("Dataset validation: Test 2: number of observations < 5 for {.val {c}} in {.val {f}} for {.val {q}}.")
            tmp_table[k, "test2b"] <- paste0("Check - number of observations is ", tmp_test2b, " - suppression needed")
          }

        } else if(n_nobs_cols == 1) {

          tmp_test2 <- tmp_table[k, grepl("nobs", colnames(tmp_table))]

          if(tmp_test2 < 5 & !is.na(tmp_test2)) {
            cli::cli_alert_warning("Dataset validation: Test 2: number of observations < 5 for {.val {c}} in {.val {f}} for {.val {q}}.")
            tmp_table[k, "test2"] <- paste0("Check - number of observations is ", tmp_test2, " - suppression needed")

          }

        } else {

          cli::cli_abort("There are either too many or too few nobs columns in the table - only 1 or 2 (combined questions only) nobs columns are permitted")

        }

      }



      ###write sheet##############################################################
      #NB: this does not save the workbook to file (it only saves it to the wb in R's memory)! The workbook must be saved outside of the function (currently done in the master_fps.R script)
      openxlsx::writeData(wb = workbook,
                          sheet = sheet,
                          x = tmp_table,
                          startCol = 1,
                          startRow = tmp_rownum,
                          colNames = F)

    }

    cli::cli_progress_update()

  }

  cli::cli_alert_success("Dataset for section updated!")
  cli::cli_alert_info("Note that the XLSX is not yet written to file - this is done in the master file by running {.fn {'openxlsx::saveWorkbook'}} once the tables for every section have been updated")


}


