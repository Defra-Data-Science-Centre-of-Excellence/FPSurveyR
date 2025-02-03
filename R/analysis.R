#' @title FPS: Analysis: Process factors
#' @author Tom Pearson
#' @description This function processes factors in the FPS dataset, including
#'   standard factors (i.e. region, farm type, and farm size) and ad-hoc factors
#'   provided by the user depending on the question. Ad-hoc factors are added to
#'   the dataset, their levels are mapped and ordered, and both standard and
#'   ad-hoc factors are prepared for analysis.
#'
#' @param .data A data frame containing the FPS data.
#' @param question A character string specifying the question of which to
#'   process the factors for.
#' @param factors A character vector of existing factor column names to include
#'   in the analysis.
#' @param factors_list A named list of factors with their levels to filter and
#'   order the factors.
#' @param adhoc_factors_list A named list where each question maps to additional
#'   ad-hoc factor column names. Defaults to `NULL`.
#' @param adhoc_factors_levels_list A named list mapping ad-hoc factor column
#'   names to their levels. Defaults to `NULL`.
#'
#' @return A list with four components:
#' \describe{
#'   \item{data}{The processed data frame with factors formatted.}
#'   \item{factors}{An updated character vector of all factors included in the analysis.}
#'   \item{factors_list}{A filtered and ordered named list of factors with levels used in the analysis.}
#'   \item{factors_list_full}{A complete list of factors (including ad-hoc factors) with their levels.}
#'   }
#'
#' @examples
#' #processed <-
#' #fps_process_factors(
#' #  .data = df,
#' #  question = "Q1",
#' #  factors = c("fps_gor", "fps_slr_name"),
#' #  factors_list = factors_list,
#' #  adhoc_factors_list = adhoc_factors_list,
#' #  adhoc_factors_levels_list = adhoc_factors_levels_list)
#'
#' @export
fps_process_factors <- function(.data,
                                question,
                                factors,
                                factors_list,
                                adhoc_factors_list = NULL,
                                adhoc_factors_levels_list = NULL) {

  #testing======================================================================
  # .data = tmp_data
  # question = q
  # factors = tmp_factors
  # factors_list = tmp_factors_list
  # adhoc_factors_list = adhoc_factors_list
  # adhoc_factors_levels_list = adhoc_factors_levels_list
  #validation===================================================================

  if (!is.data.frame(.data)) {
    cli::cli_abort("`.data` must be a data frame.")
  }

  if (!is.character(question) || length(question) != 1) {
    cli::cli_abort("`question` must be a single character string.")
  }

  if (!is.character(factors)) {
    cli::cli_abort("`factors` must be a character vector.")
  }

  if (!is.list(factors_list)) {
    cli::cli_abort("`factors_list` must be a list.")
  }

  if (!is.null(adhoc_factors_list) && !is.list(adhoc_factors_list)) {
    cli::cli_abort("`adhoc_factors_list` must be NULL or a list.")
  }

  if (!is.null(adhoc_factors_levels_list) && !is.list(adhoc_factors_levels_list)) {
    cli::cli_abort("`adhoc_factors_levels_list` must be NULL or a list.")
  }

  #processing===================================================================

  factors_list_full <- factors_list #NB: this is created to provide a complete list of factors to feed into validation later on the analysis (outside this function)

  if((!is.null(adhoc_factors_list) & !is.null(adhoc_factors_levels_list)) &&
     question %in% names(adhoc_factors_list)) {

    for(j in seq_along(adhoc_factors_list[[question]])) {

      #get new factor
      new_factor <- adhoc_factors_list[[question]][[j]]
      new_factor_sym <- rlang::sym(new_factor)

      #adding new factor to list of standard (hard-coded) factors for analysis
      factors <- c(factors, new_factor)

      #getting levels for new factor
      #IMPORTANT: levels must be in correct order
      tmp_levels <- unname(adhoc_factors_levels_list[[new_factor]])
      tmp_levels_lookup <- names(adhoc_factors_levels_list[[new_factor]])

      .data <-
        .data %>%
        dplyr::mutate({{new_factor_sym}} := as.character({{new_factor_sym}}))

      #matching numeric response factor level with english-readable factor level and replacing the numeric value with the english version
      for(k in seq_along(tmp_levels)) {

        tmp_level <- tmp_levels[[k]]
        tmp_level_lookup <- tmp_levels_lookup[[k]]
        .data <-
          .data %>%
          dplyr::mutate({{new_factor_sym}} := dplyr::case_when({{new_factor_sym}} == tmp_level_lookup ~ tmp_level,
                                                               TRUE ~ {{new_factor_sym}}))
      }


      #adding adhoc factors to full factors list
      factors_list_full[[new_factor]] <- tmp_levels

      #filtering adhoc factors (permits analysis even if no responses for a given level are present)
      tmp_levels <- tmp_levels[tmp_levels %in% .data[[new_factor]]]
      factors_list[[new_factor]] <- tmp_levels

      #ordering adhoc factors
      .data <-
        .data %>%
        dplyr::mutate({{new_factor_sym}} := ordered({{new_factor_sym}}, levels = tmp_levels))

    }

  }

  #filtering factors (permits analysis even if no responses for a given level are present)
  factors_list$fps_slr_name <- factors_list$fps_slr_name[factors_list$fps_slr_name %in% .data$fps_slr]
  factors_list$fps_gor <- factors_list$fps_gor[factors_list$fps_gor %in% .data$fps_gor]
  factors_list$fps_robust <- factors_list$fps_robust[factors_list$fps_robust %in% .data$fps_robust]

  #ordering factors
  .data <-
    .data %>%
    dplyr::mutate(fps_slr_name = ordered(fps_slr_name, levels = factors_list$fps_slr_name),
                  fps_gor = ordered(fps_gor, levels = factors_list$fps_gor),
                  fps_robust = ordered(fps_robust, levels = factors_list$fps_robust))


  return(list(data = .data,
              factors = factors,
              factors_list = factors_list,
              factors_list_full = factors_list_full))

}


#' @title FPS: Analysis: Analyse by factor
#' @author Tom Pearson (adapted to srvyr methods)
#' @description Computes means or ratios for the Farm Practices
#'   Survey (FPS) data using a survey design object. This function is designed
#'   for use with `fps_prepare_results`.
#'
#' @param design A survey design object created using the `survey` or `srvyr`
#'   packages.
#' @param factor A character string specifying the name of the grouping variable
#'   in the dataset.
#' @param variable A character string specifying the name of the binary variable
#'   to analyze.
#' @param denominator (Optional) A character string specifying the denominator
#'   variable for ratio calculations. Required if `ratio = TRUE`.
#' @param ratio A logical value indicating whether to calculate ratios (`TRUE`)
#'   or means (`FALSE`). Defaults to `FALSE`.
#' @param factor_col A character string specifying the column name for the
#'   factor in the output. Defaults to `"cat"`.
#'
#' @return A data frame containing the computed statistics, including:
#' \itemize{
#'   \item Unweighted counts.
#'   \item Weighted means or ratios.
#'   \item Confidence intervals.
#'   }
#'
#' @details
#' \itemize{
#'   \item This function assumes the `variable` is binary (e.g. `1` for presence and `0` for absence).
#'   \item Confidence intervals are computed at a 95% confidence level by default.
#'   }
#'
#' @examples
#' #result <-
#' #fps_analyse_by_factor(
#' #  design = fps_design, #Assuming `fps_design` is a survey design object
#' #  factor = "fps_gor",
#' #  variable = "Q1_1",
#' #  ratio = FALSE)
#'
#' @export
fps_analyse_by_factor <- function(design,
                                  factor,
                                  variable,
                                  denominator = NULL,
                                  ratio = FALSE,
                                  factor_col = "cat") {

  #testing======================================================================
  # factor = f
  # variable = v
  # design = design
  # denominator = denominator
  # ratio = ratio

  #validation===================================================================

  if(!inherits(design, "survey.design")) {

    cli::cli_abort("This function must be supplied a survey design object from the {.pkg {'survey'}} or {.pkg {'srvyr'}} packages")

  }

  if(!is.character(factor) || length(factor) != 1) {

    cli::cli_abort("`factor` must be a character vector of length 1")

  }

  if(!is.character(variable) || length(variable) != 1) {

    cli::cli_abort("`variable` must be a character vector of length 1")

  }

  if(!is.null(denominator) && (!is.character(denominator) || length(denominator) != 1)) {

    cli::cli_abort("`denominator` must be NULL, or a character vector of length 1")

  }

  if(!is.logical(ratio) || length(ratio) != 1) {

    cli::cli_abort("`ratio` must be a boolean vector of length 1 (TRUE or FALSE)")

  }

  if(ratio == TRUE && is.null(denominator)) {

    cli::cli_abort("Please supply a denominator to calculate ratios against (i.e. ratio == TRUE and denominator is empty/NULL)")

  }

  if(ratio == FALSE && !is.null(denominator)) {

    cli::cli_abort("Please do not supply a denominator when calculating means (i.e. ratio == FALSE and denominator is not empty/NULL)")

  }

  if(!is.character(factor_col) || length(factor_col) != 1) {

    cli::cli_abort("`factor_col` must be a character vector of length 1")

  }

  #set variables================================================================

  factor <- rlang::sym(factor)
  factor_col <- rlang::sym(factor_col)
  var <- rlang::sym(variable)
  nobs_name <- rlang::sym(paste0(variable, "_nobs"))

  var_type <- "ci"
  ci_level <- 0.95

  #calculate means==============================================================
  if(ratio == FALSE && is.null(denominator)) {

    method <- "mean"
    stat_name <- rlang::sym(paste0(variable, "_", method))

    all_res <-
      design %>%
      dplyr::summarise(
        # produce unweighted counts
        {{nobs_name}} := srvyr::unweighted(sum({{var}} == 1)),
        # produce weighted means and confidence intervals
        {{var}} := srvyr::survey_mean({{var}},
                                      proportion = FALSE,
                                      vartype = var_type,
                                      level = ci_level,
                                      na.rm = TRUE,
                                      df = Inf)) %>%
      dplyr::rename({{stat_name}} := {{var}}) %>%
      dplyr::mutate({{factor_col}} := "All farms") %>%
      dplyr::relocate({{factor_col}}, .before = 1)

    fac_res <-
      design %>%
      dplyr::group_by({{factor}}) %>%
      dplyr::summarise(
        # produce unweighted counts (NB: this assumes data are binary!)
        {{nobs_name}} := srvyr::unweighted(sum({{var}} == 1)),
        # produce weighted means and confidence intervals
        {{var}} := srvyr::survey_mean({{var}},
                                      proportion = FALSE,
                                      vartype = var_type,
                                      level = ci_level,
                                      na.rm = TRUE,
                                      df = Inf)) %>%
      dplyr::rename({{stat_name}} := {{var}},
                    {{factor_col}} := {{factor}})



    #calculate ratios (using new proportion as denominator)=======================
  } else if(ratio == TRUE && !is.null(denominator)) {

    method <- "ratio"
    stat_name <- rlang::sym(paste0(variable, "_", method))
    denominator <- rlang::sym(denominator)

    all_res <-
      design %>%
      dplyr::summarise(
        # produce unweighted counts
        {{nobs_name}} := srvyr::unweighted(sum({{var}} == 1)),
        # produce weighted proportions and confidence intervals
        {{var}} := srvyr::survey_ratio(numerator = {{var}},
                                       denominator = {{denominator}},
                                       vartype = var_type,
                                       level = ci_level,
                                       na.rm = TRUE,
                                       df = Inf)) %>%
      dplyr::rename({{stat_name}} := {{var}}) %>%
      dplyr::mutate({{factor_col}} := "All farms") %>%
      dplyr::relocate({{factor_col}}, .before = 1)

    fac_res <-
      design %>%
      dplyr::group_by({{factor}}) %>%
      dplyr::summarise(
        # produce unweighted counts
        {{nobs_name}} := srvyr::unweighted(sum({{var}} == 1)),
        # produce weighted proportions and confidence intervals
        {{var}} := srvyr::survey_ratio(numerator = {{var}},
                                       denominator = {{denominator}},
                                       vartype = var_type,
                                       level = ci_level,
                                       na.rm = TRUE,
                                       df = Inf)) %>%
      dplyr::rename({{stat_name}} := {{var}},
                    {{factor_col}} := {{factor}})

  }


  #format and bind results======================================================

  fps_out <- rbind(fac_res,
                   all_res)
  ci_name <- rlang::sym(paste0(variable, "_ci"))
  fps_out <-
    fps_out %>%
    dplyr::rename_with(~ "upp", ends_with("upp")) %>%
    dplyr::rename_with(~ "low", ends_with("low")) %>%
    #just get +/- without including stat
    dplyr::mutate({{ci_name}} := (upp - low)/2) %>%
    dplyr::select(-upp, -low)


  return(fps_out)

}

#' @title FPS: Analysis: Prepare results
#' @author Tom Pearson
#' @description This function calculates means or ratios for survey data
#'   in binary format (using `fps_analyse_by_factor`), formats the results, and
#'   saves them to an Excel file. The function iterates over multiple factors
#'   and variables, performing the analysis and saving the results into separate
#'   sheets (1 for each factor) in the output file.
#'
#' @param design A survey design object created using the `survey` or `srvyr`
#'   packages.
#' @param factors A character vector specifying the grouping variables (factors)
#'   for the analysis.
#' @param variables A character vector specifying the binary variables to
#'   analyze.
#' @param denominator (Optional) A character string specifying the denominator
#'   variable for ratio calculations. Required if `ratio = TRUE`.
#' @param ratio A logical value indicating whether to calculate ratios (`TRUE`)
#'   or means (`FALSE`). Defaults to `FALSE`.
#' @param factor_col A character string specifying the column name for factors
#'   in the output. Defaults to `"cat"`.
#' @param excel_file_path A character string specifying the file path where the
#'   Excel workbook will be saved.
#'
#' @return Saves the analysis results into an Excel file at the specified
#'   `excel_file_path`.
#'
#' @details
#' \itemize{
#'   \item Means are calculated when `ratio = FALSE` (default). Ratios are calculated when `ratio = TRUE`, and a denominator variable must be provided.
#'   \item Results are saved in separate sheets named after each factor in the `factors` parameter.
#'   \item Binary variables in `variables` should contain `1` for presence and `0` for absence.
#'   \item For variables with "answered" in their names, warnings from convergence issues are suppressed to avoid irrelevant messages.
#'   }
#'
#' @examples
#' #fps_prepare_results(
#' #  design = fpsdesign,
#' #  factors = c("region", "farm_type"),
#' #  variables = c("use_cover_crops", "answered"),
#' #  denominator = "total_area",
#' #  ratio = TRUE,
#' #  factor_col = "group",
#' #  excel_file_path = "results.xlsx")
#'
#' @export
fps_prepare_results <- function(design,
                                factors,
                                variables,
                                denominator = NULL,
                                ratio = FALSE,
                                factor_col = "cat",
                                excel_file_path) {

  #testing======================================================================
  # factors = tmp_factors
  # variables = questions
  # design = fpsdesign
  # denominator = denom
  # factor_col = "cat"
  # excel_file_path = here::here(results_fp, paste0(q, results_ext, "_results.xlsx"))
  # ratio = ratio

  #validation===================================================================

  if(!inherits(design, "survey.design")) {

    cli::cli_abort("This function must be supplied a survey design object from the {.pkg {'survey'}} or {.pkg {'srvyr'}} packages")

  }

  if(!is.character(factors)) {

    cli::cli_abort("`factors` must be a character vector")

  }

  if(!is.character(variables)) {

    cli::cli_abort("`variables` must be a character vector")

  }

  if(!is.null(denominator) && (!is.character(denominator) || length(denominator) != 1)) {

    cli::cli_abort("`denominator` must be NULL, or a character vector of length 1")

  }

  if(!is.logical(ratio) || length(ratio) != 1) {

    cli::cli_abort("`ratio` must be a boolean vector of length 1 (TRUE or FALSE)")

  }

  if(ratio == TRUE && is.null(denominator)) {

    cli::cli_abort("Please supply a denominator to calculate ratios against (i.e. ratio == TRUE and denominator is empty/NULL)")

  }

  if(ratio == FALSE && !is.null(denominator)) {

    cli::cli_abort("Please do not supply a denominator when calculating means (i.e. ratio == FALSE and denominator is not empty/NULL)")

  }

  if(!is.character(factor_col) || length(factor_col) != 1) {

    cli::cli_abort("`factor_col` must be a character vector of length 1")

  }

  if(!is.character(excel_file_path) || length(excel_file_path) != 1) {

    cli::cli_abort("`excel_file_path` must be a character vector of length 1")

  }

  #set variables================================================================

  #open workbook
  wb <- openxlsx::createWorkbook()

  #analysis=====================================================================

  #initiate list to store results
  res <- list()

  #do analysis...
  #...for each factor
  for (f in factors) {

    #...for each question option/variable
    for (v in variables) {

      # define method based on ratio arg
      method <- ifelse(ratio == TRUE & !is.null(denominator), "ratio", "mean")

      # do the analysis but suppress warnings generated by survey_mean in fps_analyse_by_factor only for the "answered" variables
      ## values for "answered" qs will all be 1 which is why the warnings are generated (glm.fit: algorithm did not converge)
      if(grepl("answered", v)) {

        suppressWarnings(
          # calculate means or ratios using fps_analyse_by_factor
          res[[f]][[v]] <- fps_analyse_by_factor(design = design,
                                                 factor = f,
                                                 variable = v,
                                                 denominator = denominator,
                                                 ratio = ratio)
        )

        # do the same analysis, but do not suppress warnings
        ## warnings (glm.fit: algorithm did not converge) should be generated when qs have no respondents for a certain factor level (i.e. all 0s) - this can be useful information
      } else {

        withCallingHandlers( {

          # Calculate means or ratios using fps_analyse_by_factor
          res[[f]][[v]] <- fps_analyse_by_factor(design = design,
                                                 factor = f,
                                                 variable = v,
                                                 denominator = denominator,
                                                 ratio = ratio)
        },

        warning = function(w) {

          #get error message
          msg <- conditionMessage(w)

          if (grepl("glm.fit: algorithm did not converge", msg, ignore.case = TRUE)) {

            #extract grouping from the error message
            grp <- stringr::str_extract(msg, "(?<=In group [^:]{1,100}: )`[^`]+ = [^`]+`")

            if(is.na(grp)) {

              # Customize and reformat the warning message for when a single group is NOT causing the warning message
              warning(cli::cli_alert_warning("Analysis algorithm did not converge for {.emph {'all groups'}} in {.val {f}} in question {.val {v}}. This is likely due to a lack of respondents."), call. = FALSE)
              tryInvokeRestart("muffleWarning")

            } else {

              # Customize and reformat the warning message for when a single group IS causing the warning message
              warning(cli::cli_alert_warning("Analysis algorithm did not converge for {.val {grp}} in question {.val {v}}. This is likely due to a lack of respondents."), call. = FALSE)
              tryInvokeRestart("muffleWarning")

            }

          } else {

            # Forward other warnings
            warning(msg, call. = FALSE)

          }

        }

        )

      }

    }

  }

  #get results and format=======================================================
  for (i in seq_along(res)) {

    sheet_name <- names(res[i]) #generally sheet_name == factor
    tmp <-
      res[[i]] %>%
      #join all results from each var for the same q together in one table
      purrr::reduce(., dplyr::full_join, by = factor_col) %>%
      #reorder columns
      dplyr::select({{factor_col}},
                    dplyr::ends_with(paste0(method)), #mean/ratio
                    dplyr::ends_with("_nobs"),
                    dplyr::ends_with("_ci"))

    #save formatted results to xlsx=============================================
    #add sheet
    openxlsx::addWorksheet(wb,
                           sheetName = sheet_name)
    #write data to sheet
    openxlsx::writeData(wb,
                        sheet = sheet_name,
                        tmp)
    #save workbook
    openxlsx::saveWorkbook(wb,
                           excel_file_path,
                           overwrite = TRUE)

  }

  cli::cli_alert_success("Analysis result tables were written to XLSX at {.path {excel_file_path}}.")

}

#' @title FPS: Analysis: Read all Excel sheets
#' @author Tom Pearson
#' @description This function reads all sheets from an Excel workbook and
#'   returns them as a list of data frames.
#'
#' @param filename A string specifying the path to the Excel file.
#'
#' @return A named list of data frames, where each element corresponds to a
#'   sheet in the workbook. The names of the list elements are the sheet names.
#'
#' @details Each sheet in the Excel workbook is read into a separate data frame.
#'   The sheets are converted to data frames regardless of their original
#'   format.
#'
#' @examples
#' #sheets <- fps_read_excel_allsheets("data.xlsx")
#'
#' @export
fps_read_excel_allsheets <- function(filename) {

  #validation===================================================================

  if (!is.character(filename) || length(filename) != 1) {
    cli::cli_abort("`filename` must be a single string specifying the path to the Excel file.")
  }

  if (!file.exists(filename)) {
    cli::cli_abort("The file {.path {filename}} does not exist. Please check the path and try again.")
  }

  #read and process sheets======================================================

  # Check if the file has at least one sheet
  sheets <- readxl::excel_sheets(filename)

  if (length(sheets) == 0) {
    cli::cli_abort("The file {.path {filename}} does not contain any sheets.")
  }

  tmp <- lapply(sheets, function(x) readxl::read_excel(filename, sheet = x))

  tmp <- lapply(tmp, as.data.frame)

  names(tmp) <- sheets

  return(tmp)
}

#' @title FPS: Analysis: Add empty factors
#' @author Tom Pearson
#' @description This function ensures that all expected factor levels, including
#'   "All farms," are present in a dataset. If a factor level is missing, an
#'   empty row (filled with `NA`) is added to the dataset and the data are
#'   re-ordered to preserve the dataset dimensions.
#'
#' @param .data A data frame containing the analysis data.
#' @param factor_col A string specifying the column name in `.data` containing
#'   the factor categories. Defaults to `"cat"`.
#' @param factor A string specifying the name of the factor to check for missing
#'   levels.
#' @param factors_list_full A named list of all expected factor levels, where
#'   each name corresponds to a factor, and the value is a vector of levels. The
#'   data are also re-ordered (by row) depending on the order of the supplied
#'   factor levels
#'
#' @return A re-oredered data frame with rows added for any missing factor
#'   levels, filled with `NA` values.
#'
#' @details If a category from `factors_list_full` is missing in the specified
#'   `factor_col`, this function adds an empty row for that category at the
#'   correct location. A warning message is displayed for each missing category
#'   that is added.
#'
#' @examples
#' #data <- data.frame(cat = c("A", "B"), value = c(10, 20))
#' #factors_list_full <- list(my_factor = c("A", "B", "C"))
#' #updated_data <- fps_add_empty_factors(data, factor_col = "cat", factor = "my_factor", factors_list_full = factors_list_full)
#'
#' @export
fps_add_empty_factors <- function(.data,
                                  factor_col = "cat",
                                  factor,
                                  factors_list_full) {

  #testing======================================================================
  # .data = tmp_table
  # factor_col = "cat"
  # factor = f
  # factors_list_full = factors_list_full

  # .data = testing_res
  # factor_col = "cat"
  # factor = "fps_slr_name"
  # factors_list_full = testing_fcts_lvl

  #add empty factors============================================================

  #validation===================================================================
  if (!is.data.frame(.data)) {
    cli::cli_abort("`.data` must be a data frame.")
  }

  if (!is.character(factor_col) || length(factor_col) != 1) {
    cli::cli_abort("`factor_col` must be a single string specifying the column name containing the factors.")
  }

  if (!factor_col %in% colnames(.data)) {
    cli::cli_abort("The specified `factor_col` {.val {factor}} does not exist in the provided data frame.")
  }

  if (!is.character(factor) || length(factor) != 1) {
    cli::cli_abort("`factor` must be a single string specifying the name of the factor.")
  }

  if (!is.list(factors_list_full) || is.null(factors_list_full[[factor]])) {
    cli::cli_abort("The specified `factor` {.val {factor}} is not found in the `factors_list_full`.")
  }

  #adding missing factor categories in analysis as empty rows===================
  cats <- c(unlist(unname(factors_list_full[factor])), "All farms")

  #ensure df order is as expected from order in factors_list supplied
  .data <-
    .data %>%
    dplyr::mutate(order = match(cat, cats)) %>%
    dplyr::arrange(order) %>%
    dplyr::select(-order)

  if(any(!(cats %in% .data[[factor_col]]))) {

    for(c in cats) {

      if(!(c %in% .data[[factor_col]])) {

        cli::cli_alert_warning("Analysis: category {.val {c}} from factor {.val {factor}} is missing from the results - adding row of {.val {NA}} values for factor")

        index <- which(cats == c)

        new_row <-
          data.frame(cat = rep(c, length(.data)-1),
                     names = colnames(.data)[colnames(.data) != factor_col],
                     value = rep(NA, length(.data)-1)) %>%
          tidyr::pivot_wider(names_from = "names", values_from = "value")

        .data <-
          rbind(
            .data[1:index-1,],
            new_row,
            .data[index:nrow(.data), ])

        #ensure df order is as expected from order in factors_list supplied (once missing factor added)
        .data <-
          .data %>%
          dplyr::mutate(order = match(cat, cats)) %>%
          dplyr::arrange(order) %>%
          dplyr::select(-order)

      }

    }

  }

  .data <- as.data.frame(.data)
  rownames(.data) <- seq_len(nrow(.data))

  return(.data)

}

#' @title FPS: Analysis: High-level analysis
#' @author Tom Pearson
#' @description This function performs high-level analysis for the Farm
#'   Practices Survey (FPS). It processes survey data, sets the survey design,
#'   and prepares results using lower-level functions (`fps_process_factors`,
#'   `fps_prepare_results`, and `fps_analyse_by_factor`). The analysis outputs
#'   include formatted results and tables, written to XLSX files for quality
#'   assurance and subsequent use in the RAP process.
#'
#' @param .data A data frame containing the survey data to be analysed. The data
#'   frame must include relevant columns for defining the survey design (e.g.
#'   `cph_no`, `post_strat`, `num_pop`).
#' @param factor_col A character string specifying the column containing the
#'   categorical variable(s) used for grouping in the analysis. Defaults to
#'   `"cat"`. The specified column should exist in `.data`.
#' @param questions_list A named list where each element corresponds to a survey
#'   question. Each element must be a character vector consisting of:
#'   - The question identifier prefixed with `"answered_"` (e.g. `"answered_Q1"`).
#'   - Response options associated with the question.
#' @param standard_factors_list A named list of standard factors used for
#'   grouping analysis. Each element of the list should be a named vector or a
#'   list defining factor levels.
#' @param adhoc_factors_list An optional named list of ad-hoc factors to be
#'   included in the analysis. These factors are added dynamically to
#'   `standard_factors_list`. Defaults to `NULL`.
#' @param adhoc_factors_levels_list An optional named list specifying the levels
#'   for the ad-hoc factors. This must align with `adhoc_factors_list` to ensure
#'   consistency. Defaults to `NULL`.
#' @param ratio Logical. If `TRUE`, the analysis calculates ratios for the
#'   specified questions using a denominator derived from the variable names in
#'   `questions_list`. If `FALSE`, means are calculated. Defaults to
#'   `FALSE`.
#' @param results_fp A character string specifying the file path where
#'   intermediate analysis results will be saved as XLSX files. The path must
#'   exist, and the function appends the results filenames.
#' @param tables_fp A character string specifying the file path where the final
#'   formatted tables will be saved as XLSX files. The path must exist, and the
#'   function appends the tables filenames.
#'
#' @return A named list (`table_list`) containing the formatted results for each
#'   question. Each element is a nested list of data frames, one for each factor
#'   analysed for the respective question.
#'
#' @details The function performs the following steps:
#' \itemize{
#'   \item Validates input arguments and ensures correct structure for all inputs.
#'   \item Iterates through the questions specified in `questions_list` to subset data for each question.
#'   \item Dynamically processes factors, including standard and ad-hoc factors, using `fps_process_factors`.
#'   \item Sets the survey design using `srvyr::as_survey_design`.
#'   \item Computes analysis results and writes intermediate outputs using `fps_prepare_results`.
#'   \item Formats results into tables compatible with further RAP processing and ensures missing factor levels are explicitly represented using `fps_add_empty_factors`.
#'   \item Writes the final tables to XLSX files in the location specified by `tables_fp`.
#'   \item Factors with missing data are assigned `NA` values in outputs.
#'   }
#'
#' @examples
#' #fps_analysis(
#' #  .data = s1_data,
#' #  factor_col = "cat",
#' #  questions_list = s1_questions_list,
#' #  standard_factors_list = factors_list,
#' #  adhoc_factors_list = s1_adhoc_factors_list,
#' #  adhoc_factors_levels_list = s1_adhoc_factors_levels_list,
#' #  ratio = FALSE,
#' #  results_fp = "results/s1_results",
#' #  tables_fp = "tables/s1_tables")
#'
#' @seealso \code{\link[fps_process_factors]{fps_process_factors}},
#'   \code{\link[fps_prepare_results]{fps_prepare_results}},
#'   \code{\link[fps_analyse_by_factor]{fps_analyse_by_factor}}
#'
#' @export
fps_analysis <- function(.data,
                         factor_col = "cat",
                         questions_list,
                         standard_factors_list,
                         adhoc_factors_list = NULL,
                         adhoc_factors_levels_list = NULL,
                         ratio = FALSE,
                         results_fp,
                         tables_fp) {

  #testing======================================================================
  # .data = s1_data
  # factor_col = "cat"
  # questions_list = s1_questions_list
  # standard_factors_list = factors_list
  # results_fp = s1_results_fp
  # tables_fp = s1_tables_fp
  # ratio = FALSE
  # adhoc_factors_list = s1_adhoc_factors_list
  # adhoc_factors_levels_list = s1_adhoc_factors_levels_list

  # .data = s1_land_data
  # factor_col = "cat"
  # questions_list = s1_land_questions_list
  # standard_factors_list = factors_list
  # results_fp = s1_land_results_fp
  # tables_fp = s1_land_tables_fp
  # adhoc_factors_list = s1_land_adhoc_factors_list
  # adhoc_factors_levels_list = s1_land_adhoc_factors_levels_list
  # ratio = TRUE

  # .data = s8_livestock_data
  # factor_col = "cat"
  # questions_list = s8_livestock_questions_list
  # standard_factors_list = factors_list
  # results_fp = s8_livestock_results_fp
  # tables_fp = s8_livestock_tables_fp
  # ratio = TRUE
  # adhoc_factors_list = NULL
  # adhoc_factors_levels_list = NULL

  #validation 1/2===============================================================

  if(!is.data.frame(.data)) {

    cli::cli_abort("This function must be supplied a data frame object")

  }

  if(!is.character(factor_col) || length(factor_col) != 1) {

    cli::cli_abort("`factor_col` must be a character vector of length 1")

  }

  if(!is.list(questions_list)) {

    cli::cli_abort("`questions_list` must be a list")

  }

  if(!is.list(standard_factors_list)) {

    cli::cli_abort("`standard_factors_list` must be a list")

  }

  if(!is.null(adhoc_factors_list) && !is.list(adhoc_factors_list)) {

    cli::cli_abort("`adhoc_factors_list` must be NULL or a list")

  }

  if(!is.null(adhoc_factors_levels_list) && !is.list(adhoc_factors_levels_list)) {

    cli::cli_abort("`adhoc_factors_levels_list` must be NULL or a list")

  }

  if(!is.logical(ratio) || length(ratio) != 1) {

    cli::cli_abort("`ratio` must be a boolean vector of length 1 (TRUE or FALSE)")

  }

  if(!is.character(results_fp) || length(results_fp) != 1) {

    cli::cli_abort("`results_fp` must be a character vector of length 1")

  }

  if(!is.character(tables_fp) || length(tables_fp) != 1) {

    cli::cli_abort("`tables_fp` must be a character vector of length 1")

  }

  #initiate table list to store outputs from loop===============================
  tmp_table_list <- list()

  #set progress bar
  cli::cli_progress_bar("Analysis progress", total = length(questions_list))

  #analysis for loop
  for(i in seq_along(names(questions_list))) {


    #validation 2/2=============================================================
    if(!is.character(questions_list[[i]])) {

      cli::cli_warning("`questions_list` should be a list of character vectors")

    }

    if(sum(grepl("answered", questions_list[[i]])) != 1) {

      cli::cli_abort("Each element of `questions_list` must contain one question prefixed with 'answered_' e.g. 'answered_Q1'")
    }


    #set loop variables=========================================================

    #reset data
    tmp_data <- .data

    #reset standard (hard-coded) factors
    tmp_factors_list <- standard_factors_list
    tmp_factors <- names(standard_factors_list)

    #set question-specific variables
    q <- names(questions_list[i])
    answered_q <- rlang::sym(paste0("answered_", q))
    questions <- questions_list[[q]]
    question_opts <- questions[!grepl("answered", questions)]


    #sub-set data for question==================================================

    #filter data for respondents who answered question of interest
    tmp_data <-
      tmp_data %>%
      dplyr::filter({{answered_q}} == 1) #dynamic variable

    #stop analysis if tmp_data is empty for a question
    if(nrow(tmp_data) == 0) {

      cli::cli_abort("Analysis error: empty table. Check that {.val {q}} was answered by >= 1 respondent")

    }


    #deal with ad-hoc factors===================================================

    #re-factorise data for adhoc factors
    results_process_factors <- fps_process_factors(tmp_data,
                                                   question = q,
                                                   factors = tmp_factors,
                                                   factors_list = tmp_factors_list,
                                                   adhoc_factors_list = adhoc_factors_list,
                                                   adhoc_factors_levels_list = adhoc_factors_levels_list)

    tmp_data <- results_process_factors$data
    tmp_factors <- results_process_factors$factors
    tmp_factors_list <- results_process_factors$factors_list
    factors_list_full <- results_process_factors$factors_list_full #used to add NA rows for empty factor levels later on


    #set the survey design======================================================
    fpsdesign <-
      tmp_data %>%
      srvyr::as_survey_design(ids = cph_no,
                              strata = post_strat,
                              fpc = num_pop,
                              nest = TRUE) # don't think nesting is actually necessary as the data aren't hierarchical? use just makes error estimates more conservative (wider CIs)

    #set method-dependent variables=============================================

    if(ratio == TRUE) {

      #get denominator to use in ratio calculations from the response name using str_extract
      #e.g. column Q1_3 must be called "Q1_3_area" to extract "area" from the name.
      #This assumes that a variable/column called "area" is also in the dataset as those values will be used to calculated the ratio from (used as the denominator)
      results_ext <- stringr::str_extract(questions[2], "([_a-z_A-Z]+)$")
      denom <- stringr::str_replace(results_ext, "_", "")
      method <- "ratio"

    } else {

      denom <- NULL
      results_ext <- ""
      method <- "mean"

    }

    #stats calculation==========================================================

    #prepare results (also calculates stats by referencing fps_analyse_by_factor function) - subset to the sample to use i.e. responders to question
    fps_prepare_results(design = fpsdesign,
                        factors = tmp_factors,
                        variables = questions,
                        denominator = denom,
                        ratio = ratio,
                        excel_file_path = here::here(results_fp, paste0(q, results_ext, "_results.xlsx")))


    #create a list of tables based on output of prepare_results_mean
    tmp_sheets <- fps_read_excel_allsheets(here::here(results_fp, paste0(q, results_ext, "_results.xlsx")))
    tmp_table_list[[i]] <- tmp_sheets

    #name each table in list based on Q it pertains to for explicit and easy reference
    names(tmp_table_list)[i] <- q


    #format results=============================================================
    # #cycle through tables in list (list of lists - one for each [[q]]uestion and then one for each [[f]]actor within each Q) and...
    for(j in seq_along(tmp_factors)) {

      f <- tmp_factors[j]

      #...filter results for table of interest
      tmp_table <- tmp_table_list[[q]][[f]]

      tmp_table <-
        tmp_table %>%

        #...select relevant columns
        dplyr::select({{factor_col}},
                      dplyr::matches(paste0(question_opts, "_", method)),
                      dplyr::matches(paste0("answered_", q, "_nobs")),
                      dplyr::matches(paste0(question_opts, "_ci")))


      #...add empty rows if no respondents for a given factor level
      tmp_table <- fps_add_empty_factors(tmp_table,
                                         factor = f,
                                         factors_list_full = factors_list_full)

      #...put formatted table back into table list
      tmp_table_list[[q]][[f]] <- tmp_table

    }

    #write formatted tables=====================================================

    #write tables (includes separate sheet for each factor)
    openxlsx::write.xlsx(tmp_table_list[[q]], here::here(tables_fp, paste0("table_", q, results_ext, ".xlsx")))

    cli::cli_progress_update()
  }

  cli::cli_alert_success("Analysis done! Outputs can be found in R within the section's `table_list`. Outputs are also written to file locally at: {.path {tables_fp}}.")
  cli::cli_alert_info("The following questions were analysed: {.val {names(tmp_table_list)}}")

  return(tmp_table_list)

}
