#' @title FPS: Time-series: Update time-series
#' @author Tom Pearson
#' @description Updates the time-series data for each table or chart displayed
#'   in the statistical notice. The function reads in existing time-series data
#'   from CSV files and appends the latest survey data taken from the analysis
#'   so the full time-series is updated and ready to be used with the narrative
#'   functions. The updated data are also written back to disk.
#'
#' @param tbl_list A named list of data frames, where each data frame
#'   corresponds to a table's latest survey results.
#' @param questions A character vector of question identifiers to update (e.g.
#'   "Q17").
#' @param special_questions A character vector containing special questions that 
#'   follow a different analysis to others. E.g. the Q7 given Q4 are seperate 
#'   tables in the time series
#' @param survey_year Numeric. The year of the latest survey data being added
#'   (defaults to the current year).
#' @param timeseries_directory A string specifying the directory path containing
#'   the time-series CSV files in Databricks.
#' @param output_directory A string specifying the directory path of your
#'   pipeline's outputs folder in Databricks. 
#'   data is written back to the original directory to overwrite any existing
#'   time-series files there. Defaults to `FALSE`.
#'
#' @return A named list of updated time-series data frames, where each data
#'   frame includes both the time-series data and the latest survey data.
#'
#' @details
#' \itemize{
#'   \item Ensure response option names in the `name_responses` script match the column names in the time-series CSV files.
#'   \item Time-series CSV filenames should correspond to the question identifiers (e.g., "Q17_exclNA.csv").
#'   }
#'
#' @examples
#' #fps_update_timeseries(
#' #  tbl_list = s8_livestock_tbl_list,
#' #  questions = s8_livestock_ts_qs, timeseries_directory = "path/to/timeseries/",
#' #  survey_year = 2023)
#'
#' @export
fps_update_timeseries <- function(tbl_list,
                                  questions,
                                  survey_year = as.numeric(format(Sys.Date(), "%Y")),
                                  timeseries_directory,
                                  output_directory) {

  #testing======================================================================
  # tbl_list = s8_livestock_tbl_list
  # questions = s8_livestock_ts_qs
  # timeseries_directory = ts_dir
  # survey_year = survey_year

  # tbl_list = testing_df
  # questions = "Q1"
  # timeseries_directory = temp_dir
  # survey_year = 2024
  # output_directory <- paste0(tempdir(), "/")
  #validation===================================================================

  if (!is.list(tbl_list)) {
    cli::cli_abort("`tbl_list` must be a named list of data frames.")
  }

  if (!is.character(questions) || length(questions) == 0) {
    cli::cli_abort("`questions` must be a non-empty character vector.")
  }

  if (!is.numeric(survey_year) || length(survey_year) != 1) {
    cli::cli_abort("`survey_year` must be a single numeric value.")
  }

  if (!DefraUtils::exists_dash_dir(timeseries_directory)) {
    cli::cli_abort("`timeseries_directory` must be a valid directory path.")
  }

  if (!is.character(output_directory) ||length(output_directory) != 1 ) {
    cli::cli_abort("`output_directory` must be a character string.")
  }

  if(timeseries_directory == output_directory) {
    cli::cli_abort("`output_directory` must be unique to `timeseries_directory`")
  }


  tmp_tbl_list <- tbl_list

  #set progress bar
  cli::cli_progress_bar("Update time-series progress", total = length(questions))
  for(i in seq_along(questions)) {

    q <- questions[i]

    #READ IN TS###################################################################
    #NOTE: The tables read in are copy-pasted from the time series into separate csv files for each question for speed. Actual values are pasted in, rather than % (i.e. 0.1 instead of 10%)

    tmp_fp <- paste(timeseries_directory, paste0(q, ".csv"), sep ='/')

    if(DefraUtils::exists_dash_file(tmp_fp)) {

      tmp_ts <- DefraUtils::read_csv_from_volume(tmp_fp, show_col_types = FALSE)

      #backing up original time series in subdirectory before modification
      backup_dir <- paste(timeseries_directory, "backup", sep ='/')
      if(!DefraUtils::exists_dash_dir(backup_dir)) {
        DefraUtils::create_dash_dir(backup_dir)
      }
      DefraUtils::write_csv_to_volume(tmp_ts,
        paste(backup_dir, paste0(q, ".csv"), sep = '/'),
        overwrite = TRUE
      )

      #FORMAT TS DATA###############################################################

      #format time series data the same as new data
      suppressWarnings(
        tmp_ts <-
          tmp_ts %>%
          dplyr::mutate(proportion = as.numeric(proportion),
                        ci = as.numeric(ci),
                        year = as.numeric(year),
                        value = proportion*100) %>%
          #IMPORTANT: ensuring only years before the current are contained within the time-series before binding the latest data
          dplyr::filter(year < survey_year)
      )

      ts_col_order <- colnames(tmp_ts)

    } else {

      ts_col_order <- c("response", "year", "proportion",  "ci", "value")

    }


    #BIND TS TO NEW DATA##########################################################
    #convert new data back into proportion values (from %) prior to bind

    tmp_tbl <-
      tmp_tbl_list[[q]] %>%
      #IMPORTANT: ensure only the latest year is used in the rest of the function (in case of accidentally running the function twice, for example)
      dplyr::filter(year == survey_year) %>%
      dplyr::mutate(proportion = value / 100,
                    ci = ci / 100)

    if(DefraUtils::exists_dash_file(tmp_fp)) {

      #match column order
      tmp_tbl <-
        tmp_tbl %>%
        dplyr::select(dplyr::all_of(ts_col_order))

      #bind time series to new data
      tmp_ts <-
        rbind(tmp_ts,
              tmp_tbl)

    } else {

      #reorder cols manually
      tmp_tbl <-
        tmp_tbl %>%
        dplyr::select(!!ts_col_order)

      #if no time series data, just keep latest year
      tmp_ts <- tmp_tbl

    }


    #WRITING TO FILE##############################################################

    tmp_ts_csv <-
      tmp_ts %>%
      dplyr::mutate(response = gsub("\\\n", "", response)) %>%
      dplyr::group_by(response) %>%
      dplyr::arrange(year)

    if(any(is.na(tmp_ts_csv$proportion[tmp_ts_csv$year == survey_year]))) {
      cli::cli_abort(paste0("Update time-series: NC values for the following responses to ", q, " in latest year: '",
                            unique(tmp_ts_csv$response[tmp_ts_csv$value[tmp_ts_csv$year == survey_year] == "nc"]),
                            "' - likely that time series response option names do not match those in the name responses script (i.e. you need to edit the response names in the name responses script so they match the time series)"))
    }


    #writing databricks============================================================
    if(!DefraUtils::exists_dash_dir(output_directory)) {
      DefraUtils::create_dash_dir(output_directory)
    }

    DefraUtils::write_csv_to_volume(tmp_ts_csv, 
      paste(output_directory, paste0(q, ".csv"), sep ='/'),
      overwrite = TRUE
    )

    cli::cli_alert_info(paste0("Time series for ", q, " written at: {.file {output_directory}}."))



    #ADDING BACK IN TO LIST OF TABLES FOR UPDATE#############################

    #add all data back into named list (from "name_responses" script)
    tmp_tbl_list[[q]] <- tmp_ts

    cli::cli_progress_update()

  }

  cli::cli_alert_success("Time series for section updated! Outputs can be found in R within `tbl_list`. Outputs are also written to file locally at: {.file {output_directory}}")

  return(tmp_tbl_list)

}


#' @title FPS: Time-series: Update timeseries template
#' @author Asher Goldfeld
#' @description Updates the timeseries workbook from the template loaded in
#'  the main pipeline script. This is used for the FPS publication. This loads in the
#'  data and also creates the figures for each tiemseries. It dynamically fills
#'  each timeseries and sets the formatting. 
#'
#' @param table_list A named list of data frames, where each data frame
#'   corresponds to a table's latest survey results.
#' @param questions A character vector of question identifiers to update (e.g.
#'   "Q17").
#' @param workbook An openxlsx2 workbook. This should the be 
#'  `fps_timeseries_template.xlsx` in the template folder in the pipeline.
#' @param sheet a character vector signifying the sheet name.
#'
#' @return A named list of updated time-series data frames, where each data
#'   frame includes both the time-series data and the latest survey data.
#'
#' @details
#' \itemize{
#'   \item Ensure response option names in the `name_responses` script match the column names in the time-series CSV files.
#'   \item Time-series CSV filenames should correspond to the question identifiers (e.g., "Q17_exclNA.csv").
#'   }
#'
#' @export
fps_update_timeseries_template <- function(table_list, 
                                           questions, 
                                           workbook, 
                                           sheet) {

  proportion_style <- workbook$get_cell_style(sheet = 'config_sheet', dims = 'B2')
  confidence_interval_style <-workbook$get_cell_style(sheet = 'config_sheet', dims = 'B3')
  col_heading_style <- workbook$get_cell_style(sheet = 'config_sheet', dims = 'B4')
  text_style <- workbook$get_cell_style(sheet = 'config_sheet', dims = 'B5')

  for (i in seq_along(questions)) {
    #Set the question
    q <- names(questions[i])
    #Set the row number to insert the table into
    tmp_rownum <- questions[[q]]
    #Set the table of interest
    q_table <- table_list[[q]]

    #Create the wide table for proportion
    prop_wide_table <- q_table %>%
      dplyr::select('Response options' = response, year, proportion) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = proportion
      ) %>% 
      dplyr::mutate(
        dplyr::across(
          -"Response options",
          ~ if (any(is.na(.x))) {
            tidyr::replace_na(as.character(.x), "[x]")
          } else {
            .x
          }
        )
      )
    #Create the wide table for CI
    ci_wide_table <- q_table %>%
      dplyr::select('Response options' = response, year, ci) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = ci
      ) %>% 
      dplyr::mutate(
        dplyr::across(
          -"Response options",
          ~ if (any(is.na(.x))) {
            tidyr::replace_na(as.character(.x), "[x]")
          } else {
            .x
          }
        )
      )
    
    number_of_columns <- ncol(prop_wide_table)

    # Section row positions
    proportion_heading_row <- tmp_rownum
    proportion_data_row <- proportion_heading_row + 2 #Needed for the blank row in the template

    ci_blank_row <- proportion_data_row + nrow(prop_wide_table) 
    ci_data_row <- ci_blank_row + 1

    # Write proportion values without column names
    workbook$add_data(
      sheet = sheet,
      x = prop_wide_table,
      start_col = 1,
      start_row = proportion_data_row,
      col_names = FALSE
    )$set_cell_style(
      sheet = sheet,
      dims = openxlsx2::wb_dims(
        rows =  1:(nrow(prop_wide_table)) + proportion_data_row - 1,
        cols = 2:ncol(prop_wide_table)
      ),
      style = proportion_style
    )$add_data(
      sheet = sheet,
      x = c('Proportion of holdings'),
      start_col = ncol(prop_wide_table),
      start_row = proportion_heading_row + 1,
      col_names = FALSE
    )$set_cell_style(
      sheet = sheet,
      dims = openxlsx2::wb_dims(
        cols =  ncol(prop_wide_table),
        rows = proportion_heading_row + 1
      ),
      style = text_style
    )
    
    # Write CI values without column names
    workbook$add_data(
      sheet = sheet,
      x = ci_wide_table,
      start_col = 1,
      start_row = ci_data_row,
      col_names = FALSE
    )$set_cell_style(
      sheet = sheet,
      dims = openxlsx2::wb_dims(
        rows = 1:(nrow(ci_wide_table)) + ci_data_row -1,
        cols = 2:ncol(ci_wide_table)
      ),
      style = confidence_interval_style
    )$add_data(
      sheet = sheet,
      x = c('95% Confidence Intervals'),
      start_col = ncol(ci_wide_table),
      start_row = ci_blank_row,
      col_names = FALSE
    )$set_cell_style(
      sheet = sheet,
      dims = openxlsx2::wb_dims(
        cols =  ncol(ci_wide_table),
        rows = ci_blank_row
      ),
      style = text_style
    )
  

    #Add in the col headers
    workbook$add_data(
      sheet = sheet,
      x = matrix(colnames(prop_wide_table), nrow =1),
      start_col = 1,
      start_row = proportion_heading_row,
      col_names = FALSE
    )$set_cell_style(
      sheet = sheet,
      dims = openxlsx2::wb_dims(
        cols = 2:ncol(prop_wide_table),
        rows = proportion_heading_row
      ),
      style = col_heading_style
    )$add_border(
      sheet = sheet,
      dims = openxlsx2::wb_dims(
        cols =  2:ncol(prop_wide_table),
        rows =  1:(2*nrow(prop_wide_table)+3) + proportion_heading_row -1
      ),
      update = TRUE
    )$add_border(
      sheet = sheet,
      dims = openxlsx2::wb_dims(
        cols =  2:ncol(prop_wide_table),
        rows =  1:(nrow(prop_wide_table)+2) + proportion_heading_row -1
      ),
      update = TRUE
    )
  }

  return(workbook)
}