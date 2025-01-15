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
#' @param timeseries_directory A string specifying the directory path containing
#'   the time-series CSV files (which should be on the L drive).
#' @param survey_yr Numeric. The year of the latest survey data being added.
#' @param write_to_file Logical. If `TRUE`, updated time-series data is written
#'   back to the original directory. Defaults to `FALSE`.
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
#' #  survey_yr = 2023, write_to_file = FALSE)
#'
#' @export
fps_update_timeseries <- function(tbl_list,
                                  questions,
                                  timeseries_directory,
                                  survey_yr,
                                  write_to_file = FALSE) {

  #testing======================================================================
  # tbl_list = s8_livestock_tbl_list
  # questions = s8_livestock_ts_qs
  # timeseries_directory = ts_dir
  # survey_yr = survey_year
  # write_to_file = FALSE
  #validation===================================================================

  if (!is.list(tbl_list)) {
    cli::cli_abort("`tbl_list` must be a named list of data frames.")
  }

  if (!is.character(questions) || length(questions) == 0) {
    cli::cli_abort("`questions` must be a non-empty character vector.")
  }

  if (!dir.exists(timeseries_directory)) {
    cli::cli_abort("`timeseries_directory` must be a valid directory path.")
  }

  if (!is.numeric(survey_yr) || length(survey_yr) != 1) {
    cli::cli_abort("`survey_yr` must be a single numeric value.")
  }

  if (!is.logical(write_to_file) || length(write_to_file) != 1) {
    cli::cli_abort("`write_to_file` must be a single logical value.")
  }


  tmp_tbl_list <- tbl_list

  #set progress bar
  cli::cli_progress_bar("Update time-series progress", total = length(questions))
  for(i in seq_along(questions)) {

    q <- questions[i]

    #READ IN TS###################################################################
    #NOTE: The tables read in are copy-pasted from the time series into separate csv files for each question for speed. Actual values are pasted in, rather than % (i.e. 0.1 instead of 10%)

    tmp_fp <- paste0(timeseries_directory, q, ".csv")

    if(file.exists(tmp_fp)) {

      tmp_ts <- readr::read_csv(tmp_fp, show_col_types = FALSE)

      #backing up original time series in subdirectory before modification
      backup_dir <- paste0(timeseries_directory, "backup/")
      if(!dir.exists(backup_dir)) {
        dir.create(backup_dir)
      }
      readr::write_csv(tmp_ts, paste0(backup_dir, q, ".csv"))

      #FORMAT TS DATA###############################################################

      #format time series data the same as new data
      suppressWarnings(
        tmp_ts <-
          tmp_ts %>%
          dplyr::mutate(value = as.numeric(value),
                        ci = as.numeric(ci),
                        year = as.numeric(year)) %>%
          #IMPORTANT: ensuring only years before the current are contained within the time-series before binding the latest data
          dplyr::filter(year < survey_year)
      )

      ts_col_order <- colnames(tmp_ts)

    } else {

      ts_col_order <- c("year", "response", "value", "ci")

    }


    #BIND TS TO NEW DATA##########################################################
    #convert new data back into proportion values (from %) prior to bind

    tmp_tbl <-
      tmp_tbl_list[[q]] %>%
      #IMPORTANT: ensure only the latest year is used in the rest of the function (in case of accidentally running the function twice, for example)
      dplyr::filter(year == survey_year) %>%
      dplyr::mutate(value = value / 100,
                    ci = ci / 100)

    if(file.exists(tmp_fp)) {

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
      dplyr::mutate(response = gsub("\\\n", "", response),
                    value = ifelse(is.na(value), "nc", value),
                    ci = ifelse(is.na(ci), "nc", ci)) %>%
      dplyr::group_by(response) %>%
      dplyr::arrange(year)

    if(any(tmp_ts_csv$value[tmp_ts_csv$year == survey_year] == "nc")) {
      cli::cli_abort(paste0("Update time-series: NC values for the following responses to ", q, " in latest year: '",
                            unique(tmp_ts_csv$response[tmp_ts_csv$value[tmp_ts_csv$year == survey_year] == "nc"]),
                            "' - likely that time series response option names do not match those in the name responses script (i.e. you need to edit the response names in the name responses script so they match the time series)"))
    }

    #writing to L drive=========================================================
    if(write_to_file == TRUE) {

      readr::write_csv(tmp_ts_csv, tmp_fp)

      cli::cli_alert_info(paste0("Time series for ", q, " written to L drive"))


    }

    #writing locally============================================================
    local_dir <- paste0(here::here("outputs", "timeseries"), "/")

    if(!dir.exists(local_dir)) {
      dir.create(local_dir)
    }

    readr::write_csv(tmp_ts_csv, paste0(local_dir, q, ".csv"))


    #ADDING BACK IN TO LIST OF TABLES FOR NARRATIVE#############################

    #final formatting of values (e.g. make % rather than proportion and get rid of NA)
    tmp_ts_narrative <-
      tmp_ts %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(value = value * 100,
                    ci = ci * 100)

    #add all data back into named list (from "name_responses" script)
    tmp_tbl_list[[q]] <- tmp_ts_narrative

    cli::cli_progress_update()

  }

  cli::cli_alert_success(paste0("Time series for section updated! Outputs can be found in R within `tbl_list`. Outputs are also written to file locally at: ", local_dir))

  return(tmp_tbl_list)

}
