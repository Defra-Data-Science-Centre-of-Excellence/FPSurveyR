#' @title FPS: Render: Delete old versions (of reports/charts)
#' @author Tom Pearson
#' @description This function deletes old versions of reports and charts in the
#'   specified output directory in Databricks if the `delete` parameter is set to `TRUE`. The
#'   deletion is based on a time period defined by `delete_unit` and
#'   `delete_number`. The function is designed to be used prior to rendering a
#'   new report to ensure only the current reports/charts are being used
#'
#' @param out_dir Character. The base directory in Databricks containing the `report` and
#'   `charts` subdirectories.
#' @param report_dir Character. The directory containing `reports`
#' @param chart_dir Character. The directory containing `charts`
#' @param section Character (optional). The specific section or subdirectory
#'   under `report` and `charts` to process. Defaults to `NULL`, meaning no
#'   specific section is targeted.
#' @param delete Logical. If `TRUE`, old files are deleted. Defaults to `FALSE`.
#' @param delete_unit Character. The time unit for the deletion threshold (e.g.
#'   "secs", "mins", "hours", "days", "weeks"). See \link[base]{difftime}.
#'   Defaults to `"hours"`.
#' @param delete_number Numeric. The number of `delete_unit` units defining the
#'   threshold for deletion. Defaults to `12`.
#'
#' @examples
#' #fps_delete_old_versions(out_dir = "/Volumes/prd_dash_lab/<volume>/<folder_path>", section = "01_nutrient_management",
#' #                        delete = TRUE, delete_unit = "days", delete_number = 30)
#'
#' @seealso \link[base]{difftime}
#'
#' @export
fps_delete_old_versions <- function(out_dir,
                                    report_dir,
                                    chart_dir,
                                    section = NULL,
                                    delete = FALSE,
                                    delete_unit = "hours",
                                    delete_number = 12) {

  #validation===================================================================
  if (!is.character(out_dir) || length(out_dir) != 1) {
    cli::cli_abort("`out_dir` must be a single character string.")
  }

  if (!exists_dash_dir(out_dir)) {
    cli::cli_abort("The specified `out_dir` does not exist.")
  }

  if (!is.character(report_dir) || length(report_dir) != 1) {
    cli::cli_abort("`report_dir` must be a single character string.")
  }

  if (!exists_dash_dir(paste(out_dir, report_dir, section, sep = "/"))) {
    cli::cli_abort("The specified `report_dir` does not exist.")
  }

  if (!is.character(chart_dir) || length(chart_dir) != 1) {
    cli::cli_abort("`chart_dir` must be a single character string.")
  }

  if (!exists_dash_dir(paste(out_dir, chart_dir, section, sep = "/"))) {
    cli::cli_abort("The specified `chart_dir` does not exist.")
  }

  if (!is.null(section) && (!is.character(section) || length(section) != 1)) {
    cli::cli_abort("`section`, if provided, must be a single character string.")
  }

  if (!is.logical(delete) || length(delete) != 1) {
    cli::cli_abort("`delete` must be a single logical value (TRUE or FALSE).")
  }

  if (!is.character(delete_unit) || length(delete_unit) != 1) {
    cli::cli_abort("`delete_unit` must be a single character string (e.g. 'secs', 'mins', 'hours', 'days', 'weeks'). See {.fn {difftime}}.")
  }

  valid_units <- c("secs", "mins", "hours", "days", "weeks")
  if (!delete_unit %in% valid_units) {
    cli::cli_abort(paste("`delete_unit` must be one of:", paste(valid_units, collapse = ", ")))
  }

  if (!is.numeric(delete_number) || length(delete_number) != 1 || delete_number <= 0) {
    cli::cli_abort("`delete_number` must be a positive numeric value.")
  }



  #list files===================================================================
  
  report_path <- paste(out_dir, report_dir, section, sep = "/")
  chart_path  <- paste(out_dir, chart_dir, section, sep = "/")

  delete_old_files <- function(path) {

    attempt <- 1
    max_tries <- 5
    interval <- 2

    while (attempt <= max_tries) {

      result <- tryCatch(
        {
          file_info <- purrr::map_dfr(
            brickster::db_volume_list(path)$contents,
            tibble::as_tibble
          ) 

          if (delete && nrow(file_info) > 0) {
            file_info <- file_info |>
              dplyr::filter(!is_directory) |>
              dplyr::mutate(
                last_modified = as.POSIXct(
                  last_modified / 1000,
                  origin = "1970-01-01",
                  tz = "UTC"
                )
              )
              
            files_to_delete <- file_info$path[
              Sys.time() - file_info$last_modified >
                lubridate::duration(
                  delete_number,
                  units = delete_unit
                )
            ]

            if (length(files_to_delete) > 0) {

              purrr::walk(
                files_to_delete,
                brickster::db_volume_delete
              )

            }

          }

          TRUE

        },

        error = function(e) {

          msg <- conditionMessage(e)

          if (grepl("404", msg)) {
            cli::cli_abort(
              paste0(
                "Databricks path does not exist: ",
                path
              )
            )
          }

          if (
            grepl("HTTP/2 stream", msg, ignore.case = TRUE) ||
            grepl("Failed to perform HTTP request", msg)
          ) {
            return(structure("retry", class = "retry"))
          }

          stop(e)

        }

      )

      if (!inherits(result, "retry")) {
        break()
      }

      if (attempt < max_tries) {
        Sys.sleep(interval)
      }

      attempt <- attempt + 1

    }

    if (attempt > max_tries && inherits(result, "retry")) {
      cli::cli_abort("Maximum retries exceeded.")
    }

  }

  # delete report files =======================================================

  delete_old_files(report_path)

  # delete chart files ========================================================

  delete_old_files(chart_path)
}

#' @title FPS: Render: Format Markdown files for GOV.UK
#' @author Tom Pearson
#' @description This function processes `.md` files in a specified output
#'   directory, removing YAML headers and replacing image tags to conform to
#'   GOV.UK formatting standards.
#'
#' @param out_dir_md Character. The directory containing the markdown files.
#'   Defaults to the current directory (`"."`).
#' @param out_dir_figs Character. The directory containing the image files.
#'   Defaults to the current directory (`"."`).
#' @param section Character (optional). A specific subdirectory or section
#'   within the output directories to process. Defaults to `NULL`.
#' @param chart_ext Character. The file extension of chart images (e.g. "svg",
#'   "png"). Defaults to `"svg"`.
#' @param date Character. The date string used to filter `.md` files. Defaults
#'   to today's date in the format `"%d%b%y"` (e.g. "14jan25").
#'
#' @return None. The function modifies `.md` files in place as a side effect.
#' @details
#' \itemize{
#'   \item YAML headers in the `.md` files are removed.
#'   \item Image tags are updated to match GOV.UK standards.
#'   \item Only `.md` (non-README) files created on the specified date are processed.
#'   }
#'
#' @examples
#' #fps_format_md(out_dir_md = "output/report", out_dir_figs = "output/charts", section = "section_name")
#'
#' @export
fps_format_md <- function(out_dir_md = ".",
                          out_dir_figs = ".",
                          section = NULL,
                          chart_ext = "svg",
                          date = tolower(format(Sys.Date(), "%d%b%y"))) {

  #testing======================================================================
  # out_dir_md = paste0(out_dir, "/report/")
  # out_dir_figs = paste0(out_dir, "/charts/")
  # section = "02_anaerobic_digestion"
  # chart_ext = "svg"
  # date = tolower(format(Sys.Date(), "%d%b%y"))

  # out_dir_md = "."
  # out_dir_figs = "."
  # section = NULL
  # chart_ext = "svg"
  # date = tolower(format(Sys.Date(), "%d%b%y"))

  #validation===================================================================
  if (!is.character(out_dir_md) || length(out_dir_md) != 1) {
    cli::cli_abort("`out_dir_md` must be a single character string.")
  }

  if (!is.character(out_dir_figs) || length(out_dir_figs) != 1) {
    cli::cli_abort("`out_dir_figs` must be a single character string.")
  }

  if (!is.null(section) && (!is.character(section) || length(section) != 1)) {
    cli::cli_abort("`section`, if provided, must be a single character string.")
  }

  if (!is.character(chart_ext) || length(chart_ext) != 1) {
    cli::cli_abort("`chart_ext` must be a single character string.")
  }

  if (!is.character(date) || length(date) != 1) {
    cli::cli_abort("`date` must be a single character string.")
  }

  if (!dir.exists(out_dir_md)) {
    cli::cli_abort("The `out_dir_md` directory does not exist.")
  }

  if (!dir.exists(out_dir_figs)) {
    cli::cli_abort("The `out_dir_figs` directory does not exist.")
  }

  #list valid md files==========================================================

  md_files <- list.files(paste0(out_dir_md, section), pattern = paste0(date, "\\.md$")) #only edit latest (today's date) md file so file deletion works for .md files too (otherwise all .md files get modified in latest run and remain undeleted)
  md_files <- md_files[!grepl("README", md_files)]

  if(length(md_files) > 0 ) {

    for(j in seq_along(md_files)){

      md_name <- stringr::str_split(md_files, pattern = "\\.md")[[j]][1]
      md_fp <- paste0(out_dir_md, section, "/", md_files[j])

      ####reads md file-------------------------------------------------------
      text <- readLines(md_fp, warn = FALSE)

      ####remove yaml---------------------------------------------------------
      yaml_end <- (which(text == "---")[2])+1 #gets the second "---" in the character vector (always the end of the yaml)

      if(!is.na(yaml_end)) {

        text <- text[yaml_end:length(text)]

      }

      ####replace default fig paths with GOV.UK attachment formatting---------
      fig_tags <- text[which(grepl('<img src="([^"]+)" style="[^"]+" \\/>', text))]

      #get fig file names
      figs <- list.files(paste0(out_dir_figs, section), pattern = chart_ext)

      #filter figs by fig found listed in .md
      figs_dir <- gsub(here::here(), "..", out_dir_figs)
      figs_pattern <- paste0("<img src=\"", figs_dir, section, "/", figs, "\" style=\"display: block; margin: auto;\" />")
      figs <- figs[figs_pattern %in% fig_tags]

      #add GOV.UK formatting to fig names
      figs <- paste0("[Image: ", figs, "]")

      #replace fig tags with GOV.UK formatted fig tags
      text[which(grepl('<img src="([^"]+)" style="[^"]+" \\/>', text))] <- figs


      ####saves md file-------------------------------------------------------
      cat(text, file = md_fp, sep = "\n")

    }

  }

}


#' @title FPS: Render: Write dataset (XLSX)
#' @author Tom Pearson
#' @description This function saves the given dataset (workbook) to a file in
#'   the specified output directory. The dataset is only saved once per run
#'   regardless of how many sections are being knit. Optionally, old copies of
#'   the dataset can be deleted based on a specified time period.
#'
#' @param out_dir Character. The directory where the dataset should be saved in 
#'   Databricks. Defaults to the current directory (`"."`).
#' @param workbook A `Workbook` object created with the `openxlsx` package. This
#'   is the dataset to save.
#' @param delete Logical. If `TRUE`, old copies of the dataset in the output
#'   directory will be deleted. Defaults to `FALSE`.
#' @param delete_unit Character. The time unit for deleting old files (e.g.,
#'   `"hours"`, `"days"`). Defaults to `"hours"`.
#' @param delete_number Numeric. The number of time units to determine file age
#'   for deletion. Files older than this value will be deleted. Defaults to
#'   `12`.
#' @param date Character. The date string used to name the output file. Defaults
#'   to today's date in the format `"%d%b%y"` (e.g., "14jan25").
#'
#' @details
#' \itemize{
#'   \item The Excel file is saved with the name `fps-ghg-dataset-{date}.xlsx`.
#'   \item Old files with `.xlsx` extensions in the output directory are deleted if `delete` is `TRUE` and they meet the age criteria.
#'   \item File overwriting is enabled for the saved dataset.
#'   }
#'
#' @examples
#' #fps_write_dataset(
#' #  out_dir = 'Volumes/prd_dash_lab/<volume>/<path>/'
#' #  workbook = my_workbook, 
#' #  delete = TRUE, 
#' #  delete_unit = "hours", 
#' #  delete_number = 24)
#'
#' @seealso \link[openxlsx]{saveWorkbook}
#'
#' @export
fps_write_dataset <- function(out_dir = ".",
                              workbook,
                              delete = FALSE,
                              delete_unit = "hours",
                              delete_number = 12,
                              date = tolower(format(Sys.Date(), "%d%b%y"))) {

  #validation===================================================================
  if (!is.character(out_dir) || length(out_dir) != 1) {
    cli::cli_abort("`out_dir` must be a single character string.")
  }

  if (!inherits(workbook, "Workbook")) {
    cli::cli_abort("`workbook` must be a `Workbook` object created with the `openxlsx` package.")
  }

  if (!is.logical(delete) || length(delete) != 1) {
    cli::cli_abort("`delete` must be a single logical value (TRUE or FALSE).")
  }

  if (!is.character(delete_unit) || length(delete_unit) != 1) {
    cli::cli_abort("`delete_unit` must be a single character string.")
  }

  if (!is.numeric(delete_number) || length(delete_number) != 1 || delete_number <= 0) {
    cli::cli_abort("`delete_number` must be a single positive numeric value.")
  }

  if (!is.character(date) || length(date) != 1) {
    cli::cli_abort("`date` must be a single character string.")
  }

  if (!DefraUtils::exists_dash_dir(out_dir)) {
    cli::cli_abort("The `out_dir` directory does not exist.")
  }

  #delete old versions
  attempt <- 1
  max_tries <- 5
  interval <- 2

  while (attempt <= max_tries) {

    result <- tryCatch(
      {
        dataset_f_info <- purrr::map_dfr(
          brickster::db_volume_list(out_dir)$contents,
          tibble::as_tibble
        ) 

        if (delete && nrow(dataset_f_info) > 0) {

          dataset_f_info <- dataset_f_info |>
            dplyr::filter(
              !is_directory,
              grepl("\\.xlsx$", path)
            ) |>
            dplyr::mutate(
              last_modified = as.POSIXct(
                last_modified / 1000,
                origin = "1970-01-01",
                tz = "UTC"
              )
            )
          
          files_to_delete <- dataset_f_info$path[
            Sys.time() - dataset_f_info$last_modified >
              lubridate::duration(delete_number, units = delete_unit)
          ]

          if (length(files_to_delete) > 0) {
            purrr::walk(
              files_to_delete,
              brickster::db_volume_delete
            )
          }
        }

        TRUE

      },

      error = function(e) {

        msg <- conditionMessage(e)

        if (grepl("404", msg)) {
          cli::cli_abort('Databricks path is incorrect')
        }

        if (
          grepl("HTTP/2 stream", msg, ignore.case = TRUE) ||
          grepl("Failed to perform HTTP request", msg)
        ) {
          return(structure("retry", class = "retry"))
        }

        stop(e)

      }

    )

    if (!inherits(result, "retry")) {
      break()
    }

    if (attempt < max_tries) {
      Sys.sleep(interval)
    }

    attempt <- attempt + 1

  }

  if (attempt > max_tries && inherits(result, "retry")) {
     cli::cli_abort("Maximum retries exceeded.")
  }


  ###write dataset==============================================================
  dataset_fp <- paste0(out_dir, "fps-ghg-dataset-", date, ".xlsx")
  DefraUtils::write_xlsx_to_volume(data = workbook,
                                   path = dataset_fp,
                                   overwrite = TRUE)
  cli::cli_alert_success("Data written to XLSX at {.path {dataset_fp}} in Databricks.")
}


#' @title FPS: Render: save files
#' @author Asher Goldfeld
#' @description This functions takes a directory and thyen will save all files
#'  within that dir to Databricks using the `DefraUtils` package. Used as a 
#'  work around for the mark down rendering process which saves locally. Locally
#'  saved markdown outputs and figures are saved to a temp dir and then this
#'  function saves them to Databricks
#'
#' @param temp_dir (chr) directory with files to be saved to Datarbricks
#' @param databricks_dir (chr) directory on databricks the files should be saved to 
#'
#' @examples
#' #fps_save_files(
#' #  temp_dir = here::here(tempdir()) 
#' #  databricks_dir = 'Volumes/prd_dash_lab/<volume>/<path>/'
#' #  workbook = my_workbook, 
#' #  delete = TRUE, 
#' #  delete_unit = "hours", 
#' #  delete_number = 24)
#'
#' @seealso \link[openxlsx]{saveWorkbook},
#'  \link[DefraUtils]{dash_volume_write}
#'
#' @export
fps_save_files <- function(dir) {

}