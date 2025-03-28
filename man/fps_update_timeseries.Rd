% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timeseries.R
\name{fps_update_timeseries}
\alias{fps_update_timeseries}
\title{FPS: Time-series: Update time-series}
\usage{
fps_update_timeseries(
  tbl_list,
  questions,
  survey_year = as.numeric(format(Sys.Date(), "\%Y")),
  timeseries_directory,
  output_directory = paste0(here::here("outputs", "timeseries"), "/"),
  write_to_timeseries_directory = FALSE
)
}
\arguments{
\item{tbl_list}{A named list of data frames, where each data frame
corresponds to a table's latest survey results.}

\item{questions}{A character vector of question identifiers to update (e.g.
"Q17").}

\item{survey_year}{Numeric. The year of the latest survey data being added
(defaults to the current year).}

\item{timeseries_directory}{A string specifying the directory path of your
pipeline's outputs folder. Defaults to
\code{"example_project/outputs/timeseries/"}}

\item{write_to_timeseries_directory}{Logical. If \code{TRUE}, updated time-series
data is written back to the original directory to overwrite any existing
time-series files there. Defaults to \code{FALSE}.}
}
\value{
A named list of updated time-series data frames, where each data
frame includes both the time-series data and the latest survey data.
}
\description{
Updates the time-series data for each table or chart displayed
in the statistical notice. The function reads in existing time-series data
from CSV files and appends the latest survey data taken from the analysis
so the full time-series is updated and ready to be used with the narrative
functions. The updated data are also written back to disk.
}
\details{
\itemize{
\item Ensure response option names in the \code{name_responses} script match the column names in the time-series CSV files.
\item Time-series CSV filenames should correspond to the question identifiers (e.g., "Q17_exclNA.csv").
\item If write_to_timeseries_directory is \code{TRUE}, then any results output will overwrite any files in the time-series directory supplied for the given question.
}
}
\examples{
#fps_update_timeseries(
#  tbl_list = s8_livestock_tbl_list,
#  questions = s8_livestock_ts_qs, timeseries_directory = "path/to/timeseries/",
#  survey_year = 2023, write_to_timeseries_directory = FALSE)

}
\author{
Tom Pearson
}
