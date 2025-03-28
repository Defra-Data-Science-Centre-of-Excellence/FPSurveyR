% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/name_responses.R
\name{fps_response_lookup_as_df}
\alias{fps_response_lookup_as_df}
\title{FPS: Name responses: Response lookup as data frame}
\usage{
fps_response_lookup_as_df(.data, ratio = FALSE)
}
\arguments{
\item{.data}{A named list or similar object where each element represents a
question containing named vectors pairing a response option code to its
English-readable response.}

\item{ratio}{A logical value indicating whether to use "mean" or "ratio" as a
suffix for the response column. Defaults to \code{FALSE}, which appends "_mean".}
}
\value{
A dataframe with columns:
\describe{
\item{response_names}{The original response values extracted from \code{.data}.}
\item{question}{The question names derived from \code{.data}.}
\item{response}{The response codes with a suffix ("_mean" or "_ratio").}
}
}
\description{
Converts a list of questions containing responses (coded and
English-readable) into a structured dataframe. The dataframe is designed
for later use in operations such as \code{left_join} to match response codes
with their corresponding English-readable response names.
}
\examples{
#s1_response_lookup_list <- list("Q1" = c("Q1_1" = "Holdings with a nutrient management plan",
#                                         "Q1_2" = "Holdings without a nutrient management plan",
#                                         "Q1_3" = "Not applicable"))
#fps_response_lookup_as_df(s1_response_lookup_list, ratio = TRUE)

}
\author{
Tom Pearson
}
