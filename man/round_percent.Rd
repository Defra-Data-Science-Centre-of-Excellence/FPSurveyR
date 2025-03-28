% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/narrative.R
\name{round_percent}
\alias{round_percent}
\title{FPS: Narrative: Convert proportions to rounded percentages}
\usage{
round_percent(val)
}
\arguments{
\item{val}{A numeric vector of decimal values to be rounded to percentages.}
}
\value{
A character vector with the values converted to percentages (e.g.
"25\%" for 0.25).
}
\description{
This function converts a decimal value (e.g. 0.25) into a
percentage (e.g. 25\%) by multiplying by 100 and rounding to the nearest
whole number. The result is formatted as a percentage.
}
\details{
\itemize{
\item The input \code{val} should be numeric and represent decimal values between 0 and 1.
\item The function uses \link[scales]{label_percent} to format the output as percentages, rounding to the nearest whole number.
\item The function uses \link[janitor]{round_half_up} to ensure rounding of halves always rounds up, rather than R's default of down when even.
}
}
\examples{
#round_percent(0.25)  # Returns "25\%"
#round_percent(c(0.1345, 0.155, 0.245))  # Returns "13\%", "16\%", "25\%"

}
\seealso{
\link[scales]{label_percent}, \link[janitor]{round_half_up}
}
\author{
Tom Pearson
}
