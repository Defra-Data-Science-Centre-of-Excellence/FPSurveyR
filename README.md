
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FPSurveyR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/FPSurveyR)](https://CRAN.R-project.org/package=FPSurveyR)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This package contains all the required functions for producing the [Farm
Practices Survey
(FPS)](https://www.gov.uk/government/collections/farm-practices-survey)
staistical notice, dataset and values for the time-series. The functions
are designed to work within the FPS analysis pipeline which is a
separate R Project. The pipeline pre-processes the FPS data exported
from MS Access by the June team, analyses the data to produce
means/ratios plus associated confidence intervals, and undertakes
post-processing to format the data correctly for output using
parameterised R Markdown to produce the statistical report on GOV.UK.

## Installation

You can install the development version of FPSurveyR from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("Defra-Data-Science-Centre-of-Excellence/FPSurveyR")
```

## Example

Load the package:

``` r
library(FPSurveyR)
```

TODO: \* Testing \* Add global options?
