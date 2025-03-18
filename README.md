
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FPSurveyR

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/FPSurveyR)](https://CRAN.R-project.org/package=FPSurveyR)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

This package contains all the required functions for producing the [Farm
Practices Survey
(FPS)](https://www.gov.uk/government/collections/farm-practices-survey)
statistical notice, dataset and values for the time-series (as CSVs).
The functions are designed to work within the [FPS
pipeline](https://github.com/Defra-Data-Science-Centre-of-Excellence/fps_pipeline)
which is a separate R Project. The pipeline validates and pre-processes
the FPS data exported from MS Access by the June team, analyses the data
to produce means/ratios plus associated confidence intervals, and
undertakes post-processing to format the data correctly for outputs.
This includes using parameterised R Markdown to produce the statistical
report for GOV.UK.

## Installation

### Option 1

You can install the development version of `FPSurveyR` from GitHub with:

``` r
devtools::install_github("Defra-Data-Science-Centre-of-Excellence/FPSurveyR")
```

If trying to install via Github on Defra laptops, you may first need to
configure your http proxy settings. Instructions on how to do this are
contained within the FPS pipeline README.

### Option 2

Alternatively, manually [download the latest
release](https://github.com/Defra-Data-Science-Centre-of-Excellence/FPSurveyR/releases/tag/v1.1.3)
and install using the following code (changing the file path to where
you downloaded the package to):

``` r
install.packages("~/Downloads/FPSurveyR_1.1.3.tar.gz", repos=NULL)
```

You may need to install a few dependencies from CRAN first if they are
missing (e.g. `srvyr` and `afcharts`).

## Example

Load the package:

``` r
library(FPSurveyR)
```
