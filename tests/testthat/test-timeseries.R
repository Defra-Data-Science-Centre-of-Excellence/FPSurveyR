testthat::test_that("fps_update_timeseries: test data loaded and formatted correctly", {

  #conditionally load depending on whether code is being run line by line in interactive session, or sourced in (e.g. as a unit test):
  #interactive
  if (sys.nframe() == 0 && interactive() == TRUE) {

    testing_ts <- readr::read_csv(file.path(getwd(), "tests", "testthat", "testdata", "Q1.csv"))
    saveRDS(testing_ts, file = "./tests/testthat/testdata/test_ts_q1.rds")

    #unit test
  } else {

    testing_ts <- readRDS(system.file("../tests/testthat/testdata/test_ts_q1.rds", package = "FPSurveyR"))

  }

  temp_dir_ts <- paste0(tempdir(), "/")
  readr::write_csv(testing_ts, paste0(temp_dir_ts, "Q1.csv"))

  temp_dir_out <- paste0(tempdir(), "/outputs/timeseries/")


  testing_df <-
    list(Q1 = dplyr::tibble(
      cat = rep("All farms", 3),
      year = rep(2024, 3),
      value = c(31.3, 59.7, 9),
      ci = c(17.5, 0, 17.5),
      response = c("Holdings with a nutrient management plan",
                   "Holdings without a nutrient management plan",
                   "Not applicable")))


  act <- fps_update_timeseries(tbl_list = testing_df,
                               questions = "Q1",
                               survey_year = 2024,
                               timeseries_directory = temp_dir_ts,
                               output_directory = temp_dir_out,
                               write_to_timeseries_directory = FALSE)
  act[["Q1"]] <-
    act[["Q1"]] %>%
    dplyr::mutate(dplyr::across( dplyr::where(is.numeric), ~ janitor::round_half_up(., digits = 1) ))

  exp <-
    list(Q1 = dplyr::tibble(
      year = c(rep(2022, 3), rep(2023, 3), rep(2024, 3)),
      response = rep(c("Holdings with a nutrient management plan",
                       "Holdings without a nutrient management plan",
                       "Not applicable"), 3),
      value = c(53.7, 33.2, 13.1,
                56.3, 30.9, 12.8,
                31.3, 59.7, 9),
      ci = c(2.3, 2.4, 1.9,
             2.2, 2.4, 1.8,
             17.5, 0, 17.5)))


  testthat::expect_equal(act, exp)
})
