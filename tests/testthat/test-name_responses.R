

testthat::test_that("fps_response_lookup_as_df: check list of named responses converts to df correctly", {

  testing_list <-
    list(Q1 = c(Q1_1 = "Holdings with a nutrient management plan",
                Q1_2 = "Holdings without a nutrient management plan",
                Q1_3 = "Not applicable"))

  act <-
    fps_response_lookup_as_df(testing_list)

  exp <-
    data.frame(response_names = c("Holdings with a nutrient management plan",
                                  "Holdings without a nutrient management plan",
                                  "Not applicable"),
               question = rep("Q1", 3),
               response = c("Q1_1_mean",
                            "Q1_2_mean",
                            "Q1_3_mean"))

  testthat::expect_equal(act, exp)
})


testthat::test_that("fps_name_responses: check data are in correct (tidy) format and are named", {

  testing_df <-
    list(Q1 = list(
      fps_slr_name = data.frame(cat = c("Small", "Medium", "Large", "All farms"),

                                Q1_1_mean = c(0.714, 1, 0, 0.313),
                                Q1_2_mean = c(0, 0, 1, 0.597),
                                Q1_3_mean = c(0.286, 0, 0, 0.090),

                                answered_Q1_nobs = c(2, 1, 1, 4),

                                Q1_1_ci = c(0.399, 0, 0, 0.175),
                                Q1_2_ci = c(0, 0, 0, 0),
                                Q1_3_ci = c(0.399, 0, 0, 0.175))))

  testing_lookup <-
    data.frame(response_names = c("Holdings with a nutrient management plan",
                                  "Holdings without a nutrient management plan",
                                  "Not applicable"),
               question = rep("Q1", 3),
               response = c("Q1_1_mean",
                            "Q1_2_mean",
                            "Q1_3_mean"))

  act <- fps_name_responses(table_list = testing_df,
                            analysis_questions = list("Q1" = c("Q1_1", "Q1_2", "Q1_3")),
                            response_lookup_df = testing_lookup,
                            survey_year = 2024)

  exp <-
    list(Q1 = dplyr::tibble(
      cat = rep("All farms", 3),
      year = rep(2024, 3),
      value = c(31.3, 59.7, 9),
      ci = c(17.5, 0, 17.5),
      response = c("Holdings with a nutrient management plan",
                   "Holdings without a nutrient management plan",
                   "Not applicable")))

  testthat::expect_equal(act, exp)
})
