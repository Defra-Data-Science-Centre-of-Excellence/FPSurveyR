testing_df <-
  list(
    Q1 = dplyr::tibble(
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

#fps_dodged#####################################################################

testthat::test_that("fps_dodged formats table correctly", {

  act <- fps_dodged(testing_df,
                    questions = "Q1",
                    svy_years = c(2022, 2023))

  exp <-
    data.frame(
      Response = c("Holdings with a nutrient management plan",
                   "Holdings without a nutrient management plan",
                   "Not applicable"),
      col1 = c("56%", "31%", "13%"),
      col2 = c("54%", "33%", "13%")) %>%
    knitr::kable(col.names = c("Response", "2023", "2022"))


  testthat::expect_equal(act, exp)
})


#fps_stacked#####################################################################

testthat::test_that("fps_stacked formats table correctly", {

  act <- fps_stacked(testing_df,
                     questions = "Q1",
                     svy_years = c(2022, 2023))

  exp <-
    data.frame(
      Year = c(2022, 2023),
      col1 = c("54%", "56%"),
      col2 = c("33%", "31%"),
      col3 = c("13%", "13%"),
      Total = c("100%", "100%")) %>%
    knitr::kable(col.names = c("Year",
                               "Holdings with a nutrient management plan",
                               "Holdings without a nutrient management plan",
                               "Not applicable",
                               "Total"))


  testthat::expect_equal(act, exp)
})
