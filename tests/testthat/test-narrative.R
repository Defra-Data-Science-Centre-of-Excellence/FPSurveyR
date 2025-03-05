#round_percent##################################################################

testthat::test_that("round_percent rounds correctly", {

  testing_vec <- c(0.255, 0.225, 0.254, -0.5)

  act <- round_percent(testing_vec)

  exp <- c("26%", "23%", "25%", "-50%")
  testthat::expect_equal(act, exp)
})

#round_number###################################################################
testthat::test_that("round_number rounds correctly", {

  testing_vec <- c(25.5, 22.25, -22.5, -22.25, 0.25, 2.25)

  act <- round_number(testing_vec)

  exp <- c("26", "22", "-23", "-22", "0", "2")
  testthat::expect_equal(act, exp)
})

#get_percent####################################################################

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

testthat::test_that("get_percent extracts percent correctly", {

  act <- get_percent(testing_df,
                     questions = "Q1",
                     response_key = "with a nutrient management plan",
                     svy_year = 2024)

  exp <- "31%"

  testthat::expect_equal(act, exp)
})

testthat::test_that("get_percent adds percents correctly", {

  act <- get_percent(testing_df,
                     questions = "Q1",
                     response_key = c("with a nutrient management plan",
                                      "without"),
                     svy_year = 2024)

  exp <- "91%"

  testthat::expect_equal(act, exp)
})

testthat::test_that("get_percent outputs missing questions message correctly", {

  act <- get_percent(testing_df,
                     questions = "Q2",
                     response_key = "self-funded",
                     svy_year = 2024)

  exp <- "**The following questions are not in `.data`: Q2**"

  testthat::expect_equal(act, exp)
})

#get_percent####################################################################

testthat::test_that("get_doc extracts DOC wording correctly: present tense, long", {

  act <- get_doc(testing_df,
                 question = "Q1",
                 response_key = "with a nutrient management plan",
                 svy_years = c(2022, 2023))

  exp <- "an increase from"

  testthat::expect_equal(act, exp)
})

testthat::test_that("get_doc extracts percent correctly", {

  act <- get_doc(testing_df,
                 question = "Q1",
                 response_key = "with a nutrient management plan",
                 svy_years = c(2022, 2023),
                 get_pc = TRUE)

  exp <- "4%"

  testthat::expect_equal(act, exp)
})


testthat::test_that("get_doc orders years correctly", {

  act <- get_doc(testing_df,
                 question = "Q1",
                 response_key = "with a nutrient management plan",
                 svy_years = c(2023, 2022),
                 get_pc = TRUE)

  exp <- "4%"

  testthat::expect_equal(act, exp)
})

testthat::test_that("get_doc processes multiple responses correctly", {

  act <- get_doc(testing_df,
                 question = "Q1",
                 response_key = c("with a nutrient management plan",
                                  "without"),
                 svy_years = c(2023, 2024),
                 get_pc = TRUE)

  exp <- "5%"

  testthat::expect_equal(act, exp)
})

testthat::test_that("get_doc outputs missing questions message correctly", {

  act <- get_doc(testing_df,
                 question = "Q2",
                 response_key = "self-funded",
                 svy_years = c(2023, 2024))

  exp <- "**The following questions are not in `.data`: Q2**"

  testthat::expect_equal(act, exp)
})

#get_name#######################################################################

testthat::test_that("get_name extracts response correctly", {

  act <-  get_name(testing_df,
                   questions = "Q1",
                   ordinal = 1,
                   svy_year = 2024)

  exp <- "holdings without a nutrient management plan"

  testthat::expect_equal(act, exp)
})

testthat::test_that("get_name extracts response correctly: lowest values", {

  act <-  get_name(testing_df,
                   questions = "Q1",
                   ordinal = "last",
                   svy_year = 2024)

  exp <- "not applicable"

  testthat::expect_equal(act, exp)
})

testthat::test_that("get_name extracts response correctly: attempts at selecting beyond table range", {

  act <-  get_name(testing_df,
                   questions = "Q1",
                   ordinal = 4,
                   svy_year = 2024)

  exp <- "not applicable"

  testthat::expect_equal(act, exp)
})

testthat::test_that("get_name extracts percent correctly", {

  act <-  get_name(testing_df,
                   questions = "Q1",
                   ordinal = 1,
                   svy_year = 2024,
                   get_percent = TRUE)

  exp <- "60%"

  testthat::expect_equal(act, exp)
})

testthat::test_that("get_name extracts percent correctly", {

  act <-  get_name(testing_df,
                   questions = "Q1",
                   ordinal = 1,
                   svy_year = 2024,
                   get_percent = TRUE,
                   responses_to_exclude = "without")

  exp <- "31%"

  testthat::expect_equal(act, exp)
})


testthat::test_that("get_name outputs missing questions message correctly", {

  act <- get_name(testing_df,
                  questions = "Q2",
                  ordinal = 1,
                  svy_year = 2024)

  exp <- "**The following questions are not in `.data`: Q2**"

  testthat::expect_equal(act, exp)
})
