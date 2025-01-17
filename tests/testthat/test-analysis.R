testing_svy <-
  data.frame(
    cph_no = c("70000001", "60000001", "40000001", "50000001"),
    h10 = c(100, 200, 50, 150),
    fps_gor = c("North West and Merseyside", "South West", "North East", "Yorkshire and The Humber"),
    fps_slr_name = c("Small", "Large", "Small", "Medium"),
    fps_robust = c("Dairy", "Mixed", "Pigs and poultry", "Pigs and poultry"),
    post_strat = c(15, 8, 10, 10),
    num_pop = c(750, 2000, 600, 600),
    weight = c(750, 2000, 300, 300),
    Q1 = c(1, 2, 3, 1),
    Q4 = c(1, 3, 1, 1),
    Q7 = c(1, 2, 6, 1),
    answered_Q1 = c(1, 1, 1, 1),
    Q1_1 = c(1, 0, 0, 1),
    Q1_2 = c(0, 1, 0, 0),
    Q1_3 = c(0, 0, 1, 0),
    Q1_1_h10 = c(100, 0, 0, 150),
    Q1_2_h10 = c(0, 200, 0, 0),
    Q1_3_h10 = c(0, 0, 50, 0)
  )

testing_dsn <-
  testing_svy %>%
  srvyr::as_survey_design(ids = cph_no,
                          strata = post_strat,
                          fpc = num_pop,
                          nest = TRUE)

testing_fcts_lvl <-
  list(fps_gor = c("North East","North West and Merseyside","Yorkshire and The Humber", "East Midlands","West Midlands","East of England", "South East including London","South West"),
       fps_slr_name = c("Small", "Medium", "Large"),
       fps_robust = c("Cereals", "Other crops", "Pigs and poultry", "Dairy", "Grazing livestock (LFA)", "Grazing livestock (lowland)", "Mixed"))
testing_fcts <- names(testing_fcts_lvl)

testing_fcts_adhoc <- list(Q7 = c("Q4"))
testing_fcts_adhoc_lvl <- list(Q4 = c("1" = "I created the plan myself without advice", "2" = "I created the plan myself with advice", "3" = "The plan was created by an adviser"))


#fps_process_factors############################################################
testthat::test_that("fps_process_factors produces a list", {
  act <-
    fps_process_factors(testing_svy,
                        question = "Q1",
                        factors = testing_fcts,
                        factors_list = testing_fcts_lvl)
  testthat::expect_type(act, "list")
})

testthat::test_that("fps_process_factors produces a list with elements of the correct class", {
  act <-
    fps_process_factors(testing_svy,
                        question = "Q1",
                        factors = testing_fcts,
                        factors_list = testing_fcts_lvl)
  testthat::expect_equal(unname(sapply(act, class)), c("data.frame", "character", "list", "list"))
})

testthat::test_that("fps_process_factors removes missing factors from the factors list", {
  act <-
    fps_process_factors(testing_svy,
                        question = "Q1",
                        factors = testing_fcts,
                        factors_list = testing_fcts_lvl)$factors_list$fps_robust
  exp <-
    c("Pigs and poultry",
      "Dairy",
      "Mixed")
  testthat::expect_equal(act, exp)
})

testthat::test_that("fps_process_factors removes missing adhoc factors from the factors list", {
  act <-
    fps_process_factors(testing_svy,
                        question = "Q7",
                        factors = testing_fcts,
                        factors_list = testing_fcts_lvl,
                        adhoc_factors_list = testing_fcts_adhoc,
                        adhoc_factors_levels_list = testing_fcts_adhoc_lvl)$factors_list$Q4
  exp <-
    c("I created the plan myself without advice",
      "The plan was created by an adviser")
  testthat::expect_equal(act, exp)
})

testthat::test_that("fps_process_factors formats adhoc factors in the data correctly", {
  act <-
    fps_process_factors(testing_svy,
                        question = "Q7",
                        factors = testing_fcts,
                        factors_list = testing_fcts_lvl,
                        adhoc_factors_list = testing_fcts_adhoc,
                        adhoc_factors_levels_list = testing_fcts_adhoc_lvl)$data$Q4 %>%
    as.character()
  exp <-
    factor(c("I created the plan myself without advice",
             "The plan was created by an adviser",
             "I created the plan myself without advice",
             "I created the plan myself without advice")) %>%
    as.character()
  testthat::expect_equal(act, exp)
})

#fps_analyse_by_factor##########################################################
testthat::test_that("fps_analyse_by_factor calculates output correctly in the format expected: means", {
  options(survey.lonely.psu ="remove")
  act <-
    fps_analyse_by_factor(design = testing_dsn,
                          factor = "fps_slr_name",
                          variable = "Q1_1") %>%
    dplyr::mutate(dplyr::across( dplyr::where(is.numeric), ~ round(., digits = 3) ))

  exp <-
    dplyr::tibble(cat = c("Large", "Medium", "Small", "All farms"),
                  Q1_1_nobs = c(0, 1, 1, 2),
                  Q1_1_mean = c(0, 1, 0.714, 0.313),
                  Q1_1_ci = c(0, 0, 0.399, 0.175))

  testthat::expect_equal(act, exp)
})

testthat::test_that("fps_analyse_by_factor calculates output correctly in the format expected: means (answered Q)", {
  options(survey.lonely.psu ="remove")
  act <-
    fps_analyse_by_factor(design = testing_dsn,
                          factor = "fps_slr_name",
                          variable = "answered_Q1") %>%
    dplyr::mutate(dplyr::across( dplyr::where(is.numeric), ~ round(., digits = 3) ))

  exp <-
    dplyr::tibble(cat = c("Large", "Medium", "Small", "All farms"),
                  answered_Q1_nobs = c(1, 1, 2, 4),
                  answered_Q1_mean = c(1, 1, 1, 1),
                  answered_Q1_ci = c(0, 0, 0, 0))

  testthat::expect_equal(act, exp)
})

testthat::test_that("fps_analyse_by_factor calculates output correctly in the format expected: ratios", {
  options(survey.lonely.psu ="remove")
  act <-
    fps_analyse_by_factor(design = testing_dsn,
                          factor = "fps_slr_name",
                          variable = "Q1_1_h10",
                          denominator = "h10",
                          ratio = TRUE) %>%
    dplyr::mutate(dplyr::across( dplyr::where(is.numeric), ~ round(., digits = 3) ))

  exp <-
    dplyr::tibble(cat = c("Large", "Medium", "Small", "All farms"),
                  Q1_1_h10_nobs = c(0, 0, 0, 0),
                  Q1_1_h10_ratio = c(0, 1, 0.833, 0.224),
                  Q1_1_h10_ci = c(0, 0, 0.272, 0.14))

  testthat::expect_equal(act, exp)
})

#fps_prepare_results (and fps_read_excel_allsheets)#############################
testthat::test_that("fps_prepare_results formats the output correctly and fps_read_excel_allsheets reads the data back in", {
  options(survey.lonely.psu ="remove")
  tmp_file <- tempfile(fileext = ".xlsx")
  fps_prepare_results(design = testing_dsn,
                      factors = testing_fcts,
                      variables = c("answered_Q1", "Q1_1", "Q1_2", "Q1_3"),
                      excel_file_path = tmp_file)
  act <-
    fps_read_excel_allsheets(tmp_file)$fps_slr_name %>%
    dplyr::mutate(dplyr::across( dplyr::where(is.numeric), ~ round(., digits = 3) ))

  exp <-
    data.frame(cat = c("Large", "Medium", "Small", "All farms"),
               answered_Q1_mean = c(1, 1, 1, 1),

               Q1_1_mean = c(0, 1, 0.714, 0.313),
               Q1_2_mean = c(1, 0, 0, 0.597),
               Q1_3_mean = c(0, 0, 0.286, 0.090),

               answered_Q1_nobs = c(1, 1, 2, 4),
               Q1_1_nobs = c(0, 1, 1, 2),
               Q1_2_nobs = c(1, 0, 0, 1),
               Q1_3_nobs = c(0, 0, 1, 1),

               answered_Q1_ci = c(0, 0, 0, 0),
               Q1_1_ci = c(0, 0, 0.399, 0.175),
               Q1_2_ci = c(0, 0, 0, 0),
               Q1_3_ci = c(0, 0, 0.399, 0.175))

  testthat::expect_equal(act, exp)
})

#fps_add_empty_factors##########################################################
testthat::test_that("fps_add_empty_factors adds NA rows for missing factors", {
  testing_res <-
    data.frame(cat = c("Medium", "Small", "All farms"),
               answered_Q1_mean = c(1, 1, 1),

               Q1_1_mean = c(1, 0.714, 0.313),
               Q1_2_mean = c(0, 0, 0.597),
               Q1_3_mean = c(0, 0.286, 0.090),

               answered_Q1_nobs = c(1, 2, 4),
               Q1_1_nobs = c(1, 1, 2),
               Q1_2_nobs = c(0, 0, 1),
               Q1_3_nobs = c(0, 1, 1),

               answered_Q1_ci = c(0, 0, 0),
               Q1_1_ci = c(0, 0.399, 0.175),
               Q1_2_ci = c(0, 0, 0),
               Q1_3_ci = c(0, 0.399, 0.175))
  act <- fps_add_empty_factors(testing_res,
                               factor_col = "cat",
                               factor = "fps_slr_name",
                               factors_list_full = testing_fcts_lvl)
  exp <-
    data.frame(cat = c("Small", "Medium", "Large", "All farms"),
               answered_Q1_mean = c(1, 1, NA, 1),

               Q1_1_mean = c(0.714, 1, NA, 0.313),
               Q1_2_mean = c(0, 0, NA, 0.597),
               Q1_3_mean = c(0.286, 0, NA, 0.090),

               answered_Q1_nobs = c(2, 1, NA, 4),
               Q1_1_nobs = c(1, 1, NA, 2),
               Q1_2_nobs = c(0, 0, NA, 1),
               Q1_3_nobs = c(1, 0, NA, 1),

               answered_Q1_ci = c(0, 0, NA, 0),
               Q1_1_ci = c(0.399, 0, NA, 0.175),
               Q1_2_ci = c(0, 0, NA, 0),
               Q1_3_ci = c(0.399, 0, NA, 0.175))
  testthat::expect_equal(act, exp)
})

#fps_analysis###################################################################
testthat::test_that("fps_analysis: everything must work as a whole - Q1 means", {
  tmp_file_res <- tempdir()
  tmp_file_tbl <- tempdir()

  act <-
    fps_analysis(testing_svy,
                 questions_list = list("Q1" = c("answered_Q1", "Q1_1", "Q1_2", "Q1_3")),
                 standard_factors_list = testing_fcts_lvl,
                 results_fp = tmp_file_res,
                 tables_fp = tmp_file_tbl)$Q1$fps_slr_name %>%
    dplyr::mutate(dplyr::across( dplyr::where(is.numeric), ~ round(., digits = 3) ))

  exp <-
    data.frame(cat = c("Small", "Medium", "Large", "All farms"),

               Q1_1_mean = c(0.714, 1, 0, 0.313),
               Q1_2_mean = c(0, 0, 1, 0.597),
               Q1_3_mean = c(0.286, 0, 0, 0.090),

               answered_Q1_nobs = c(2, 1, 1, 4),

               Q1_1_ci = c(0.399, 0, 0, 0.175),
               Q1_2_ci = c(0, 0, 0, 0),
               Q1_3_ci = c(0.399, 0, 0, 0.175))

  testthat::expect_equal(act, exp)
})
