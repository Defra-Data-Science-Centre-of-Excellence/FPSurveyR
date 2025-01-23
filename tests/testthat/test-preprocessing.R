#fps_data_to_binary#############################################################
testthat::test_that("fps_data_to_binary formats data correctly", {

  testing_df <-
    data.frame(
      cph_no = c(70000001, 60000001, 40000001, 50000001),
      Q1 = c(1, 3, 2, 1),
      Q1_2 = c("3,2", "1", "1,2", "3")
    )

  testing_qvals <-
    list("Q1" = 1:3,
         "Q1_2" = 1:3)

  act <- fps_data_to_binary(testing_df, testing_qvals)

  exp <- data.frame(
    cph_no = c(70000001, 60000001, 40000001, 50000001),
    Q1 = c(1, 3, 2, 1),
    Q1_2 = c("3,2", "1", "1,2", "3"),

    Q1_v1 = c(1, 0, 0, 1),
    Q1_v2 = c(0, 0, 1, 0),
    Q1_v3 = c(0, 1, 0, 0),

    Q1_2_v1 = c(0, 1, 1, 0),
    Q1_2_v2 = c(1, 0, 1, 0),
    Q1_2_v3 = c(1, 0, 0, 1)
  )
  testthat::expect_equal(act, exp)
})



#fps_format_factors#############################################################
testing_svy <-
  data.frame(
    cph_no = c(70000001, 60000001, 40000001, 50000001),
    region = c("North West (England)", "South West (England)", "North East (England)", "Yorkshire and The Humber"),
    size = c("small", "large", "Small", "medium"),
    type = c("Dairy", "Mixed", "pigs", "poultry"),
    strata = c(15, 8, 10, 10)
  )

testing_pop <-
  data.frame(
    post_strat = c("8", "10", "15"),
    population = c(2000, 600, 750)
  )

act1 <-
  fps_format_factors(testing_svy,
                     region_col = "region",
                     farm_size_col = "size",
                     farm_type_col = "type",
                     strat_col = "strata")

testthat::test_that("fps_format_factors produces data frame", {
  testthat::expect_s3_class(act1, "data.frame")
})
testthat::test_that("fps_format_factors renames columns correctly", {
  testthat::expect_equal(colnames(act1), c("cph_no", "fps_gor", "fps_slr_name", "fps_robust", "post_strat"))
})
testthat::test_that("fps_format_factors formats region correctly", {
  testthat::expect_equal(act1$fps_gor, c("North West and Merseyside", "South West", "North East", "Yorkshire and The Humber"))
})
testthat::test_that("fps_format_factors formats farm size correctly", {
  testthat::expect_equal(act1$fps_slr_name, c("Small", "Large", "Small", "Medium"))
})
testthat::test_that("fps_format_factors formats farm type correctly", {
  testthat::expect_equal(act1$fps_robust, c("Dairy", "Mixed", "Pigs and poultry", "Pigs and poultry"))
})
testthat::test_that("fps_format_factors formats strata as character", {
  testthat::expect_type(act1$post_strat, "character")
})

#fps_calc_weights###############################################################
act2 <- fps_calc_weights(act1,
                         testing_pop,
                         pop_col = "population",
                         unique_ref_col = "cph_no",
                         join_col = "post_strat")

testthat::test_that("fps_calc_weights produces data frame", {
  testthat::expect_s3_class(act2, "data.frame")
})
testthat::test_that("fps_calc_weights renames columns correctly", {
  testthat::expect_equal(colnames(act2), c("cph_no", "fps_gor", "fps_slr_name", "fps_robust", "post_strat", "num_pop", "weight"))
})
testthat::test_that("fps_calc_weights calculates weighting correctly", {
  testthat::expect_equal(act2$weight, c(750, 2000, 300, 300))
})


