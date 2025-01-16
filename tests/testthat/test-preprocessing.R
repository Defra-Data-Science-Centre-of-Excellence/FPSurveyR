testing_df_svy <-
  data.frame(
    cph_no = c(70000001, 60000001, 40000001, 50000001),
    region = c("North West (England)", "South West (England)", "North East (England)", "Yorkshire and The Humber"),
    size = c("small", "large", "Small", "medium"),
    type = c("Dairy", "Mixed", "pigs", "poultry"),
    strata = c(15, 8, 10, 10)
  )

testing_df_pop <-
  data.frame(
    post_strat = c("8", "10", "15"),
    population = c(2000, 600, 750)
  )

#fps_format_factors#############################################################
res1 <-
  fps_format_factors(testing_df_svy,
                     region_col = "region",
                     farm_size_col = "size",
                     farm_type_col = "type",
                     strat_col = "strata")

testthat::test_that("fps_format_factors produces data frame", {
  testthat::expect_s3_class(res1, "data.frame")
})
testthat::test_that("fps_format_factors renames columns correctly", {
  testthat::expect_equal(colnames(res1), c("cph_no", "fps_gor", "fps_slr_name", "fps_robust", "post_strat"))
})
testthat::test_that("fps_format_factors formats region correctly", {
  testthat::expect_equal(res1$fps_gor, c("North West and Merseyside", "South West", "North East", "Yorkshire and The Humber"))
})
testthat::test_that("fps_format_factors formats farm size correctly", {
  testthat::expect_equal(res1$fps_slr_name, c("Small", "Large", "Small", "Medium"))
})
testthat::test_that("fps_format_factors formats farm type correctly", {
  testthat::expect_equal(res1$fps_robust, c("Dairy", "Mixed", "Pigs and poultry", "Pigs and poultry"))
})
testthat::test_that("fps_format_factors formats strata as character", {
  testthat::expect_type(res1$post_strat, "character")
})

#fps_calc_weights###############################################################
res2 <- fps_calc_weights(res1,
                         testing_df_pop,
                         pop_col = "population",
                         unique_ref_col = "cph_no",
                         join_col = "post_strat")

testthat::test_that("fps_calc_weights produces data frame", {
  testthat::expect_s3_class(res2, "data.frame")
})
testthat::test_that("fps_calc_weights renames columns correctly", {
  testthat::expect_equal(colnames(res2), c("cph_no", "fps_gor", "fps_slr_name", "fps_robust", "post_strat", "num_pop", "weight"))
})
testthat::test_that("fps_calc_weights calculates weighting correctly", {
  testthat::expect_equal(res2$weight, c(750, 2000, 300, 300))
})


