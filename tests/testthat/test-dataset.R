#fps_remove_no_column###########################################################
testthat::test_that("fps_remove_no_column can remove 'No' and combine questions correctly", {

  testing_tbl_list <-
    list(
      Q9_exclNA = list(fps_slr_name = data.frame(
        cat = c("Small", "Medium", "Large", "All farms"),

        Q9_v1_mean = c(0.714, 1, 0, 0.313),
        Q9_v2_mean = c(0, 0, 1, 0.597),

        answered_Q9_exclNA_nobs = c(2, 1, 1, 4),

        Q9_v1_ci = c(0.399, 0, 0, 0.175),
        Q9_v2_ci = c(0, 0, 0, 0)
      )),
      Q13_exclNA = list(fps_slr_name = data.frame(
        cat = c("Small", "Medium", "Large", "All farms"),

        Q13_v1_mean = c(0, 0.6, 0, 0.4),
        Q31_v2_mean = c(0.9, 0, 0, 0.7),

        answered_Q13_exclNA_nobs = c(2, 1, 1, 4),

        Q13_v1_ci = c(0, 0.2, 0, 0.1),
        Q13_v2_ci = c(0.5, 0, 0, 0.3)
      ))
    )

  act <-
    fps_remove_no_column(table_list = testing_tbl_list,
                         questions = c("Q9_exclNA"),
                         questions_to_combine_list = list("Q9_exclNA" = "Q13_exclNA"))$Q9xQ13$fps_slr_name

  exp <-
    data.frame(
      cat = c("Small", "Medium", "Large", "All farms"),

      Q9_v1_mean = c(0.714, 1, 0, 0.313),
      Q13_v1_mean = c(0, 0.6, 0, 0.4),

      answered_Q9_exclNA_nobs = c(2, 1, 1, 4),
      answered_Q13_exclNA_nobs = c(2, 1, 1, 4),

      Q9_v1_ci = c(0.399, 0, 0, 0.175),
      Q13_v1_ci = c(0, 0.2, 0, 0.1)
    )
  testthat::expect_equal(act, exp)
})


