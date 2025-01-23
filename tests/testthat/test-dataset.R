#fps_remove_no_column###########################################################
testthat::test_that("fps_remove_no_column can remove 'No' and combine questions correctly", {

  testing_tbl_list <-
    list(
      Q9_exclNA = list(fps_slr_name = data.frame(
        cat = c("Small", "Medium", "Large", "All farms"),

        Q9_1_mean = c(0.714, 1, 0, 0.313),
        Q9_2_mean = c(0, 0, 1, 0.597),

        answered_Q9_exclNA_nobs = c(2, 1, 1, 4),

        Q9_1_ci = c(0.399, 0, 0, 0.175),
        Q9_2_ci = c(0, 0, 0, 0)
      )),
      Q13_exclNA = list(fps_slr_name = data.frame(
        cat = c("Small", "Medium", "Large", "All farms"),

        Q13_1_mean = c(0, 0.6, 0, 0.4),
        Q31_2_mean = c(0.9, 0, 0, 0.7),

        answered_Q13_exclNA_nobs = c(2, 1, 1, 4),

        Q13_1_ci = c(0, 0.2, 0, 0.1),
        Q13_2_ci = c(0.5, 0, 0, 0.3)
      ))
    )

  act <-
    fps_remove_no_column(table_list = testing_tbl_list,
                         questions = c("Q9_exclNA"),
                         questions_to_combine_list = list("Q9_exclNA" = "Q13_exclNA"))$Q9xQ13$fps_slr_name

  exp <-
    data.frame(
      cat = c("Small", "Medium", "Large", "All farms"),

      Q9_1_mean = c(0.714, 1, 0, 0.313),
      Q13_1_mean = c(0, 0.6, 0, 0.4),

      answered_Q9_exclNA_nobs = c(2, 1, 1, 4),
      answered_Q13_exclNA_nobs = c(2, 1, 1, 4),

      Q9_1_ci = c(0.399, 0, 0, 0.175),
      Q13_1_ci = c(0, 0.2, 0, 0.1)
    )
  testthat::expect_equal(act, exp)
})

#fps_make_allfarms_table########################################################
#SKIP FOR NOW (applies to s5 only, and no longer needed as of 2025)

#fps_update_dataset#############################################################

testthat::test_that("fps_update_dataset adds data to the dataset template correctly, and that fps_write_dataset can write it to be read in again", {

  #conditionally load depending on whether code is being run line by line in interactive session, or sourced in (e.g. as a unit test):
  #interactive
  if (sys.nframe() == 0 && interactive() == TRUE) {

    testing_wb <- openxlsx::loadWorkbook(file.path(getwd(), "tests", "testthat", "testdata", "fps-ghg-dataset-template.xlsx"))
    # saveRDS(testing_wb, file = "./tests/testthat/testdata/test_dataset.rds")

    #unit test
  } else {

    testing_wb <- readRDS(system.file("../tests/testthat/testdata/test_dataset.rds", package = "FPSurveyR"))
  }

  testing_res <-
    list(
      Q1 = list(fps_slr_name = data.frame(
        cat = c("Small", "Medium", "Large", "All farms"),

        Q1_1_mean = c(0.714, 1, 0, 0.313),
        Q1_2_mean = c(0, 0, 1, 0.597),
        Q1_3_mean = c(0.286, 0, 0, 0.090),

        answered_Q1_nobs = c(2, 1, 1, 4),

        Q1_1_ci = c(0.399, 0, 0, 0.175),
        Q1_2_ci = c(0, 0, 0, 0),
        Q1_3_ci = c(0.399, 0, 0, 0.175))))

  testing_fcts_lvl <-
    list(fps_slr_name = c("Small", "Medium", "Large"))

  testing_questions <-
    list("Q1" = c(7, 12, 22))

  fps_update_dataset(table_list = testing_res,
                     questions = testing_questions,
                     standard_factors_list = testing_fcts_lvl,
                     workbook = testing_wb,
                     sheet = "Nutrient_Management_-_Holdings",
                     special_qs =  c("Q3", "Q4a", "Q4b", "Q7", "Q8", "Q21", "Q9xQ13"))
  temp_dir <- tempdir()
  fps_write_dataset(out_dir = paste0(temp_dir, "/"),
                    workbook = testing_wb,
                    delete = FALSE,
                    date = tolower(format(Sys.Date(), "%d%b%y")))

  act <-
    openxlsx::read.xlsx(paste0(temp_dir, "/", "fps-ghg-dataset-", tolower(format(Sys.Date(), "%d%b%y")), ".xlsx"),
                        sheet = "Nutrient_Management_-_Holdings",
                        rows = c(6:10),
                        cols = c(1:8)) %>%
    janitor::clean_names()

  exp <-
    data.frame(
      farm_size  = c("Small", "Medium", "Large", "All farms"),

      holdings_with_a_nm_plan  = c(0.714, 1, 0, 0.313),
      holdings_without_a_nm_plan = c(0, 0, 1, 0.597),
      not_applicable  = c(0.286, 0, 0, 0.090),

      no_of_responses_used  = c(2, 1, 1, 4),

      holdings_with_a_nm_plan_2 = c(0.399, 0, 0, 0.175),
      holdings_without_a_nm_plan_2  = c(0, 0, 0, 0),
      not_applicable_2 = c(0.399, 0, 0, 0.175))

  testthat::expect_equal(act, exp)
})
