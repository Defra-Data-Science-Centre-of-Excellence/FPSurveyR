#' @title FPS: Narrative: Convert proportions to rounded percentages
#' @author Tom Pearson
#' @description This function converts a decimal value (e.g. 0.25) into a
#'   percentage (e.g. 25%) by multiplying by 100 and rounding to the nearest
#'   whole number. The result is formatted as a percentage.
#'
#' @param val A numeric vector of decimal values to be rounded to percentages.
#'
#' @return A character vector with the values converted to percentages (e.g.
#'   "25%" for 0.25).
#'
#' @details
#' \itemize{
#'   \item The input `val` should be numeric and represent decimal values between 0 and 1.
#'   \item The function uses \link[scales]{label_percent} to format the output as percentages, rounding to the nearest whole number.
#'   \item The function uses \link[janitor]{round_half_up} to ensure rounding of halves always rounds up, rather than R's default of down when even.
#'   }
#'
#' @examples
#' #round_percent(0.25)  # Returns "25%"
#' #round_percent(c(0.1345, 0.155, 0.245))  # Returns "13%", "16%", "25%"
#'
#' @seealso \link[scales]{label_percent}, \link[janitor]{round_half_up}
#'
#' @export
round_percent <- function(val) {

  #validation===================================================================
  if (!is.numeric(val)) {
    cli::cli_abort("Input 'val' must be a numeric vector.")
  }

  if (any(val < -1 | val > 1)) {
    cli::cli_abort("Input 'val' must be decimal values between -1 and 1.")
  }

  #rounding=====================================================================
  val <- janitor::round_half_up(val, digits = 2)
  scales::label_percent(accuracy = 1)(val)
}

#' @title FPS: Narrative: Round numbers (0 dp) and optionally format
#' @author Tom Pearson
#' @description This function rounds numeric values to the nearest whole number
#'   and formats them with optional prefix, suffix, and thousands separator. It
#'   uses the `scales::label_number()` function for formatting.
#'
#' @param val A numeric vector of values to be rounded.
#' @param prefix A character string to be added before the number (optional).
#' @param suffix A character string to be added after the number (optional).
#' @param big.mark A character string to separate thousands (optional, default
#'   is no mark, "").
#'
#' @return A character vector with the values rounded to whole numbers and
#'   formatted according to the specified options.
#'
#' @details
#' \itemize{
#'   \item The `val` input must be numeric.
#'   \item `prefix`, `suffix`, and `big.mark` are optional formatting parameters to control how the numbers are displayed.
#'   \item The function uses \link[scales]{label_number} to format the output, rounding to the nearest whole number.
#'   \item The function uses \link[janitor]{round_half_up} to ensure rounding of halves always rounds up, rather than R's default of down when even.
#'   }
#'
#' @examples
#' #round_number(12345.67)  # Returns "12346"
#' #round_number(12345.67, prefix = "£")  # Returns "£12346"
#' #round_number(12345.67, suffix = " units")  # Returns "12346 units"
#' #round_number(12345.67, big.mark = ",")  # Returns "12,346"
#' #round_number(12345.67, prefix = "£", suffix = " units", big.mark = ",")  # Returns "£12,346 units"
#'
#' @seealso \link[scales]{label_number}, \link[janitor]{round_half_up}
#'
#' @export
round_number <- function(val, prefix = NULL, suffix = NULL, big.mark = "") {

  #validation===================================================================
  if (!is.numeric(val)) {
    cli::cli_abort("Input 'val' must be a numeric vector.")
  }

  if (!is.null(prefix) && !is.character(prefix)) {
    cli::cli_abort("Input 'prefix' must be a character string or NULL.")
  }

  if (!is.null(suffix) && !is.character(suffix)) {
    cli::cli_abort("Input 'suffix' must be a character string or NULL.")
  }

  if (!is.character(big.mark) || length(big.mark) != 1) {
    cli::cli_abort("Input 'big.mark' must be a character string.")
  }

  #round========================================================================
  val <- janitor::round_half_up(val, digits = 0)
  scales::label_number(accuracy = 1,
                       prefix = prefix, suffix = suffix, big.mark = big.mark)(val)
}

#' @title FPS: Narrative: Extract a single percentage from the analysis results
#' @author Tom Pearson
#' @description This function extracts and calculates the percentage value for a
#'   given set of questions and response keys from the analysed survey data.
#'
#' @param .data A list of data frames, each representing survey question data.
#' @param questions A character or numeric vector specifying the question(s) to
#'   process. Numeric input is automatically converted to "QX" format (e.g.
#'   `"Q4"`).
#' @param response_key A character vector of keywords for the desired
#'   response(s). Supports regex patterns for filtering responses.
#' @param svy_year A numeric or character string representing the survey year
#'   (e.g. `2024`).
#'
#' @return A string representing the percentage value(s) for the specified
#'   question(s) and response(s). If multiple responses are provided, their
#'   values are summed.
#'
#' @details
#' \itemize{
#'   \item Rounds values using \link[janitor]{round_half_up} before summing to ensure consistency.
#'   \item Handles filtering of irregular or multiple-choice data with validation.
#'   \item Provides warnings and errors when input parameters are missing or invalid.
#'   }
#'
#' @examples
#' #data_list <- list(Q4 = data.frame(year = "2024", value = c(50, 30),
#' #                                  response = c("yes", "no")))
#' #get_percent(data_list, questions = 4, response_key = "yes", svy_year = "2024")
#'
#' @seealso \link[janitor]{round_half_up}
#'
#' @export
get_percent <- function(.data, questions, response_key, svy_year) {

  #testing======================================================================
  # .data = testing_df
  # questions = "Q1"
  # response_key = "with nutrient management"
  # svy_year = 2024

  #validation===================================================================

  if (missing(.data) || !is.list(.data)) {
    cli::cli_abort("Please supply a valid list of tables.")
  }

  if (missing(questions) || length(questions) == 0) {
    cli::cli_abort("Please supply at least one question number.")
  }

  if (length(questions) > 1) {
    cli::cli_alert_warning("Joining data from multiple questions. Ensure data are comparable.")
  }

  if (missing(response_key) || length(response_key) == 0) {
    cli::cli_abort("Please supply one or more keywords for the question's response.")
  }

  if (missing(svy_year) || length(svy_year) != 1 || !is.character(as.character(svy_year))) {
    cli::cli_abort("Please supply a valid survey year in the correct format (e.g., '2024').")
  }

  # if(any(questions %in% new_qs)) {
  #
  #   return(paste0("**", questions, " NOT COLLECTED IN SURVEY YEAR**"))
  #
  # }

  #ensure question is passed in the format "QX" e.g. "Q4", not "q4" or "4"
  if(is.numeric(questions)) {
    questions <- toupper(paste0("Q", questions))
  }


  if (!all(questions %in% names(.data))) {
    rtn_missing_qs <- (paste0("**", "The following questions are not in `.data`: ",
                              paste(questions[!(questions %in% names(.data))], collapse = ", "), "**"))
    return(rtn_missing_qs)
  }


  response_key <- tolower(response_key)
  response_key <-  gsub("([()])", "\\\\\\1", response_key) #adding "\\" to any parentheses found in the response key - to escape the parentheses as reserved characters
  svy_year <- as.character(svy_year)

  #begin formatting=============================================================
  list <- .data

  #af tables####################################################################
  #simple formatting and filtering of the table
  tmp <- data.frame()
  for(i in seq_along(questions)) {

    q <- questions[i]
    tmp <- rbind(tmp, list[[q]])

  }

  tmp <- dplyr::filter(tmp, year == svy_year)

  if(nrow(tmp) == 0) {
    cli::cli_abort("Resulting data frame is empty. Data may not be available for year input")
  }


  tmp$value <- janitor::round_half_up(tmp$value, digits = 0)

  tmp <-
    tmp %>%
    dplyr::select(value, response) %>%
    dplyr::mutate(response = tolower(response))

  #cycle through response keys and filter the data frame based on that key, appending each result to a new data frame
  vals <- data.frame()
  for(i in seq_along(response_key)) {

    if(nrow(unique(tmp[grepl(response_key[i], tmp$response),])) > 1) {

      vals <- rbind(vals,
                    tmp[tmp$response == response_key[i], ])
    } else {

      vals <- rbind(vals,
                    tmp[grepl(response_key[i], tmp$response, ignore.case = T), ])
    }

  }
  #if the keywords match multiple responses, then filter the new data frame based on an exact match of the response key
  if(length(unique(vals$response)) > 1 & length(response_key) == 1) {

    vals <- vals[vals$response == response_key, ]

  }
  if(nrow(vals)==0) {

    cli::cli_abort("Resulting data frame is empty. Try a more specific keyword(s) for the response.")

  }

  #format resulting values as %, but...
  #...if more than one response key is given, sum the values of those responses
  #...unless the data are multiple choice - stop the rounding in this case
  if(length(response_key) > 1) {

    # if(questions %in% irregular_qs) {
    #
    #   cli::cli_abort("Please only provide one response_key when using multiple choice data")
    #
    # }

    vals <-
      vals %>%
      dplyr::summarise(
        sum = sum(value)
      )

    vals <- paste0(vals$sum, "%")

  } else {

    vals <- paste0(vals$value, "%")

  }

  return(vals)
}


#' @title FPS: Narrative: Print the direction of change (doc) associated with
#'   two values generated from the analysis results
#' @author Tom Pearson
#' @description This function determines the direction of change (increase,
#'   decrease, or no change) for a specified question and response in Farm
#'   Practices Survey data, when comparing values between two survey years.
#'
#' @param .data A list of data frames, each representing survey question data.
#' @param question A character or numeric value specifying the question to
#'   process. Numeric input is automatically converted to "QX" format (e.g.
#'   `"Q4"`).
#' @param response_key A character vector of keywords for the desired
#'   response(s). Supports regex patterns for filtering responses.
#' @param svy_years A numeric or character vector of length 2 specifying the
#'   survey years for comparison (e.g. `c(2024, 2023)`).
#' @param past Logical. If `TRUE`, returns past-tense narrative (e.g.
#'   "increased").
#' @param abbr Logical. If `TRUE`, returns abbreviated narrative (e.g. "an
#'   increase" instead of "an increase from").
#' @param get_pc Logical. If `TRUE`, returns the percentage change instead of
#'   narrative.
#'
#' @return A character string describing the direction of change or the
#'   percentage change if `get_pc = TRUE`.
#'
#' @details
#' \itemize{
#'   \item The comparison is always done from an older date to a newer date.
#'   \item Values are rounded using \link[janitor]{round_half_up} before comparison.
#'   \item Handles multiple response keys by summing their values unless the question is irregular.
#'   }
#'
#' @examples
#' #data_list <- list(Q4 = data.frame(year = c("2024", "2023"),
#' #                                  value = c(60, 50),
#' #                                  response = c("yes", "yes")))
#' #get_doc(data_list, question = 4, response_key = "yes",
#' #        svy_years = c(2024, 2023), past = FALSE, abbr = FALSE, get_pc = TRUE)
#'
#' @seealso \link[janitor]{round_half_up}
#'
#' @export
get_doc <- function(.data, question, response_key, svy_years, past = FALSE, abbr = FALSE, get_pc = FALSE) {

  #validation===================================================================
  if (missing(.data) || !is.list(.data)) {
    cli::cli_abort("Please supply a valid list of tables.")
  }

  if (missing(question) || length(question) != 1) {
    cli::cli_abort("Please supply one question only.")
  }

  if (missing(response_key) || length(response_key) == 0) {
    cli::cli_abort("Please supply one or more keywords for the question's response.")
  }

  if (missing(svy_years) || length(svy_years) != 2 || !all(nchar(as.character(svy_years)) == 4)) {
    cli::cli_abort("Please supply two valid survey years only in the format c(YYYY, YYYY).")
  }

  # if(question %in% new_qs) {
  #
  #   return(paste0("**", question, " NOT COLLECTED IN SURVEY YEAR**"))
  #
  # }

  #ensure question is passed in the format "QX" e.g. "Q4", not "q4" or "4"
  if(is.numeric(question)) {
    question <- toupper(paste0("Q", question))
  }

  if (!all(question %in% names(.data))) {
    rtn_missing_qs <- (paste0("**", "The following questions are not in `.data`: ",
                              paste(question[!(question %in% names(.data))], collapse = ", "), "**"))
    return(rtn_missing_qs)
  }

  response_key <- tolower(response_key)
  response_key <-  gsub("([()])", "\\\\\\1", response_key) #adding "\\" to any parentheses found in the response key - to escape the parentheses as reserved characters

  if(length(svy_years) != 2) {
    cli::cli_abort("Please supply 2 svy_years to compare in the correct format e.g. c(2024, 2023)")
  }
  if(nchar(svy_years[1])!=4 & nchar(svy_years[2])!=4) {
    cli::cli_abort("svy_years must be given in the correct format e.g. c(2024, 2023)")
  }
  #ensure data compares dates in correct order (regardless of which order passed into function)
  date1 <- svy_years[1]
  date2 <- svy_years[2]

  if(date1 < date2) {
    svy_years <- c(svy_years[2], svy_years[1])
  }

  #begin formatting=============================================================

  list <- .data

  #all farms####################################################################
  tmp <- list[[question]]
  tmp1 <- dplyr::filter(tmp, year == svy_years[1])
  tmp2 <- dplyr::filter(tmp, year == svy_years[2])

  tmp1$value <- janitor::round_half_up(tmp1$value, digits = 0)
  tmp2$value <- janitor::round_half_up(tmp2$value, digits = 0)

  tmp1 <-
    tmp1 %>%
    dplyr::select(value, response) %>%
    dplyr::mutate(response = tolower(response))
  tmp2 <-
    tmp2 %>%
    dplyr::select(value, response) %>%
    dplyr::mutate(response = tolower(response))

  #comparison
  vals1 <- data.frame()
  vals2 <- data.frame()

  for(i in seq_along(response_key)) {

    if(nrow(unique(tmp1[grepl(response_key[i], tmp1$response),])) > 1) {

      vals1 <- rbind(vals1, tmp1[tmp1$response == response_key[i], ])

    } else {

      vals1 <- rbind(vals1, tmp1[grepl(response_key[i], tmp1$response, ignore.case = T), ])

    }

    if(nrow(unique(tmp2[grepl(response_key[i], tmp2$response),])) > 1) {

      vals2 <- rbind(vals2, tmp2[tmp2$response == response_key[i], ])

    } else {

      vals2 <- rbind(vals2, tmp2[grepl(response_key[i], tmp2$response, ignore.case = T), ])

    }

  }

  if(nrow(vals1)==0 | nrow(vals2)==0) {

    cli::cli_abort("Resulting data frame is empty. Try a more specific keyword(s) for the response.")

  }

  #format resulting values as %, but...
  #...if more than one response key is given, sum the values of those responses
  #...unless the data are multiple choice (Q35 / Q46) - stop the rounding in this case
  if(length(response_key) > 1) {

    # if(question %in% irregular_qs) {
    #
    #   cli::cli_abort("Please only provide one response_key when using multiple choice data")
    #
    # }

    vals1 <-
      vals1 %>%
      dplyr::summarise(
        sum = sum(value)
      )
    vals2 <-
      vals2 %>%
      dplyr::summarise(
        sum = sum(value)
      )

    vals1 <- vals1$sum
    vals2 <- vals2$sum

    vals_diff <- vals1-vals2
    vals_pc <- round_percent(vals_diff/vals2)

  } else {

    vals1 <- vals1$value
    vals2 <- vals2$value

    vals_diff <- vals1-vals2
    vals_pc <- round_percent(vals_diff/vals2)

  }

  if(get_pc == TRUE) {

    return(vals_pc)

  } else {


    if(vals_diff > 0) {

      if(past == TRUE) {

        return("increased")

      } else if(abbr == TRUE) {

        return("an increase")

      } else {

        return("an increase from")

      }

    } else if(vals_diff == 0) {

      if(past == TRUE) {

        return("did not change")

      } else if(abbr == TRUE) {

        return("no change")

      } else {

        return("no change from")

      }

    } else {

      if(past == TRUE) {

        return("decreased")

      } else if(abbr == TRUE) {

        return("a decrease")

      } else {

        return("a decrease from")

      }

    }

  }

}

#' @title FPS: Narrative: Print the name/category associated with a value based
#'   on its ordered position in the analysis' results.
#' @author Tom Pearson
#' @description This function retrieves the response or percentage corresponding
#'   to the specified ordinal position (e.g. the most common response) for a
#'   question from a list of survey data, ordering by the proportion of holdings
#'   in descending order. You can also exclude specific responses and request
#'   the result as a percentage rather than its name.
#'
#' @param .data A list of data frames, each representing survey question data.
#' @param questions A character or numeric vector specifying the question(s) to
#'   process. Numeric input is automatically converted to "QX" format (e.g.
#'   `"Q4"`).
#' @param svy_year A character or numeric value specifying the survey year
#'   (e.g. `2024`).
#' @param ordinal A numeric value specifying the ordinal position (e.g. 1 for
#'   the most frequent response).
#' @param get_percent Logical. If `TRUE`, returns the percentage of the selected
#'   response instead of the response itself.
#' @param responses_to_exclude A character vector of responses to exclude from
#'   the result. Responses are matched using regular expressions.
#'
#' @return A character string representing either the response or the
#'   percentage, depending on `get_percent`.
#'
#' @details
#' \itemize{
#'   \item The data are ordered by the proportion of holdings in descending order (pre-rounding).
#'   \item The function allows the exclusion of certain responses by their keywords.
#'   \item The `ordinal` parameter specifies which position to retrieve (e.g., the most common response or the 2nd most common).
#'   \item If `get_percent` is `TRUE`, the percentage of the selected response is returned.
#'   }
#'
#' @examples
#' #data_list <- list(Q4 = data.frame(year = c("2024", "2024", "2024"),
#' #                                  value = c(50, 30, 20),
#' #                                  response = c("yes", "no", "maybe")))
#' #get_name(data_list, questions = 4, svy_year = 2024, ordinal = 1)
#' #get_name(data_list, questions = 4, svy_year = 2024, ordinal = 2, get_percent = TRUE)
#'
#' @seealso \link[janitor]{round_half_up}
#'
#' @export
get_name <- function(.data, questions, svy_year, ordinal, get_percent = FALSE, responses_to_exclude = NULL) {

  #validation===================================================================
  if (missing(.data) || !is.list(.data)) {
    cli::cli_abort("Please supply a valid list of tables.")
  }

  if (missing(questions) || length(questions) == 0) {
    cli::cli_abort("Please supply one or more question numbers.")
  }

  if (length(questions) > 1) {
    cli::cli_alert_warning("You are joining data from multiple questions, please ensure data are comparable.")
  }

  if (missing(svy_year) || length(svy_year) != 1) {
    cli::cli_abort("Please supply one survey year only in the correct format e.g., 'Apr-22' or '2024'.")
  }

  if (missing(ordinal)) {
    cli::cli_abort("Please supply an ordinal in numeric format (e.g. 1 to get the category that MOST frequently selects a particular response).")
  }

  # if(any(questions %in% new_qs)) {
  #
  #   return(paste0("**", questions, " NOT COLLECTED IN SURVEY YEAR**"))
  # }

  #ensure question is passed in the format "QX" e.g. "Q4", not "q4" or "4"
  if(is.numeric(questions)) {
    questions <- toupper(paste0("Q", questions))
  }

  if (!all(questions %in% names(.data))) {
    rtn_missing_qs <- (paste0("**", "The following questions are not in `.data`: ",
                              paste(questions[!(questions %in% names(.data))], collapse = ", "), "**"))
    return(rtn_missing_qs)
  }


  if((is.character(ordinal) & !grepl("last", ordinal, ignore.case = T)) | is.factor(ordinal)) {
    ordinal <- as.numeric(ordinal)
    if(is.na(ordinal)) {
      cli::cli_abort("Please supply an ordinal in numeric format (e.g. 1 to get the category that MOST frequently selects a particular response)")
    }
  }

  if(length(svy_year) != 1) {
    cli::cli_abort("Please supply only 1 svy_year in the correct format e.g. 2024")
  }

  #begin formatting=============================================================

  list <- .data

  #af tables####################################################################
  tmp <- data.frame()
  for(i in seq_along(questions)) {

    q <- questions[i]
    tmp <- rbind(tmp, list[[q]])

  }

  tmp <- dplyr::filter(tmp, year == svy_year)

  if(nrow(tmp) == 0) {
    cli::cli_abort("Resulting data frame is empty. Data may not be available for year input")
  }

  #ordering df
  vals <-
    tmp %>%
    dplyr::select(value, response) %>%
    dplyr::arrange(desc(value)) %>%
    dplyr::mutate(response = tolower(response))



  if(!is.null(responses_to_exclude)) {

    responses_to_exclude <- tolower(responses_to_exclude)
    responses_to_exclude <-  gsub("([()])", "\\\\\\1", responses_to_exclude) #adding "\\" to any parentheses found in the responses_to_exclude - to escape the parentheses as reserved characters

    excl_qa1 <- nrow(vals)

    for(i in seq_along(responses_to_exclude)) {

      if(nrow(vals[grepl(responses_to_exclude[i], vals$response, ignore.case = T),]) > 1) {

        vals <- dplyr::filter(vals, response != responses_to_exclude[i])

      } else {

        vals <- dplyr::filter(vals, !grepl(responses_to_exclude[i], response, ignore.case = T))

      }

    }

    excl_qa2 <- nrow(vals)

    if(excl_qa2 == 0 || !(excl_qa2 < excl_qa1)) {

      cli::cli_abort("The response(s) supplied to exclude are not specific enough")

    }

  }


  if(grepl("last", ordinal, ignore.case = T)) {

    ordinal <- as.numeric(nrow(vals))

  } else if(nrow(vals) < ordinal) {

    cli::cli_alert_warning("There too few data points for the ordinal you have provided, the last data point has been used as a substitute")
    ordinal <- as.numeric(nrow(vals))

  }

  #return either category or percent based on whether get_percent is FALSE (default) or not
  if(get_percent==FALSE) {

    val <- as.character(vals$response[ordinal])

  } else {

    val <- janitor::round_half_up(vals$value[ordinal], digits = 0)
    val <- paste0(val, "%")

  }

  return(val)

}



