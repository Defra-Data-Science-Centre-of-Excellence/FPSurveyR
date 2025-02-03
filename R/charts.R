#' @title FPS: Charts: Dodged auto-barchart for GOV.UK
#' @author Tom Pearson
#' @description This function processes survey data to generate a table for use
#'   on GOV.UK as a dodged chart using the `{barchart}` tag. The output displays
#'   the proportion of responses for specified survey years, either by response
#'   or by year. It allows for the visualization of how responses evolve across
#'   survey years and can pivot the chart either by response or by year.
#'
#' @param .data A `tbl_list` for a specific section containing survey data for
#'   different questions.
#' @param questions A character vector of questions (e.g. "Q1", "Q2", etc.) for
#'   which the data should be processed. The function supports multiple
#'   questions but assumes the data is comparable across them.
#' @param svy_years A vector of survey years to include in the chart (e.g.
#'   `c(2020, 2021, 2022)`).
#' @param pivot_from_response A logical value. If `TRUE`, pivots the data from
#'   the "response" variable to the response values. If `FALSE` (default),
#'   pivots the data from the "year" variable to create columns for each year.
#'
#' @return A table formatted as a knitr object displaying the proportion of
#'   responses in a dodged format. The chart can be pivoted either by response
#'   or by year, depending on the value of `pivot_from_response`.
#'
#' @details
#' \itemize{
#'   \item The function works with survey data stored in `tbl_list` for each section. Each table should represent a survey question.
#'   \item It processes multiple questions by combining the data for all specified questions into one dataset.
#'   \item The function handles two types of pivoting:
#'   \itemize{
#'     \item Pivoting by "response" (`pivot_from_response = TRUE`): This will create one column per response category.
#'     \item Pivoting by "year" (`pivot_from_response = FALSE`): This will create one column per survey year.
#'     }
#'   \item It filters data for the specified survey years and formats the responses as percentages.
#' }
#'
#' @examples
#' #fps_dodged(survey_data, questions = c("Q1", "Q2"), svy_years = c(2020, 2021), pivot_from_response = TRUE)
#'
#' @export
fps_dodged <- function(.data, questions, svy_years, pivot_from_response = FALSE) {

  #testing======================================================================
  # .data = s3_tbl_list
  # question = "Q27"
  # svy_years = seq(survey_year-1, survey_year)
  # pivot_from_response = FALSE

  #validation===================================================================


  if (!is.list(.data)) {
    cli::cli_abort("Input '.data' must be a list object containing survey data.")
  }

  if (missing(questions) || length(questions) == 0) {
    cli::cli_abort("Please provide at least one valid question (e.g. 'Q1').")
  }

  if (!all(questions %in% names(.data))) {
    rtn_missing_qs <- (paste0("**", "The following questions are not in `.data`: ",
                              paste(questions[!(questions %in% names(.data))], collapse = ", "), "**"))
    return(rtn_missing_qs)
  }

  if (missing(svy_years) || length(svy_years) < 1) {
    cli::cli_abort("Please provide a valid vector of survey years (e.g. c(2020, 2021)).")
  }

  if (!all(svy_years %in% unique(.data[[questions[1]]]$year))) {
    cli::cli_abort("The survey years provided do not match the available years in the dataset.")
  }

  # if(any(questions %in% new_qs)) {
  #
  #   return(paste0("**", questions, " NOT COLLECTED IN SURVEY YEAR**"))
  #
  # }

  #create table=================================================================

  if(length(questions) > 1) {

    tmp_data <- data.frame()
    for(i in seq_along(questions)) {

      q <- questions[i]
      tmp_data <- rbind(tmp_data,
                        .data[[q]])

    }

  } else {

    tmp_data <- .data[[questions]]

  }

  if(pivot_from_response == TRUE) {

    tmp_chart <-
      tmp_data %>%
      dplyr::select(-ci) %>% #first, removing new ci column
      dplyr::filter(year %in% as.character(svy_years)) %>%
      dplyr::arrange(year) %>%
      dplyr::mutate(response = gsub("\\\n", "", response),
                    #value must be in proportion (rather than percentage) for round_percent to work
                    value = round_percent(value/100)) %>%
      tidyr::pivot_wider(names_from = "response", values_from = "value") %>%
      dplyr::rename(Year = year)


  } else {

    tmp_chart <-
      tmp_data %>%
      dplyr::select(-ci) %>% #first, removing new ci column
      dplyr::filter(year %in% as.character(svy_years)) %>%
      dplyr::arrange(desc(value)) %>%
      dplyr::mutate(response = gsub("\\\n", "", response),
                    #value must be in proportion (rather than percentage) for round_percent to work
                    value = round_percent(value/100)) %>%
      tidyr::pivot_wider(names_from = "year", values_from = "value") %>%
      dplyr::rename(Response = response)

  }

  tmp_chart <- knitr::kable(tmp_chart)
  return(tmp_chart)

}

#' @title FPS: Charts: Stacked auto-barchart for GOV.UK
#' @author Tom Pearson
#' @description This function processes survey data to create a table for use on
#'   GOV.UK as a stacked percentage chart using the `{barchart stacked}` tag.
#'   The output shows the proportions of different responses over the specified
#'   years. It supports multiple questions and allows for comparison across
#'   different survey years.
#'
#' @param .data A `tbl_list` for a specific section containing survey data for
#'   different questions.
#' @param questions A character vector of questions (e.g. "Q1", "Q2", etc.) for
#'   which the data should be processed. The function supports multiple
#'   questions but assumes the data is comparable across them.
#' @param svy_years A vector of survey years to include in the chart (e.g.
#'   `c(2020, 2021, 2022)`).
#'
#' @return A table formatted as a knitr object showing the proportion of
#'   different responses in a stacked format, for each year in `svy_years`. Each
#'   response is shown as a percentage of the total, with a "100%" total column
#'   added.
#'
#' @details
#' \itemize{
#'   \item The function works with survey data stored in `tbl_list` for each section. Each table should represent a survey question.
#'   \item It filters the data to include only the specified survey years, formats the response proportions as percentages, and arranges the data in a stacked format.
#'   \item The input questions should be valid and exist in the `.data` list. If a question is not collected in the given years, the function will notify the user.
#'   \item The function will process data for multiple questions by combining their data into one, ensuring the columns are compatible.
#'   }
#'
#' @examples
#' #fps_stacked(survey_data, questions = c("Q1", "Q2"), svy_years = c(2020, 2021, 2022))
#'
#' @export
fps_stacked <- function(.data, questions, svy_years) {

  #testing======================================================================
  # .data = s3_tbl_list
  # question = "Q24"
  # svy_years = seq(survey_year-4, survey_year)

  #validation===================================================================
  if (!is.list(.data)) {
    cli::cli_abort("Input '.data' must be a list object containing survey data.")
  }

  if (missing(questions) || length(questions) == 0) {
    cli::cli_abort("Please provide at least one valid question (e.g. 'Q1').")
  }

  if (!all(questions %in% names(.data))) {
    rtn_missing_qs <- (paste0("**", "The following questions are not in `.data`: ",
                              paste(questions[!(questions %in% names(.data))], collapse = ", "), "**"))
    return(rtn_missing_qs)
  }

  if (missing(svy_years) || length(svy_years) < 1) {
    cli::cli_abort("Please provide a valid vector of survey years (e.g. `c(2020, 2021)`).")
  }

  if (!all(svy_years %in% unique(.data[[questions[1]]]$year))) {
    cli::cli_abort("The survey years provided do not match the available years in the dataset.")
  }

  # if(any(questions %in% new_qs)) {
  #
  #   return(paste0("**", questions, " NOT COLLECTED IN SURVEY YEAR**"))
  #
  # }

  #create table=================================================================

  if(length(questions) > 1) {

    tmp_data <- data.frame()
    for(i in seq_along(questions)) {

      q <- questions[i]
      tmp_data <- rbind(tmp_data,
                        .data[[q]])

    }

  } else {

    tmp_data <- .data[[questions]]

  }

  tmp_chart <-
    tmp_data %>%
    dplyr::select(-ci) %>% #first, removing new ci column
    dplyr::filter(year %in% as.character(svy_years)) %>%
    dplyr::mutate(response = gsub("\\\n", "", response)) %>%
    dplyr::mutate(value = round_percent(value/100)) %>%
    dplyr::mutate(Total = "100%") %>%
    tidyr::pivot_wider(names_from = "response", values_from = "value") %>%
    dplyr::arrange(year) %>%
    dplyr::rename(Year = year) %>%
    dplyr::relocate(Total, .after = dplyr::last_col())

  tmp_chart <- knitr::kable(tmp_chart)
  return(tmp_chart)

}

#' @title FPS: Charts: Render image file imitation of GOV.UK auto-barchart for
#'   QA
#' @author Tom Pearson
#' @description This function generates custom bar charts for Farm Practices
#'   Survey (FPS) data using ggplot2. The charts mimic GOV.UK auto-barchart
#'   formatting and are intended for QA purposes.
#'
#' @param .data A named list of data frames, where each list element corresponds
#'   to a survey question and contains its associated data. Each data frame must
#'   include columns `year`, `value`, and any variables specified by `yaxis` and
#'   `fill`.
#' @param questions A character vector of one or more question identifiers
#'   (e.g. "Q1", "Q2") to extract and visualize data for. Each identifier must
#'   match a name in `.data`.
#' @param years A numeric vector of survey years to include in the chart.
#' @param yaxis A character string specifying the variable for the y-axis (e.g.
#'   `"year"` or `"response"`).
#' @param fill A character string specifying the variable to use for fill colors
#'   (e.g. `"year"` or `"response"`).
#' @param stacked Logical. If `TRUE`, the chart will use a stacked bar format;
#'   if `FALSE`, it will use a dodged bar format. Defaults to `TRUE`.
#'
#' @return A ggplot object representing the bar chart.
#'
#' @details The function:
#' \itemize{
#'   \item Extracts and combines data for the specified questions.
#'   \item Filters data by the given years.
#'   \item Formats data for GOV.UK-style barcharts with appropriate aesthetics and color schemes.
#'   \item Dynamically adjusts graph parameters, such as text size and bar positioning, based on the input data.
#'   }
#'
#' @examples
#' #fps_chart(
#' #  .data = intro_tbl_list,
#' #  questions = "Q1",
#' #  years = c(2023, 2024),
#' #  yaxis = "year",
#' #  fill = "response",
#' #  stacked = TRUE)
#'
#' @export
fps_chart <- function(.data, questions, years, yaxis, fill, stacked = TRUE) {

  #testing======================================================================
  # .data = intro_tbl_list
  # questions = "Q1"
  # years = seq(2024, 2024-1)
  #
  # #example stacked:
  # # yaxis = "year"
  # # fill = "response"
  # # stacked = TRUE
  #
  # #example dodged:
  # yaxis = "response"
  # fill = "year"
  # stacked = FALSE

  #validation===================================================================
  if (!is.list(.data) || is.null(names(.data))) {
    cli::cli_abort("`.data` must be a named list of data frames.")
  }

  if (!is.character(questions) || length(questions) == 0) {
    cli::cli_abort("`questions` must be a non-empty character vector of questions")
  }

  if (!all(questions %in% names(.data))) {
    rtn_missing_qs <- (paste0("**", "The following questions are not in `.data`: ",
                              paste(questions[!(questions %in% names(.data))], collapse = ", "), "**"))
    return(rtn_missing_qs)
  }

  if (!is.numeric(years) || length(years) == 0) {
    cli::cli_abort("`years` must be a non-empty numeric vector of survey years.")
  }

  if (!is.character(yaxis) || length(yaxis) != 1) {
    cli::cli_abort("`yaxis` must be a single character string specifying a column in the data.")
  }

  if (!is.character(fill) || length(fill) != 1) {
    cli::cli_abort("`fill` must be a single character string specifying a column in the data.")
  }

  if (!is.logical(stacked) || length(stacked) != 1) {
    cli::cli_abort("`stacked` must be a single logical value.")
  }

  #get data=====================================================================

  # if(any(questions %in% new_qs)) {
  #
  #   return(paste0("**", questions, " NOT COLLECTED IN SURVEY YEAR**"))
  #
  # }

  if(length(questions) > 1) {

    tmp_data <- data.frame()
    for(i in seq_along(questions)) {

      q <- questions[i]
      tmp_data <- rbind(tmp_data,
                        .data[[q]])

    }

    .data <- tmp_data

  } else {

    .data <- .data[[questions]]

  }

  #filter data==================================================================
  .data <- .data %>% dplyr::filter(year %in% years)

  if(stacked == FALSE && !grepl("year", yaxis, ignore.case = T)) {
    .data <- .data %>% dplyr::arrange(desc(value))
  }

  #format data==================================================================
  #factorise data
  .data[[yaxis]] <- factor(.data[[yaxis]], levels = rev(as.character(unique(.data[[yaxis]]))))
  .data[[fill]] <- factor(.data[[fill]], levels = as.character(unique(.data[[fill]])))


  #set general parameters=======================================================
  #set GOV.UK palette
  DEFRAcolours <- c(unname(afcharts::af_colour_palettes$main6),
                    "#FFDD00")

  #if series == 2, then use blue and orange from palette only (as per AF guidance)
  if(length(levels(.data[[fill]])) == 2) {

    DEFRAcolours <- DEFRAcolours[c(1,4)]

  }

  #set subsidiary vars
  group <- fill
  sym_fill <- rlang::sym(fill)
  sym_yaxis <- rlang::sym(yaxis)

  #set legend parameters
  legend_breaks <- as.character(levels(.data[[fill]]))
  legend_position <- "top"

  #expand size of graph according to unique number of series (instead of ggplot default of making bars thicker)
  expand_factor <- length(levels(.data[[yaxis]]))

  #set font size if any yaxis category character string is over a certain length - should be combined with scales::label_wrap setting
  axis_text_y_size <- dplyr::case_when(any(nchar(as.character(.data[[yaxis]])) >= 30) & any(nchar(as.character(.data[[yaxis]])) < 50) ~ 18,
                                       any(nchar(as.character(.data[[yaxis]])) >= 50) ~ 14,
                                       TRUE ~ 25)

  #set stacked-specific parameters==============================================
  if(stacked == TRUE) {

    bar_position <- ggplot2::position_stack(reverse = TRUE)
    text_position <- ggplot2::position_stack(reverse = TRUE, vjust = 0.5) #NB: vjust in this case is "hjust" as we are creating horizontal stacking by choosing x as the value axis
    text_hjust <- .5
    text_size <- 8
    text_colour <- ifelse(as.integer(.data[[fill]]) %in% c(4, 6, 7), "black", "white") #for levels 4, 6 or 7, make the text black, otherwise white (the level number matches the DEFRAcolours palette - orange (4), purple (6), yellow (7))

    brks <- seq(0, 100, by = 10)
    lims <- c(0, 102)

    #removing labels if under 10
    .data <-
      .data %>%
      dplyr::mutate(x_value_label = dplyr::case_when(value < 10 ~ NA,
                                                     TRUE ~ value),
                    x_value_label = round_number(x_value_label, suffix = "%"))

    #set dodged-specific parameters===============================================
  } else {

    bar_position <- ggplot2::position_dodge2(reverse = TRUE, width = .9)
    text_position <- ggplot2::position_dodge2(reverse = TRUE, width = .9)
    text_hjust <- -.1
    text_size <- 8
    text_colour <- "black"

    brks <- NULL
    lims <- c(0, max(.data$value) + 10)

    #removing labels if 0
    .data <-
      .data %>%
      dplyr::mutate(x_value_label = dplyr::case_when(value == 0 ~ NA,
                                                     TRUE ~ value),
                    x_value_label = round_number(x_value_label, suffix = "%"))


  }

  #create plot==================================================================
  p <-
    ggplot2::ggplot(.data, ggplot2::aes(y = .data[[yaxis]], x = .data[["value"]], fill = .data[[fill]] ) ) +
    ggplot2::geom_bar(stat="identity",
                      position = bar_position,
                      width = .9) +
    ggplot2::geom_text(ggplot2::aes(label = x_value_label),
                       position = text_position,
                       hjust = text_hjust, #NB: this is just for dodged charts - in stacked charts hjust is controlled using vjust in position_stack()
                       size = text_size,
                       colour = text_colour) +
    ggplot2::scale_x_continuous(expand = c(0, 0),
                                breaks = brks,
                                limits = lims,
                                labels = scales::percent_format(scale = 1)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = 0, 1/expand_factor), #expand = c(0, 0)
                              labels = scales::label_wrap(35)) + #forces labels to break lines if above a certain character length
    ggplot2::scale_fill_manual(values = DEFRAcolours,
                               breaks = legend_breaks, #order of legend
                               labels = scales::label_wrap(55),
                               drop = FALSE) +
    ggplot2::xlab("") + ggplot2::ylab("") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = legend_position,
                   legend.justification = c(0, 0),
                   legend.margin = ggplot2::margin(t=0, r=0, b=-5, l=0, unit="pt"),
                   legend.direction = "vertical",
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 25),
                   axis.title.y = ggplot2::element_text(size = 25),
                   axis.text.y = ggplot2::element_text(size = axis_text_y_size,
                                                       colour = "black"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())


  return(p)

}

