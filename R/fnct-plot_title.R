# mx Plot Title -----------------------------------------------------------

#' Generate mx Plot Main Title Based on Selected Data
GenerateMxPlotMainTitle <- function (x, input) {

  if (IsDataFrameEmpty(x) == TRUE){
    main_title <- "No Data Available"
  } else {
    main_title <- paste0("Mortality Rates of ", input$country)
  }

  return(main_title)

}

#' Generate mx Plot Sub Title Based on Selected Data
GenerateMxPlotSubTitle <- function (x, input) {

  if (IsDataFrameEmpty(x) == TRUE){

    sub_title <- ""

  } else {

    # timebase labels
    if (input$timebase == "period") timebase_title <- "Period"
    if (input$timebase == "cohort") timebase_title <- "Cohort"

    # sex labels
    if (input$sex == "f")  sex_title  <- "Female"
    if (input$sex == "m")  sex_title  <- "Male"
    if (input$sex == "fm") sex_title  <- "Total"

    # year labels
    year_min_title <- min(x$year)
    year_max_title <- max(x$year)
    year_title     <- paste(year_min_title, "to", year_max_title)

    sub_title <- paste0(timebase_title, " ", year_title, ", ", sex_title)

  }

  return(sub_title)

}

# mx Sex Differences Plot Title -------------------------------------------

#' Generate mx Sex Differences Plot Main Title Based on Selected Data
GenerateMxSexDiffPlotMainTitle <- function (x, input) {

  if (IsDataFrameEmpty(x) == TRUE){
    main_title <- "No Data Available"
  } else {
    main_title <- paste0("Mortality Rate Sex Differences in ", input$country)
  }

  return(main_title)

}

#' Generate mx Sex Differences Plot Sub Title Based on Selected Data
GenerateMxSexDiffPlotSubTitle <- function (x, input) {

  if (IsDataFrameEmpty(x) == TRUE){

    sub_title <- ""

  } else {

    # timebase labels
    if (input$timebase == "period") timebase_title <- "Period"
    if (input$timebase == "cohort") timebase_title <- "Cohort"

    # year labels
    year_min_title <- min(x$year)
    year_max_title <- max(x$year)
    year_title     <- paste(year_min_title, "to", year_max_title)

    sub_title <- paste0(timebase_title, " ", year_title)

  }

  return(sub_title)

}

# mx Country Differences Plot Title ---------------------------------------

#' Generate mx Country Differences Plot Main Title Based on Selected Data
GenerateMxCntryDiffPlotMainTitle <- function (x, input) {

  if (IsDataFrameEmpty(x) == TRUE){

    main_title <- "No Data Available"

  } else {

    # country labels
    country_1_title <- input$country_1
    country_2_title <- input$country_2

    main_title <- paste0("Mortality Rate Country Differences of ",
                         country_1_title, " and ", country_2_title)

  }

  return(main_title)

}

#' Generate mx Country Differences Plot Main Title Based on Selected Data
GenerateMxCntryDiffPlotSubTitle <- function (x, input) {

  if (IsDataFrameEmpty(x) == TRUE){

    sub_title <- ""

  } else {

    # timebase labels
    if (input$timebase == "period") timebase_title <- "Period"
    if (input$timebase == "cohort") timebase_title <- "Cohort"

    # sex labels
    if (input$sex == "f")  sex_title  <- "Female"
    if (input$sex == "m")  sex_title  <- "Male"
    if (input$sex == "fm") sex_title  <- "Total"

    # year range labels
    year_min_title <- min(x$year)
    year_max_title <- max(x$year)
    year_title     <- paste(year_min_title, "to", year_max_title)

    sub_title <- paste0(timebase_title, " ", year_title, ", ", sex_title)
  }

  return(sub_title)

}
