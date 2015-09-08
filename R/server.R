# Input -------------------------------------------------------------------

source("./init.R")         # load libraries
source("./function_def.R") # load function definitions

# load human mortality data
load("../priv/data/hmdmx.Rdata")

# load list of countries in data
load("../priv/data/hmdcbook.Rdata")

# Shiny -------------------------------------------------------------------

shinyServer(function(input, output, session) {

  # Reactive Dataset Generation -------------------------------------------

  # project cohort-age data onto period-age grid
  hmdmx[hmdmx$timebase == "cohort", "year"] <-
    hmdmx[hmdmx$timebase == "cohort", "year"] +
    hmdmx[hmdmx$timebase == "cohort", "age"]

  # filter to single sex, country and timeframe
  dataset_mx <- reactive({
    filter(hmdmx,
           country   == input$country_mx,
           sex       == input$sex_mx,
           timebase  == input$timebase_mx)
  })

  # filter to males and females, country and timeframe
  dataset_mx_sex_diff <- reactive({
    filter(hmdmx,
           country   == input$country_mx_sex_diff,
           sex       != "fm",
           timebase  == input$timebase_mx_sex_diff) %>%
      spread(key = sex, value = mx) %>%
      mutate(mx_sex_diff = f - m)
  })

  # Output: Mortality Rate Plot Title -------------------------------------

  output$plot_mx_title <- renderText({

    # generate plot title based on subsetted dataset
    GenerateMxPlotTitle(x = dataset_mx(), hmd_country_codes = hmdcbook)

  })

  # Output: Mortality Rate Sex Diff Plot Title ----------------------------

  output$plot_mx_sex_diff_title <- renderText({

    # generate plot title based on subsetted dataset
    GenerateMxSexDiffPlotTitle(x = dataset_mx_sex_diff(), hmd_country_codes = hmdcbook)

  })

  # Output: Heatmap Plot ---------------------------------------------------

  output$plot_mx <- renderPlot({

    # discretize mx
    years_of_mx <- DiscretizeMx(dataset_mx())

    # generate heatmap
    plot_mx <- PlotMx(years_of_mx)

    print(plot_mx)

  }, bg = "transparent")

  # Output: Sex Difference Plot -------------------------------------------

  output$plot_mx_sex_diff <- renderPlot({

    years_of_mx_sex_diff <- DiscretizeMxSexDiff(dataset_mx_sex_diff())

    plot_mx_sex_diff <- PlotMxSexDiff(years_of_mx_sex_diff)

    print(plot_mx_sex_diff)

  }, bg = "transparent")

})
