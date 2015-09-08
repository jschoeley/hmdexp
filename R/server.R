# Input -------------------------------------------------------------------

source("./init.R")         # load libraries
source("./function_def.R") # load function definitions

# load human mortality data
load("./data/hmd_mx.Rdata")
load("./data/hmd_mx_sex_diff.Rdata")

# load list of countries in data
load("./data/hmdcbook.Rdata")

# Shiny -------------------------------------------------------------------

shinyServer(function(input, output, session) {

  # Reactive Dataset Generation -------------------------------------------

  # filter based on user input
  dataset_mx <- reactive({
    filter(hmd_mx,
           country   == hmdcbook[hmdcbook$Label == input$country, "Code"],
           sex       == input$sex,
           timebase  == input$timebase)
  })

  # filter based on user input
  dataset_mx_sex_diff <- reactive({
    filter(hmd_mx_sex_diff,
           country   == hmdcbook[hmdcbook$Label == input$country, "Code"],
           timebase  == input$timebase)
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
