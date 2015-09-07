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

  # filter to single sex, country and timeframe
  dataset <- reactive({
    filter(hmdmx,
           country   == input$country,
           sex       == input$sex,
           timebase  == input$timebase)
  })

  # Output: Mx Plot Title --------------------------------------------------

  output$plot_title <- renderText({

    # import user-filtered data
    dataset <- dataset()

    # generate plot title based on subsetted dataset
    GeneratePlotTitle(x = dataset, hmd_country_codes = hmdcbook)

  })

  # Output: Heatmap Plot ---------------------------------------------------

  output$plot <- renderPlot({

    # import user-filtered data
    dataset <- dataset()

    # Discretize mx -------------------------------------------------------

    years_of_mx <- DiscretizeMx(dataset)

    # Plot ----------------------------------------------------------------

    # generate heatmap
    plot_mx <- PlotMx(years_of_mx)

    print(plot_mx)

  }, bg="transparent")
})
