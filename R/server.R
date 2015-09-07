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

    # year breaks for x-scale
    xbreak <- c(1670, seq(1680, 1690, 10),
                1700, seq(1710, 1790, 10),
                1800, seq(1810, 1890, 10),
                1900, seq(1910, 1990, 10),
                2000, 2010)
    # year labels for x-scale
    xlabel <- c(1670, paste0("'", seq(80, 90, 10)),
                1700, paste0("'", seq(10, 90, 10)),
                1800, paste0("'", seq(10, 90, 10)),
                1900, paste0("'", seq(10, 90, 10)),
                2000, "'10")

    # age breaks & labels for y-scale
    ybreak <- seq(0, 110, 10)

    # Discretize mx -------------------------------------------------------

    years_of_mx <- DiscretizeMx(dataset)

    # Plot ----------------------------------------------------------------

    # generate heatmap
    plot_mx <-
      ggplot(years_of_mx, aes(x = year, y = age)) +
      # heatmap
      geom_tile(aes(fill = mx)) +
      # discrete colour scale
      scale_fill_brewer(expression(m(x)%*%10000), palette = 16) +
      guides(fill = guide_legend(reverse = TRUE)) +
      # custom xy scale labels
      scale_x_continuous("Year", limits = c(1670, 2015),
                         breaks = xbreak, labels = xlabel,
                         expand = c(0, 0.5)) +
      scale_y_continuous("Age",
                         breaks = ybreak,
                         expand = c(0, 0.5)) +
      # equidistant xy-coordinates
      coord_equal() +
      theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm"),
            panel.background = element_blank(),
            plot.background  = element_blank())

    print(plot_mx)

  }, bg="transparent")
})
