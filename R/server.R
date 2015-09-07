# Init --------------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

# Input -------------------------------------------------------------------

# human mortality data
load("../priv/data/hmdmx.Rdata")

# list of countries in data
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

  # Plot Title ------------------------------------------------------------

  output$plot_title <- renderText({

    # import user-filtered data
    dataset <- dataset()

    # Plot Title Generation -----------------------------------------------

    # title: timebase labels
    if (dataset$timebase[1] == "period") timebase_title <- "Period"
    if (dataset$timebase[1] == "cohort") timebase_title <- "Cohort"

    # title: sex labels
    country_title <- hmdcbook[hmdcbook$Code == dataset$country[1], 2]
    if (dataset$sex[1] == "f")  sex_title  <- "Female"
    if (dataset$sex[1] == "m")  sex_title  <- "Male"
    if (dataset$sex[1] == "fm") sex_title  <- "Total"

    # title: year labels
    # base year range on available data
    dataset_naomit <- na.omit(dataset)
    year_min_title <- min(dataset_naomit$year)
    year_max_title <- max(dataset_naomit$year)
    year_title     <- paste(year_min_title, "to", year_max_title)
    # if no data
    if (year_title == "Inf to -Inf") year_title <- "No Data Availabe"

    plot_title <- paste0("Age-specific ", timebase_title, " Mortality Rates of",
                         country_title, ", ",
                         sex_title, " Population", ", ",
                         year_title,
                         "\n")

    plot_title

  })

  # Heatmap Plot ----------------------------------------------------------

  output$plot <- renderPlot({

    # import user-filtered data
    dataset <- dataset()

    # mortality rate scaling factor
    scale  <- 10000
    # mortality rate breaks for discrete colour scale
    breaks <- c(0,
      0.0001, 0.0005,
      0.001, 0.005,
      0.01, 0.05,
      0.1, 0.5, 10) * scale

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

    # generate timeline of discrete mx
    dataset %>%
      mutate(mx = cut(mx*scale,
                      breaks = breaks,
                      include.lowest = TRUE)) -> years_of_mx

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
      theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm"))

    print(plot_mx)

  }
  )
})
