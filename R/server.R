# Input -------------------------------------------------------------------

source("./init.R")         # load libraries
# load function definitions
source("./fnct-misc.R")
source("./fnct-discretizer.R")
source("./fnct-plot.R")
source("./fnct-plot_title.R")

# load human mortality data
load("./data/hmd_mx.Rdata")
load("./data/hmd_mx_sex_diff.Rdata")

# load list of countries in data
load("./data/hmdcbook.Rdata")

# Shiny -------------------------------------------------------------------

shinyServer(function(input, output, session) {

  # Reactive Dataset Generation -------------------------------------------

  # filter rates data based on user input
  dataset_mx <- reactive({
    filter(hmd_mx,
           country   == hmdcbook[hmdcbook$Label == input$country, "Code"],
           sex       == input$sex,
           timebase  == input$timebase)
  })

  # filter sex diff data based on user input
  dataset_mx_sex_diff <- reactive({
    filter(hmd_mx_sex_diff,
           country   == hmdcbook[hmdcbook$Label == input$country, "Code"],
           timebase  == input$timebase)
  })

  # filter country comparison data based on user input
  dataset_mx_cntry_diff <- reactive({
    country_1 <- filter(hmd_mx,
                        country   == hmdcbook[hmdcbook$Label == input$country_1, "Code"],
                        sex       == input$sex,
                        timebase  == input$timebase)
    country_2 <- filter(hmd_mx,
                        country   == hmdcbook[hmdcbook$Label == input$country_2, "Code"],
                        sex       == input$sex,
                        timebase  == input$timebase)
    full_join(country_1, country_2, by = c("timebase", "sex", "year", "age")) %>%
      mutate(mx_country_diff = mx.x - mx.y) %>%
      select(country_1 = country.x, country_2 = country.y,
             timebase, sex, year, age, mx_country_diff) %>%
      na.omit()
  })

  # Output: Mortality Rate Plot Title -------------------------------------

  output$plot_mx_main_title <- renderText({
    GenerateMxPlotMainTitle(dataset_mx(), input)
  })
  output$plot_mx_sub_title <- renderText({
    GenerateMxPlotSubTitle(dataset_mx(), input)
  })

  # Output: Mortality Rate Sex Diff Plot Title ----------------------------

  output$plot_mx_sex_diff_main_title <- renderText({
    GenerateMxSexDiffPlotMainTitle(dataset_mx_sex_diff(), input)
  })
  output$plot_mx_sex_diff_sub_title <- renderText({
    GenerateMxSexDiffPlotSubTitle(dataset_mx_sex_diff(), input)
  })

  # Output: Mortality Rate Country Diff Plot Title -------------------------

  output$plot_mx_country_diff_main_title <- renderText({
    GenerateMxCntryDiffPlotMainTitle(dataset_mx_cntry_diff(), input)
  })
  output$plot_mx_country_diff_sub_title <- renderText({
    GenerateMxCntryDiffPlotSubTitle(dataset_mx_cntry_diff(), input)
  })

  # Output: Mortality Rate Plot -------------------------------------------

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

  # Output: Country Difference Plot ---------------------------------------

  output$plot_mx_cntry_diff <- renderPlot({

    years_of_mx_cntry_diff <- DiscretizeMxCntryDiff(dataset_mx_cntry_diff(),
                                                    input = input)

    plot_mx_cntry_diff <- PlotMxCntryDiff(years_of_mx_cntry_diff)

    print(plot_mx_cntry_diff)

  }, bg = "transparent")

})
