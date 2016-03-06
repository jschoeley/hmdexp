# Input -------------------------------------------------------------------

# load libraries
source("./00-init.R")
# load ui building blocks
source("./02-ui_elements.R")

# Page Layout -------------------------------------------------------------

shinyUI(
  navbarPage(id = "navbar", title = "Human Mortality Explorer",
             theme = "bootstrap.css",
             header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),

             # Mortality Rates --------------------------------------------

             tabPanel(value = "tab_mx", title = "Mortality Rates",

                      UIPlotTitle("plot_mx_main_title"),
                      UIPlotSubTitle("plot_mx_sub_title"),

                      UIPlot("plot_mx")

             ),

             # Sex Differences --------------------------------------------

             tabPanel(value = "tab_mx_sex_diff", title = "Sex Differences",

                      UIPlotTitle("plot_mx_sex_diff_main_title"),
                      UIPlotSubTitle("plot_mx_sex_diff_sub_title"),

                      UIPlot("plot_mx_sex_diff")

             ),

             # Country Differences ----------------------------------------

             tabPanel(value = "tab_mx_cntry_diff", title = "Country Comparison",

                      UIPlotTitle("plot_mx_country_diff_main_title"),
                      UIPlotSubTitle("plot_mx_country_diff_sub_title"),

                      UIPlot("plot_mx_cntry_diff")

             ),

             hr(),

             # Settings Panel ---------------------------------------------

             fluidRow(
               # country selection
               conditionalPanel("(input.navbar == 'tab_mx' || input.navbar == 'tab_mx_sex_diff')",
                                cntry_dropdown),
               # country 2 selection (for country comparisions)
               conditionalPanel("(input.navbar == 'tab_mx_cntry_diff')",
                                cntry1_diff_dropdown,
                                cntry2_diff_dropdown),
               # sex selection (if not in sex comparision mode)
               conditionalPanel("(input.navbar == 'tab_mx' || input.navbar == 'tab_mx_cntry_diff')",
                                sex_radio),
               conditionalPanel("(input.navbar == 'tab_mx_sex_diff')",
                                column(1)),
               # advanced options
               column(2,
                      # on/off
                      advanced_switch,
                      # options
                      conditionalPanel("(input.advanced == true)",
                                       timebase_radio,
                                       scale_switch)),
               about)

  )
)
