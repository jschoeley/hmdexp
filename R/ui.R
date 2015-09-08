# Input -------------------------------------------------------------------

source("./init.R")

# list of countries in data
load("./data/hmdcbook.Rdata")
cntry_code  <- hmdcbook$Code

# UI Blocks ---------------------------------------------------------------

# country dropdown menu
cnty_dropdown <-
  column(4,
         selectInput("country",
                     label    = "Choose Country",
                     choices  = cntry_code, multiple = FALSE,
                     selected = "SWE")
  )

# timebase radio button
timebase_radio <-
  column(2,
         radioButtons(inputId = "timebase",
                      label   = "Choose Timebase",
                      choices = list(Period  = "period",
                                     Cohort  = "cohort"),
                      selected = "period")
  )

# sex radio button
sex_radio <-
  column(2,
         # sex checkbox
         radioButtons(inputId = "sex",
                      label   = "Choose Sex",
                      choices = list(Total  = "fm",
                                     Female = "f",
                                     Male   = "m"),
                      selected = "fm")
  )

# about
about <-
  column(4,
         p("Idea & Realization: ",
           a(href = "https://github.com/jschoeley", "Jonas Schöley")),
         p("Data Source: ",
           a(href = "http://www.mortality.org/", "Human Mortality Database"))
  )

# Page Layout -------------------------------------------------------------

shinyUI(
  navbarPage(id = "navbar", title = "Human Mortality Database Explorer",
             theme = "bootstrap.css",

             # Mortality Rates --------------------------------------------
             tabPanel(value = "tab_mx", title = "Mortality Rates",

                      fluidRow(
                        column(12, h4(textOutput("plot_mx_title"),
                                      align = "center"))
                      ),
                      fluidRow(
                        column(12, plotOutput("plot_mx"))
                      )

             ),
             # Sex Differences --------------------------------------------
             tabPanel(value = "tab_mx_sex_diff", title = "Mortality Sex Differences",

                      fluidRow(
                        column(12, h4(textOutput("plot_mx_sex_diff_title"),
                                      align = "center"))
                      ),

                      fluidRow(
                        column(12, plotOutput("plot_mx_sex_diff"))
                      )

             ),

             hr(),

             fluidRow(cnty_dropdown,
                      timebase_radio,
                      conditionalPanel(condition = "input.navbar == 'tab_mx'", sex_radio),
                      about)

  )
)
