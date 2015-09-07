# Input -------------------------------------------------------------------

source("./init.R")

# list of countries in data
load("../priv/data/hmdcbook.Rdata")
cntry_code  <- hmdcbook$Code

# Page Layout -------------------------------------------------------------

shinyUI(
  navbarPage(title = "Human Mortality Database Explorer", theme = "bootstrap.css",
             tabPanel("Mortality Rates",

                      # Mx Pannel -----------------------------------------

                      fluidRow(
                        column(12, h4(textOutput("plot_title"), align = "center"))
                      ),
                      fluidRow(
                        column(12, plotOutput("plot"))
                      ),

                      hr()

             ),

  # Bottom Panel ----------------------------------------------------------

  fluidRow(
    column(4,
           # country dropdown
           selectInput("country",
                       label    = "Choose Country",
                       choices  = cntry_code, multiple = FALSE,
                       selected = "SWE")
    ),
    column(2,
           # timebase checkbox
           radioButtons(inputId = "timebase",
                        label   = "Choose Timebase",
                        choices = list(Period  = "period",
                                       Cohort  = "cohort"),
                        selected = "period")
    ),
    column(2,
           # sex checkbox
           radioButtons(inputId = "sex",
                        label   = "Choose Sex",
                        choices = list(Total  = "fm",
                                       Female = "f",
                                       Male   = "m"),
                        selected = "fm")
    ),
    column(4,
           p("Idea & Realization: ",
             a(href = "https://github.com/jschoeley", "Jonas Schöley")),
           p("Data Source: ",
             a(href = "http://www.mortality.org/", "Human Mortality Database"))
    )
  )
))
