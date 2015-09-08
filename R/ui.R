# Input -------------------------------------------------------------------

source("./init.R")

# list of countries in data
load("../priv/data/hmdcbook.Rdata")
cntry_code  <- hmdcbook$Code

# UI Blocks ---------------------------------------------------------------

# country dropdown menu (plain mx panel)
cnty_dropdown_mx <-
  column(4,
         selectInput("country_mx",
                     label    = "Choose Country",
                     choices  = cntry_code, multiple = FALSE,
                     selected = "SWE")
  )

# country dropdown menu (mx sex differences panel)
cnty_dropdown_mx_sex_diff <-
  column(4,
         selectInput("country_mx_sex_diff",
                     label    = "Choose Country",
                     choices  = cntry_code, multiple = FALSE,
                     selected = "SWE")
  )

# timebase radio button (plain mx panel)
timebase_radio_mx <-
  column(2,
         radioButtons(inputId = "timebase_mx",
                      label   = "Choose Timebase",
                      choices = list(Period  = "period",
                                     Cohort  = "cohort"),
                      selected = "period")
  )

# timebase radio button (mx sex differences panel)
timebase_radio_mx_sex_diff <-
  column(2,
         radioButtons(inputId = "timebase_mx_sex_diff",
                      label   = "Choose Timebase",
                      choices = list(Period  = "period",
                                     Cohort  = "cohort"),
                      selected = "period")
  )

# sex radio button (plain mx panel)
sex_radio_mx <-
  column(2,
         # sex checkbox
         radioButtons(inputId = "sex_mx",
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
  navbarPage(title = "Human Mortality Database Explorer", theme = "bootstrap.css",

             # Mortality Rates --------------------------------------------
             tabPanel("Mortality Rates",

                      fluidRow(
                        column(12, h4(textOutput("plot_mx_title"),
                                      align = "center"))
                      ),
                      fluidRow(
                        column(12, plotOutput("plot_mx"))
                      ),

                      hr(),

                      fluidRow(cnty_dropdown_mx,
                               timebase_radio_mx,
                               sex_radio_mx, about)

             ),
             # Sex Differences --------------------------------------------
             tabPanel("Mortality Sex Differences",

                      fluidRow(
                        column(12, h4(textOutput("plot_mx_sex_diff_title"),
                                      align = "center"))
                      ),

                      fluidRow(
                        column(12, plotOutput("plot_mx_sex_diff"))
                      ),

                      hr(),

                      fluidRow(cnty_dropdown_mx_sex_diff,
                               timebase_radio_mx_sex_diff,
                               about)

                      )


  )
)
