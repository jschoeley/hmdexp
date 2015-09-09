# Input -------------------------------------------------------------------

source("./init.R")

# list of countries in data
load("./data/hmdcbook.Rdata")
#cntry_code  <- hmdcbook$Code
cntry_lab  <- hmdcbook$Label

# UI Blocks ---------------------------------------------------------------

# country dropdown menu
cntry_dropdown <-
  column(3,
         selectInput("country",
                     label    = "Choose Country",
                     choices  = cntry_lab, multiple = FALSE,
                     selected = " Sweden")
  )

# country comparison dropdown menu
cntry1_comp_dropdown <-
  column(2,
         selectInput("country_1",
                     label    = "Choose Country 1",
                     choices  = cntry_lab, multiple = FALSE,
                     selected = " Sweden")
  )
cntry2_comp_dropdown <-
  column(2,
         selectInput("country_2",
                     label    = "Choose Country 2",
                     choices  = cntry_lab, multiple = FALSE,
                     selected = " Denmark")
  )

# timebase radio button
timebase_radio <-
  column(1,
         radioButtons(inputId = "timebase",
                      label   = "Choose Timebase",
                      choices = list(Period  = "period",
                                     Cohort  = "cohort"),
                      selected = "period")
  )

# sex radio button
sex_radio <-
  column(1,
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
         p("Idea & Realization:", br(),
           a(href = "https://github.com/jschoeley", "Jonas Schöley"), "at",
           a(href = "http://www.sdu.dk/en/om_sdu/institutter_centre/maxo", br(),
             "Max-Planck Odense Center on the Biodemography of Aging")),
         p("Data Source:", br(),
           a(href = "http://www.mortality.org/", "Human Mortality Database")),
         p("A great thanks goes towards the Human Mortality Database team.
           Their magnificient work makes it possible for layman, student and
           researcher alike to explore the incredible rise of human life expectancy,
           the mysteries of the gender differences in survival or the global
           variation of mortality patterns. This project would not be possible
           without free access to the HMD data."),
          p("If you want to dig into the underlying data yourself just go to",
           a(href = "http://www.mortality.org/", "http://www.mortality.org/"),
           "and register.")
  )

# Page Layout -------------------------------------------------------------

shinyUI(
  navbarPage(id = "navbar", title = "Human Mortality Explorer",
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

             fluidRow(cntry_dropdown,
                      timebase_radio,
                      conditionalPanel(condition = "input.navbar == 'tab_mx'", sex_radio),
                      conditionalPanel(condition = "input.navbar == 'tab_mx_sex_diff'", column(1)),
                      column(3),
                      about)

  )
)
