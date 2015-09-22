# Input -------------------------------------------------------------------

source("./init.R")

# list of countries in data
load("./data/hmdcbook.Rdata")
#cntry_code  <- hmdcbook$Code
cntry_lab  <- hmdcbook$Label

# UI Blocks ---------------------------------------------------------------

# country dropdown menu
cntry_dropdown <-
  column(4,
         selectInput("country",
                     label    = "Choose Country",
                     choices  = cntry_lab, multiple = FALSE,
                     selected = "Sweden")
  )

# country comparison dropdown menu
cntry1_diff_dropdown <-
  column(2,
         selectInput("country_1",
                     label    = "Choose Country 1",
                     choices  = cntry_lab, multiple = FALSE,
                     selected = "Sweden")
  )
cntry2_diff_dropdown <-
  column(2,
         selectInput("country_2",
                     label    = "Choose Country 2",
                     choices  = cntry_lab, multiple = FALSE,
                     selected = "Denmark")
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
  column(6,
         p("Idea & Realization:", br(),
           a(href = "https://github.com/jschoeley", "Jonas Schöley"), "at",
           a(href = "http://www.sdu.dk/en/om_sdu/institutter_centre/maxo", br(),
             "Max-Planck Odense Center on the Biodemography of Aging")),
         p("Data Source:", br(),
           a(href = "http://www.mortality.org/", "Human Mortality Database")),
         p("A big thanks goes towards the Human Mortality Database team.
           Their magnificient work makes it possible for laymen, students and
           researchers alike to explore the incredible rise of human life expectancy,
           the mysteries of the gender differences in survival or the global
           variation in mortality patterns. This project would not be possible
           without free access to the HMD data."),
          p("If you want to dig into the underlying data yourself just go to",
           a(href = "http://www.mortality.org/", "http://www.mortality.org/"),
           "and register.")
  )

# Page Layout -------------------------------------------------------------

shinyUI(
  navbarPage(id = "navbar", title = "Human Mortality Explorer",
             theme = "bootstrap.css",
             header = tags$head(tags$style(".plotRow{height:32vw !important;}")),

             # Mortality Rates --------------------------------------------
             tabPanel(value = "tab_mx", title = "Mortality Rates",

                      fluidRow(h5(textOutput("plot_mx_main_title"),
                                  align = "center")),
                      fluidRow(h6(textOutput("plot_mx_sub_title"),
                                      align = "center")),

                      fluidRow(class = "plotRow",
                               plotOutput("plot_mx", height = "100%"))

             ),

             # Sex Differences --------------------------------------------
             tabPanel(value = "tab_mx_sex_diff", title = "Sex Differences",

                      fluidRow(h5(textOutput("plot_mx_sex_diff_main_title"),
                                  align = "center")),

                      fluidRow(h6(textOutput("plot_mx_sex_diff_sub_title"),
                                      align = "center")),

                      fluidRow(class = "plotRow",
                               plotOutput("plot_mx_sex_diff", height = "100%"))

             ),

             # Country Differences ----------------------------------------
             tabPanel(value = "tab_mx_cntry_diff", title = "Country Comparison",

                      fluidRow(h5(textOutput("plot_mx_country_diff_main_title"),
                                  align = "center")),

                      fluidRow(h6(textOutput("plot_mx_country_diff_sub_title"),
                                  align = "center")),

                      fluidRow(class = "plotRow",
                               plotOutput("plot_mx_cntry_diff", height = "100%"))

             ),

             hr(),

             # Settings Panel ---------------------------------------------
             fluidRow(
               conditionalPanel("(input.navbar == 'tab_mx' || input.navbar == 'tab_mx_sex_diff')",
                                cntry_dropdown),
               conditionalPanel("(input.navbar == 'tab_mx_cntry_diff')",
                                cntry1_diff_dropdown,
                                cntry2_diff_dropdown),
               timebase_radio,
               conditionalPanel("(input.navbar == 'tab_mx' || input.navbar == 'tab_mx_cntry_diff')",
                                sex_radio),
               conditionalPanel("(input.navbar == 'tab_mx_sex_diff')",
                                column(1)),
               about)

  )
)
