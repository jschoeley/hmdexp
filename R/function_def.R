# Functions ---------------------------------------------------------------

DiscretizeMx <- function (x) {

  # mortality rate scaling factor
  scale  <- 10000
  # mortality rate breaks for discrete colour scale
  breaks <- c(0,
              0.0001, 0.0005,
              0.001, 0.005,
              0.01, 0.05,
              0.1, 0.5, 10) * scale

  # generate timeline of discrete mx
  x %>%
    mutate(mx = cut(mx*scale,
                    breaks = breaks,
                    include.lowest = TRUE)) -> years_of_mx

  return(years_of_mx)

}


GeneratePlotTitle <- function (x, hmd_country_codes) {

  # title: timebase labels
  if (x$timebase[1] == "period") timebase_title <- "Period"
  if (x$timebase[1] == "cohort") timebase_title <- "Cohort"

  # title: sex labels
  country_title <- hmd_country_codes[hmd_country_codes$Code == x$country[1], 2]
  if (x$sex[1] == "f")  sex_title  <- "Female"
  if (x$sex[1] == "m")  sex_title  <- "Male"
  if (x$sex[1] == "fm") sex_title  <- "Total"

  # title: year labels
  # base year range on available data
  x_naomit <- na.omit(x)
  year_min_title <- min(x_naomit$year)
  year_max_title <- max(x_naomit$year)
  year_title     <- paste(year_min_title, "to", year_max_title)
  # if no data
  if (year_title == "Inf to -Inf") year_title <- "No Data Availabe"

  plot_title <- paste0("Age-specific ", timebase_title, " Mortality Rates of",
                       country_title, ", ",
                       sex_title, " Population", ", ",
                       year_title,
                       "\n")

  return(plot_title)

}
