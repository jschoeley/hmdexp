# Data Discretizer --------------------------------------------------------

# discretize a continuous mx vector
DiscretizeMx <- function (x) {

  # mortality rate scaling factor
  scale  <- 10000
  # mortality rate breaks for discrete colour scale
  breaks <- c(0,
              0.0001, 0.0005,
              0.001, 0.005,
              0.01, 0.05,
              0.1, 0.5, 10) * scale
  labels <- c("<1", " 1", " 5",
             " 10", " 50",
             " 100", " 500",
             " 1,000", ">5,000")

  # generate timeline of discrete mx
  x %>%
    mutate(mx = cut(mx*scale,
                    breaks = breaks,
                    labels = labels,
                    include.lowest = TRUE)) -> result

  return(result)

}

# discretize a continuous mx vector
DiscretizeMxSexDiff <- function (x) {

  # mortality rate sex diff breaks for discrete colour scale
  breaks <- c(-100 , -1, -0.01, -0.001, -0.0001,
              0, 0.0001, 0.001, 0.01, 1, 100)
  labels <- c("< -1 Excess Male Mortality",
              "-1", "-0.01", "-0.001", "-0.0001",
              "+0.0001", "+0.001", "+0.01", "+1",
              "> 1 Excess Female Mortality")

  # generate timeline of discrete mx
  x %>%
    mutate(mx_sex_diff = cut(mx_sex_diff,
                             breaks = breaks,
                             labels = labels,
                             include.lowest = TRUE)) -> result

  return(result)

}

# Plot Theme --------------------------------------------------------------

theme_hmdexp <-
  theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm"),
        panel.background  = element_blank(),
        plot.background   = element_blank(),
        panel.grid.major  = element_line(colour = "grey20"),
        panel.grid.minor  = element_line(colour = "grey20"),
        legend.background = element_blank(),
        axis.title        = element_text(colour = "grey50"),
        axis.ticks        = element_blank(),
        legend.key        = element_blank(),
        legend.text       = element_text(colour = "grey50"),
        legend.title      = element_text(colour = "grey50"))

# Plot Lexis Grid Breaks --------------------------------------------------

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

# Plot Mortality Rates ----------------------------------------------------

# Generate a plot mx title based on the data subset displayed
GenerateMxPlotTitle <- function (x, hmd_country_codes) {

  # timebase labels
  if (x$timebase[1] == "period") timebase_title <- "Period"
  if (x$timebase[1] == "cohort") timebase_title <- "Cohort"

  # country labels
  country_title <- hmd_country_codes[hmd_country_codes$Code == x$country[1], 2]

  # sex labels
  if (x$sex[1] == "f")  sex_title  <- "Female"
  if (x$sex[1] == "m")  sex_title  <- "Male"
  if (x$sex[1] == "fm") sex_title  <- "Total"

  # year labels
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

# plot mx values
PlotMx <- function (x) {

  # plot
  plot_mx <-
    ggplot(x, aes(x = year, y = age)) +
    # heatmap
    geom_tile(aes(fill = mx)) +
    # discrete colour scale
    scale_fill_brewer(expression(atop(atop("Deaths per",
                                           "10,000 Person Years"),
                                      m(x)%*%10000)),
                      palette = "PuBuGn") +
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
    theme_hmdexp

  return(plot_mx)

}

# Plot Mortality Sex Differences ------------------------------------------

# Generate a plot mx sex differences title based on the data subset displayed
GenerateMxSexDiffPlotTitle <- function (x, hmd_country_codes) {

  # title: timebase labels
  if (x$timebase[1] == "period") timebase_title <- "Period"
  if (x$timebase[1] == "cohort") timebase_title <- "Cohort"

  # country labels
  country_title <- hmd_country_codes[hmd_country_codes$Code == x$country[1], 2]

  # title: year labels
  # base year range on available data
  x_naomit <- na.omit(x)
  year_min_title <- min(x_naomit$year)
  year_max_title <- max(x_naomit$year)
  year_title     <- paste(year_min_title, "to", year_max_title)
  # if no data
  if (year_title == "Inf to -Inf") year_title <- "No Data Availabe"

  plot_title <- paste0("Age-specific ", timebase_title,
                       " Mortality Rate Sex Proportions of",
                       country_title, ", ",
                       year_title,
                       "\n")

  return(plot_title)

}

# plot mx sex differences
PlotMxSexDiff <- function (x) {

  # plot
  plot_mx_sex_diff <-
    ggplot(x, aes(x = year, y = age)) +
    # heatmap
    geom_tile(aes(fill = mx_sex_diff)) +
    # divergent, cntn. colour scale
    scale_fill_manual(expression(atop(atop("Absolute Difference in",
                                            "Female and Male Mortality Rates"),
                                      m(x)[F]-m(x)[M])),
                      values = rev(brewer.pal(10,"RdBu"))) +
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
    theme_hmdexp

  return(plot_mx_sex_diff)

}
