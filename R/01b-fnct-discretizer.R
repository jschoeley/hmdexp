# Discretize mx Vector ----------------------------------------------------

#' Discretize a Continuous mx Vector
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
    mutate(mx_disc = cut(mx*scale,
                         breaks = breaks,
                         labels = labels,
                         include.lowest = TRUE)) -> result

  return(result)

}

# Discretize mx Sex Ratio Vector ------------------------------------------

#' Discretize a Continuous mx Sex Diff Vector
DiscretizeMxSexDiff <- function (x) {

  # mortality rate sex ratio breaks for discrete colour scale
  breaks <- c(0, 1/2 , 100/175, 100/150, 100/125, 100/101,
              101/100, 125/100, 150/100, 175/100, 2/1, Inf)
  labels <- c(">100% Excess Male Mortality",
              "75 to 100%",
              "   50 to 75%",
              "      25 to 50%",
              "         1 to 25%",
              "            -1 to 1%",
              "               1 to 25%",
              "                  25 to 50%",
              "                     50 to 75%",
              "                        75 to 100%",
              ">100% Excess Female Mortality")

  # generate timeline of discrete mx
  x %>%
    mutate(mx_sex_diff_disc = cut(mx_sex_diff,
                                  breaks = breaks,
                                  labels = labels,
                                  include.lowest = TRUE)) -> result

  return(result)

}

# Discretize mx Country Ratio Vector --------------------------------------

#' Discretize a Continuous mx Country Diff Vector
DiscretizeMxCntryDiff <- function (x, input) {

  # mortality rate country ratio breaks for discrete colour scale
  breaks <- c(0, 1/2 , 100/175, 100/150, 100/125, 100/101,
              101/100, 125/100, 150/100, 175/100, 2/1, Inf)
  labels <- c(paste(">100% Excess Mortality", input$country_2),
              "75 to 100%",
              "   50 to 75%",
              "      25 to 50%",
              "         1 to 25%",
              "            -1 to 1%",
              "               1 to 25%",
              "                  25 to 50%",
              "                     50 to 75%",
              "                        75 to 100%",
              paste(">100% Excess Mortality", input$country_1))

  # generate timeline of discrete mx
  x %>%
    mutate(mx_country_diff_disc = cut(mx_country_diff,
                                      breaks = breaks,
                                      labels = labels,
                                      include.lowest = TRUE)) -> result

  return(result)

}
