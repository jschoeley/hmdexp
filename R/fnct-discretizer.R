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
    mutate(mx = cut(mx*scale,
                    breaks = breaks,
                    labels = labels,
                    include.lowest = TRUE)) -> result

  return(result)

}

#' Discretize a Continuous mx Sex Diff Vector
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

#' Discretize a Continuous mx Country Diff Vector
DiscretizeMxCntryDiff <- function (x, input) {

  # mortality rate sex diff breaks for discrete colour scale
  breaks <- c(-100 , -1, -0.01, -0.001, -0.0001,
              0, 0.0001, 0.001, 0.01, 1, 100)
  labels <- c(paste("< -1 Excess Mortality", input$country_2),
              "-1", "-0.01", "-0.001", "-0.0001",
              "+0.0001", "+0.001", "+0.01", "+1",
              paste("> 1 Excess Mortality", input$country_1))

  # generate timeline of discrete mx
  x %>%
    mutate(mx_country_diff = cut(mx_country_diff,
                                 breaks = breaks,
                                 labels = labels,
                                 include.lowest = TRUE)) -> result

  return(result)

}
