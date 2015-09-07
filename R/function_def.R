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
