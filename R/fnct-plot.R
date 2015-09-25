# Plot Theme --------------------------------------------------------------

theme_hmdexp <-
  theme(plot.margin       = unit(c(0, 0, 0, 0), units = "cm"),
        panel.background  = element_blank(),
        plot.background   = element_blank(),
        panel.grid.major  = element_line(colour = "grey50", size = 0.25),
        panel.grid.minor  = element_blank(),
        legend.background = element_blank(),
        axis.title        = element_text(colour = "grey50"),
        axis.ticks        = element_blank(),
        legend.key        = element_blank(),
        legend.text       = element_text(colour = "grey50"),
        legend.title      = element_text(colour = "grey50"))

# Plot Lexis Grid Breaks --------------------------------------------------

# year breaks for x-scale
xbreak <- c(1750, seq(1760, 1790, 10),
            1800, seq(1810, 1890, 10),
            1900, seq(1910, 1990, 10),
            2000, 2010)
# year labels for x-scale
xlabel <- c(1750, paste0("'", seq(60, 90, 10)),
            1800, paste0("'", seq(10, 90, 10)),
            1900, paste0("'", seq(10, 90, 10)),
            2000, "'10")
# year limits
xlimit <- c(1750, 2012)

# age breaks & labels for y-scale
ybreak <- seq(0, 110, 10)
# age limits
ylimit <- c(0, 112)

# Plot mx -----------------------------------------------------------------

#' Plot mx Values
PlotMx <- function (x) {

  # plot
  plot_mx <-
    ggplot(x, aes(x = year, y = age)) +
    # cohort lines
    geom_abline(intercept = seq(-2000, 2000, 10),
                colour = "grey50", size = 0.25, lty = 2) +
    # heatmap
    geom_tile(aes(fill = mx)) +
    # discrete colour scale
    scale_fill_brewer(expression(atop(atop("Deaths per",
                                           "10,000 Person Years"),
                                      m(x)%*%10000)),
                      palette = "PuBuGn",
                      # plot the full scale even if not all colours are used
                      drop = FALSE) +
    guides(fill = guide_legend(reverse = TRUE)) +
    # custom xy scale labels
    scale_x_continuous("Year", limits = xlimit,
                       breaks = xbreak, labels = xlabel,
                       expand = c(0, 0.5)) +
    scale_y_continuous("Age", limits = ylimit,
                       breaks = ybreak,
                       expand = c(0, 0.5)) +
    # equidistant xy-coordinates
    coord_equal() +
    theme_hmdexp

  return(plot_mx)

}

# Plot mx Sex Differences -------------------------------------------------

#' Plot mx Sex Differences
PlotMxSexDiff <- function (x) {

  # plot
  plot_mx_sex_diff <-
    ggplot(x, aes(x = year, y = age)) +
    # cohort lines
    geom_abline(intercept = seq(-2000, 2000, 10),
                colour = "grey50", size = 0.25, lty = 2) +
    # heatmap
    geom_tile(aes(fill = mx_sex_diff)) +
    # divergent, cntn. colour scale
    scale_fill_manual(expression(atop(atop("Absolute Difference in",
                                           "Female and Male Mortality Rates"),
                                      m(x)[F]-m(x)[M])),
                      values = rev(brewer.pal(10, "RdBu")),
                      # plot the full scale even if not all colours are used
                      drop = FALSE) +
    guides(fill = guide_legend(reverse = TRUE)) +
    # custom xy scale labels
    scale_x_continuous("Year", limits = xlimit,
                       breaks = xbreak, labels = xlabel,
                       expand = c(0, 0.5)) +
    scale_y_continuous("Age", limits = ylimit,
                       breaks = ybreak,
                       expand = c(0, 0.5)) +
    # equidistant xy-coordinates
    coord_equal() +
    theme_hmdexp

  return(plot_mx_sex_diff)

}

# Plot mx Country Differences ---------------------------------------------

#' Plot mx Country Differences
PlotMxCntryDiff <- function (x) {

  # plot
  plot_mx_cntry_diff <-
    ggplot(x, aes(x = year, y = age)) +
    # cohort lines
    geom_abline(intercept = seq(-2000, 2000, 10),
                colour = "grey50", size = 0.25, lty = 2) +
    # heatmap
    geom_tile(aes(fill = mx_country_diff)) +
    # divergent, cntn. colour scale
    scale_fill_brewer(expression(atop(atop("Absolute Difference in",
                                           "Country 1 and Country 2 Mortality Rates"),
                                      m(x)[1]-m(x)[2])),
                      palette = "BrBG",
                      # plot the full scale even if not all colours are used
                      drop = FALSE) +
    guides(fill = guide_legend(reverse = TRUE)) +
    # custom xy scale labels
    scale_x_continuous("Year", limits = xlimit,
                       breaks = xbreak, labels = xlabel,
                       expand = c(0, 0.5)) +
    scale_y_continuous("Age", limits = ylimit,
                       breaks = ybreak,
                       expand = c(0, 0.5)) +
    # equidistant xy-coordinates
    coord_equal() +
    theme_hmdexp

  return(plot_mx_cntry_diff)

}
