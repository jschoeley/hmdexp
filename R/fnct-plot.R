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

cpal_mx <- c("#FFFFD9", "#EDF8B1", "#C7E9B4",
             "#7FCDBB", "#41B6C4", "#1D91C0",
             "#225EA8", "#253494", "#081D58")

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
xlimit <- c(1748, 2013)

# age breaks & labels for y-scale
ybreak <- seq(0, 110, 10)
# age limits
ylimit <- c(0, 112)


# Lexis Grid Base Plot ----------------------------------------------------

PlotLexisGrid <- function (x) {
  ggplot(x, aes(x = year, y = age)) +
    # cohort lines
    geom_abline(intercept = seq(-2000, 2000, 10),
                colour = "grey50", size = 0.25, lty = 2) +
    # custom xy scale labels
    scale_x_continuous("Year", limits = xlimit,
                       breaks = xbreak, labels = xlabel,
                       expand = c(0, 0.5)) +
    scale_y_continuous("Age", limits = ylimit,
                       breaks = ybreak,
                       expand = c(0, 0.5)) +
    # equidistant xy-coordinates
    coord_equal() +
    # add theme
    theme_hmdexp
}


# Plot mx -----------------------------------------------------------------

#' Plot mx Values
PlotMx <- function (x, cont) {

  # title for colour scale legend
  fill_label <- expression(atop(atop("Deaths per",
                                     "10,000 Person Years"),
                                m(x)%*%10000))

  if (cont == FALSE) {
    plot_mx <-
      PlotLexisGrid(x) +
      # discrete heatmap
      geom_tile(aes(fill = mx_disc)) +
      # discrete colour scale
      scale_fill_manual(fill_label, values = cpal_mx,
                        # plot the full scale even if not all colours are used
                        drop = FALSE) +
      guides(fill = guide_legend(reverse = TRUE))
  }
  if (cont == TRUE) {
    breaks <- c(0.0001, 0.001, 0.01, 0.1, 1)
    labels <- breaks * 10000
    plot_mx <-
      PlotLexisGrid(x) +
      # continuous heatmap
      geom_tile(aes(fill = mx)) +
      # continuous colour scale
      scale_fill_gradientn(fill_label, colours = cpal_mx,
                           values = c(0, (0.9/8)*(1:7), 1),
                           trans  = "log10",
                           breaks = breaks,
                           labels = labels)
  }

  return(plot_mx)

}

# Plot mx Sex Differences -------------------------------------------------

#' Plot mx Sex Differences
PlotMxSexDiff <- function (x) {

  # title for colour scale legend
  fill_label <- expression(atop(atop("Absolute Difference in",
                                     "Female and Male Mortality Rates"),
                                m(x)[F]-m(x)[M]))

  # plot
  plot_mx_sex_diff <-
    PlotLexisGrid(x) +
    # heatmap
    geom_tile(aes(fill = mx_sex_diff)) +
    # divergent colour scale
    scale_fill_manual(fill_label,
                      values = rev(brewer.pal(10, "RdBu")),
                      # plot the full scale even if not all colours are used
                      drop = FALSE) +
    guides(fill = guide_legend(reverse = TRUE))

  return(plot_mx_sex_diff)

}

# Plot mx Country Differences ---------------------------------------------

#' Plot mx Country Differences
PlotMxCntryDiff <- function (x) {

  # title for colour scale legend
  fill_label <- expression(atop(atop("Absolute Difference in",
                                     "Country 1 and Country 2 Mortality Rates"),
                                m(x)[1]-m(x)[2]))

  # plot
  plot_mx_cntry_diff <-
    PlotLexisGrid(x) +
    # heatmap
    geom_tile(aes(fill = mx_country_diff)) +
    # divergent colour scale
    scale_fill_brewer(fill_label,
                      palette = "BrBG",
                      # plot the full scale even if not all colours are used
                      drop = FALSE) +
    guides(fill = guide_legend(reverse = TRUE))

  return(plot_mx_cntry_diff)

}
