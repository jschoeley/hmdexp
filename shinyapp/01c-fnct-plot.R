# Setup Plot Theme --------------------------------------------------------

theme_hmdexp <-
  theme(plot.margin       = unit(c(0, 0, 0, 0), units = "cm"),
        panel.background  = element_blank(),
        plot.background   = element_blank(),
        panel.grid.major  = element_blank(),
        panel.grid.minor  = element_blank(),
        legend.background = element_blank(),
        axis.title        = element_text(colour = "grey50"),
        axis.ticks        = element_blank(),
        legend.key        = element_blank(),
        legend.text       = element_text(colour = "grey50"),
        legend.title      = element_text(colour = "grey50"))

# Setup Plot Colours ------------------------------------------------------

# dput(RColorBrewer::brewer.pal(9, "PuBuGn"))
cpal_mx <- c("#FFF7FB", "#ECE2F0", "#D0D1E6",
             "#A6BDDB", "#67A9CF", "#3690C0",
             "#02818A", "#016C59", "#014636")

# dput(RColorBrewer::brewer.pal(11, "RdBu"))
cpal_sex <- c("#67001F", "#B2182B", "#D6604D",
              "#F4A582", "#FDDBC7", "#F7F7F7",
              "#D1E5F0", "#92C5DE", "#4393C3",
              "#2166AC", "#053061")

# dput(RColorBrewer::brewer.pal(1, "BrBG"))
cpal_cntry <- c("#543005", "#8C510A", "#BF812D",
                "#DFC27D", "#F6E8C3", "#F5F5F5",
                "#C7EAE5", "#80CDC1", "#35978F",
                "#01665E", "#003C30")

# Setup Lexis Grid --------------------------------------------------------

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
xlimit <- c(1748, 2015)

# age breaks & labels for y-scale
ybreak <- seq(0, 110, 10)
# age limits
ylimit <- c(0, 112)

# cohort lines
cohort_lines <-  geom_abline(intercept = seq(-2000, 2000, 10),
                             colour = "grey50", size = 0.25, lty = 2)
# period lines
period_lines <- geom_vline(xintercept = xbreak,
                           colour = "grey50", size = 0.25)
# age lines
age_lines <- geom_hline(yintercept = ybreak,
                        colour = "grey50", size = 0.25)

# Prepare Canvas ----------------------------------------------------------

#' Prepare the Heatmap Plot Canvas
PrepareCanvas <- function (x, grid_on_top) {
  # shift the tiles away from the midpoint
  # so that they align with the Lexis grid
  canvas <- ggplot(x, aes(x = year+0.5, y = age+0.5)) +
    # custom xy scale labels
    scale_x_continuous("Year",
                       breaks = xbreak, labels = xlabel,
                       limits = xlimit, expand = c(0, 0.5)) +
    scale_y_continuous("Age",
                       breaks = ybreak,
                       limits = ylimit, expand = c(0, 0.5)) +
    # equidistant xy-coordinates
    coord_equal() +
    # add theme
    theme_hmdexp

  # IF the grid should be drawn below the heatmap, add it now
  if (grid_on_top == FALSE) {
    canvas <- canvas + cohort_lines + period_lines + age_lines
  }

  return(canvas)
}

# Plot mx -----------------------------------------------------------------

#' Plot mx Values
PlotMx <- function (x, cont, grid_on_top) {

  # title for colour scale legend
  fill_label <- expression(atop(atop("Deaths per",
                                     "10,000 Person Years"),
                                m(x)%*%10000))

  # add discrete heatmap to plot
  if (cont == FALSE) {
    plot_mx <-
      PrepareCanvas(x, grid_on_top) +
      # discrete heatmap
      geom_raster(aes(fill = mx_disc), na.rm = TRUE) +
      # discrete colour scale
      scale_fill_manual(fill_label,
                        values = cpal_mx,
                        # plot the full scale even
                        # if not all colours are used
                        drop = FALSE) +
      # reverse legend
      guides(fill = guide_legend(reverse = TRUE))
  }
  # add continuous heatmap to plot
  if (cont == TRUE) {
    breaks <- c(0.0001, 0.001, 0.01, 0.1, 1)
    labels <- breaks * 10000
    plot_mx <-
      PrepareCanvas(x, grid_on_top) +
      # continuous heatmap
      geom_raster(aes(fill = mx), na.rm = TRUE) +
      # continuous colour scale
      scale_fill_gradientn(fill_label,
                           colours = cpal_mx,
                           # stretch top end of colour scale
                           # across wider data range
                           values = c(0, (0.9/8)*(1:7), 1),
                           breaks = breaks, labels = labels,
                           trans  = "log10")
  }

  # IF grid should be drawn over data, add it now
  # IF not the grid lines have already been added by `PrepareCanvas()`
  if (grid_on_top == TRUE) {
    plot_mx <- plot_mx +
      cohort_lines + period_lines + age_lines
  }

  return(plot_mx)

}

# Plot mx Sex Differences -------------------------------------------------

#' Plot mx Sex Differences
PlotMxSexDiff <- function (x, cont, grid_on_top) {

  # title for colour scale legend
  fill_label <- expression(atop(atop("Mortality Rate Ratio between",
                                     "Females and Males"),
                                m(x)[F]/m(x)[M]))

  # add discrete heatmap to plot
  if (cont == FALSE) {
  plot_mx_sex_diff <-
    PrepareCanvas(x, grid_on_top) +
    # discrete heatmap
    geom_raster(aes(fill = mx_sex_diff_disc), na.rm = TRUE) +
    # discrete divergent colour scale
    scale_fill_manual(fill_label,
                      values = rev(cpal_sex),
                      # plot the full scale even if
                      # not all colours are used
                      drop = FALSE) +
    guides(fill = guide_legend(reverse = TRUE))
  }
  # add continuous heatmap to plot
  if (cont == TRUE) {
    breaks <- c(2/1,
                150/100,
                1,
                100/150,
                1/2)
    labels <- c("100% Excess Female Mortality",
                "50%",
                "Equal Mortality",
                "50%",
                "100% Excess Male Mortality")

    plot_mx_sex_diff <-
      PrepareCanvas(x, grid_on_top) +
      # continuous heatmap
      geom_raster(aes(fill = mx_sex_diff), na.rm = TRUE) +
      # continuous divergent colour scale
      scale_fill_gradientn(fill_label,
                           colours = rev(cpal_sex),
                           breaks  = breaks, labels = labels,
                           limits  = c(0.5, 2),
                           trans   = "log10",
                           # squish outside limit values
                           # into extremes of colourscale
                           oob     = squish)
  }

  # IF grid should be drawn over data, add it now
  # IF not the grid lines have already been added by `PrepareCanvas()`
  if (grid_on_top == TRUE) {
    plot_mx_sex_diff <- plot_mx_sex_diff +
      cohort_lines + period_lines + age_lines
  }

  return(plot_mx_sex_diff)

}

# Plot mx Country Differences ---------------------------------------------

#' Plot mx Country Differences
PlotMxCntryDiff <- function (x, cont, grid_on_top, input) {

  # title for colour scale legend
  fill_label <- expression(atop(atop("Mortality Rate Ratio between",
                                     "Country 1 and Country 2"),
                                m(x)[1]/m(x)[2]))

  # add discrete heatmap to plot
  if (cont == FALSE) {
  plot_mx_cntry_diff <-
    PrepareCanvas(x, grid_on_top) +
    # discrete heatmap
    geom_raster(aes(fill = mx_country_diff_disc), na.rm = TRUE) +
    # discrete divergent colour scale
    scale_fill_manual(fill_label,
                      values = rev(cpal_cntry),
                      # plot the full scale even if
                      # not all colours are used
                      drop = FALSE) +
    guides(fill = guide_legend(reverse = TRUE))
  }
  # add continuous heatmap to plot
  if (cont == TRUE) {
    breaks <- c(1/2,
                100/150,
                1,
                150/100,
                2/1)
    labels <- c(paste("100% Excess Mortality", input$country_2),
                "50%",
                "Equal Mortality",
                "50%",
                paste("100% Excess Mortality", input$country_1))

    plot_mx_cntry_diff <-
      PrepareCanvas(x, grid_on_top) +
      # continuous heatmap
      geom_raster(aes(fill = mx_country_diff), na.rm = TRUE) +
      # continuous divergent colour scale
      scale_fill_gradientn(fill_label,
                           colours = rev(cpal_cntry),
                           breaks = breaks, labels = labels,
                           limits = c(0.5, 2),
                           trans = "log10",
                           # squish outside limit values
                           # into extremes of colourscale
                           oob = squish)
  }

  # IF grid should be drawn over data, add it now
  # IF not the grid lines have already been added by `PrepareCanvas()`
  if (grid_on_top == TRUE) {
    plot_mx_cntry_diff <- plot_mx_cntry_diff +
      cohort_lines + period_lines + age_lines
  }

  return(plot_mx_cntry_diff)

}
