set_ts_plot_options <- function(linecolour,
                                ribboncolour,
                                trendlinecolour,
                                envelopecolour,
                                x_label,
                                y_label,
                                y_label_default,
                                x_expand,
                                y_expand) {

  # Set colours using the `rlang::%||%` operator for conciseness
  linecolour <- linecolour %||% "darkorange"
  ribboncolour <- ribboncolour %||% "goldenrod1"
  trendlinecolour <- trendlinecolour %||% "blue"
  envelopecolour <- envelopecolour %||% "lightsteelblue1"

  # Set axis titles
  x_label <- x_label %||% "Year"
  y_label <- y_label %||% y_label_default

  # Set axis limits
  x_expand <- x_expand %||% c(0, 0)
  y_expand <- y_expand %||% c(0, 0)

  return(list(
    linecolour = linecolour,
    ribboncolour = ribboncolour,
    trendlinecolour = trendlinecolour,
    envelopecolour = envelopecolour,
    x_label = x_label,
    y_label = y_label,
    x_expand = x_expand,
    y_expand = y_expand
  ))

}
