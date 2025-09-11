#' @noRd
create_ts_plot <- function(data,
                           point_line,
                           ci_type,
                           smoothed_trend,
                           pointsize,
                           linecolour,
                           linealpha,
                           linewidth,
                           ribboncolour,
                           ribbonalpha,
                           error_alpha,
                           error_width,
                           error_thickness,
                           trendlinecolour,
                           trendlinealpha,
                           envelopecolour,
                           envelopealpha,
                           smooth_cialpha,
                           smooth_linewidth,
                           smooth_cilinewidth,
                           smooth_linetype,
                           gridoff,
                           suppress_y,
                           x_label,
                           y_label,
                           title,
                           x_expand,
                           y_expand,
                           x_breaks,
                           y_breaks) {

  if ("ll" %in% colnames(data) && "ul" %in% colnames(data)) {
    # Remove NAs from CIs
    data$ll <- ifelse(is.na(data$ll), data$diversity_val, data$ll)
    data$ul <- ifelse(is.na(data$ul), data$diversity_val, data$ul)
  }

  # Create basis of plot
  plot <- ggplot2::ggplot(data, aes(x = year, y = diversity_val))

  # Add smooth trends (LOESS) if specified
  if (smoothed_trend == TRUE) {
    # Add a smoothed trend
    plot <- plot +
      geom_smooth(
        colour = alpha(trendlinecolour, trendlinealpha),
        lwd = smooth_linewidth,
        linetype = smooth_linetype,
        method = "loess",
        formula = "y ~ x",
        se = FALSE)

    # Add smooth trends for confidence limits if available
    if ("ll" %in% colnames(data) && "ul" %in% colnames(data)) {
      plot <- plot +
        geom_smooth(aes(y = ul),
                    colour = alpha(envelopecolour, smooth_cialpha),
                    lwd = smooth_cilinewidth,
                    linetype = "dashed",
                    method = "loess",
                    formula = "y ~ x",
                    se = FALSE) +
        geom_smooth(aes(y = ll),
                    colour = alpha(envelopecolour, smooth_cialpha),
                    lwd = smooth_cilinewidth,
                    linetype = "dashed",
                    method = "loess",
                    formula = "y ~ x",
                    se = FALSE) +
        geom_ribbon(aes(ymin = predict(loess(ll ~ year)),
                        ymax = predict(loess(ul ~ year))),
                    alpha = envelopealpha,
                    fill = envelopecolour)
    }
  }

  # If upper and lower limits are present, add errorbars
  if ("ll" %in% colnames(data) && "ul" %in% colnames(data)) {
    if (ci_type == "error_bars") {
      plot <- plot +
        geom_errorbar(aes(ymin = ll, ymax = ul),
                      colour = ribboncolour,
                      alpha = error_alpha,
                      width = error_width,
                      linewidth = error_thickness)
    } else {
      plot <- plot +
        geom_ribbon(aes(ymin = ll, ymax = ul),
                    alpha = ribbonalpha,
                    fill = ribboncolour)
    }

  }

  if (point_line == "point") {
    plot <- plot +
      geom_point(colour = linecolour,
                 alpha = linealpha,
                 size = pointsize)
  } else {
    plot <- plot +
      geom_line(aes(group = 1),
                colour = linecolour,
                alpha = linealpha,
                lwd = linewidth)
  }


  plot <- plot +
    scale_x_continuous(breaks = breaks_pretty_int(n = x_breaks),
                       expand = expansion(mult = x_expand)) +
    scale_y_continuous(breaks = breaks_pretty_int(n = y_breaks),
                       expand = expansion(mult = y_expand)) +
    labs(x = x_label, y = y_label, title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14),
          panel.grid.major = if (gridoff == TRUE) {
            element_blank()
          } else {
            element_line()
          },
          panel.grid.minor = element_blank(),
          axis.text.y = if (suppress_y == TRUE) {
            element_blank()
          } else {
            element_text()
          },
          strip.text = element_text(face = "italic"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
    )

  # Exit function
  return(plot)

}
