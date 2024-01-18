# Function to plot biodiversity indicators calculated over time
#' @export
plot_ts <- function(x,
                    auto_title = NULL,
                    y_label_default = NULL,
                    species = NULL,
                    suppress_y = FALSE,
                    title = "auto",
                    linecolour = NULL,
                    trendlinecolour = NULL,
                    envelopecolour = NULL,
                    auccolour = NULL,
                    gridoff = FALSE,
                    x_label = NULL,
                    y_label = NULL,
                    min_year = NULL,
                    max_year = NULL,
                    wrap_length = 60) {



  # Filter by min and max year if set
  if (!is.null(min_year) | !is.null(max_year)) {
    min_year <- ifelse(is.null(min_year), x$first_year, min_year)
    max_year <- ifelse(is.null(max_year), x$last_year, max_year)
    x$data <-
      x$data %>%
      dplyr::filter(year >= min_year) %>%
      dplyr::filter(year <= max_year)
  }

  # Create plot title if title is set to "auto"
  if (!is.null(title)) {
    if (title == "auto") {
      title <- paste(auto_title,
                     " (",
                     min_year,
                     "-",
                     max_year,
                     ")",
                     sep="")
    }
  }


  # Set some defaults for plotting

    # Set colours
    if (is.null(linecolour)) linecolour = "darkorange"
    if (is.null(trendlinecolour)) trendlinecolour = "blue"
    if (is.null(envelopecolour)) envelopecolour = "lightsteelblue1"
    if (is.null(auccolour)) auccolour = "orange"

    # Set axis titles
    if (is.null(x_label)) x_label = "Year"
    if (is.null(y_label)) y_label = y_label_default

  # Create plot with trend line
  trend_plot <-
    ggplot2::ggplot(x$data, aes(x = year,
                                y = diversity_val)) +
    geom_line(colour = linecolour,
              lwd = 1) +
    scale_x_continuous(breaks = breaks_pretty_int(n=10)) +
    scale_y_continuous(breaks = breaks_pretty_int(n = 6)) +
    labs(x = x_label, y = y_label,
         title = title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14),
          panel.grid.major = if (gridoff == TRUE) {
            element_blank()
          } else {
            element_line()
          },
          panel.grid.minor = element_blank(),
          axis.text.y = if (suppress_y==TRUE) {
            element_blank()
          } else {
            element_text()
          },
          strip.text = element_text(face = "italic")
    )


  # if (div_type == "cum_richness") {
  #
  #   # If plotting cumulative richness, colour the area under the curve
  #   trend_plot <- trend_plot +
  #     geom_ribbon(aes(ymin = 0,
  #                     ymax = diversity_val),
  #                 fill = auccolour, alpha = 0.4)

  # } else {

    # Otherwise, add a smoothed trend
    trend_plot <- trend_plot +
      geom_smooth(fill = envelopecolour,
                  lwd = 1,
                  linetype = 0,
                  method = "loess",
                  formula = "y ~ x") +
      stat_smooth(colour = trendlinecolour,
                  geom = "line",
                  method = "loess",
                  formula = "y ~ x",
                  linetype = "dashed",
                  alpha = 0.3,
                  lwd = 1)

  # }

    # Wrap title if longer than wrap_length
    if(!is.null(title)) {
      wrapper <- function(x, ...)
      {
        paste(strwrap(x, ...), collapse = "\n")
      }
      trend_plot <-
        trend_plot +
        labs(title = wrapper(title, wrap_length))
    }

  # Show plot
  trend_plot

}
