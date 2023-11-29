plot_ts <- function(data,
                    suppress_y = "n",
                    title = "auto",
                    linecolour = NULL,
                    trendlinecolour = NULL,
                    envelopecolour = NULL,
                    auccolour = NULL,
                    gridoff = FALSE) {

  # Get start and end years
  first_year <- head(data$year, n=1)
  final_year <- tail(data$year, n=1)

  # Get indicator type
  div_type <- dplyr::first(data$diversity_type, na_rm = TRUE)

  # Get default parameters for indicator
  div_types <- read.csv("diversity_types.csv",
                        na.strings = c(""))
  default_params <- div_types[div_types$label %in% div_type,]

  # y-axis title
  y_label <- default_params$legend

  # Plot title
  if (!is.null(title)) {
    if (title == "auto") {
      title <- paste(default_params$title,
                     if (div_type != "cum_richness") " Trend",
                     " (",
                     first_year,
                     "-",
                     final_year,
                     ")",
                     sep="")
    }
  }

  # Set colours
  if (is.null(linecolour)) linecolour = "darkorange"
  if (is.null(trendlinecolour)) trendlinecolour = "blue"
  if (is.null(envelopecolour)) envelopecolour = "lightsteelblue1"
  if (is.null(auccolour)) auccolour = "orange"


  if ("type" %in% colnames(data)) {

    # Plot line graph with separation by dataset or type
    trend_plot <-
      ggplot2::ggplot(data, aes(x = year,
                              y = diversity_val)) +
      geom_line(aes(color = type),
                size = 0.8) +
      scale_x_continuous(breaks = seq(first_year,
                                      final_year,
                                      by = 5)) +
      labs(x = "Year", y = y_label,
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
            axis.text.y = if (suppress_y=="y") {
              element_blank()
            } else {
              element_text()
            }
      )

  } else {

    # Plot line graph
    trend_plot <-
      ggplot2::ggplot(data, aes(x = year,
                                y = diversity_val)) +
      geom_line(color = linecolour,
                lwd = 1) +
      scale_x_continuous(breaks = breaks_pretty_int(n=10)) +
      scale_y_continuous(breaks = breaks_pretty_int(n = 6)) +
      labs(x = "Year", y = y_label,
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
            axis.text.y = if (suppress_y=="y") {
              element_blank()
            } else {
              element_text()
            }
      )

  }

  if (div_type == "cum_richness") {

    # Plot area under the curve
    trend_plot <- trend_plot +
      geom_ribbon(aes(ymin = 0,
                      ymax = diversity_val),
                  fill = auccolour, alpha = 0.4)


  } else {

    # Plot smoothed trend
    trend_plot <- trend_plot +
      geom_smooth(fill = envelopecolour,
                  lwd = 1,
                  linetype = 0,
                  method = "loess",
                  formula = "y ~ x") +
      stat_smooth(geom = "line",
                  method = "loess",
                  formula = "y ~ x",
                  linetype = "dashed",
                  color = trendlinecolour,
                  alpha = 0.3,
                  lwd = 1)
  }

  trend_plot

}
