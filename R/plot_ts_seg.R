# Function to plot biodiversity indicators calculated over time
#' @export
plot_ts_seg <- function(x,
                        auto_title = NULL,
                        y_label_default = NULL,
                        suppress_y = FALSE,
                        title = "auto",
                        gridoff = FALSE,
                        x_label = NULL,
                        y_label = NULL,
                        min_year = NULL,
                        max_year = NULL,
                        facets = FALSE,
                        facet_label_width = 60,
                        facet_scales = "free",
                        facet_rows = NULL,
                        facet_cols = NULL) {

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

  # Set axis titles
  if (is.null(x_label)) x_label = "Year"
  if (is.null(y_label)) y_label = y_label_default

  # Set type as a factor and remove any types with only 1 occurrence
  x$data$type <- factor(x$data$type, levels = unique(x$data$type))
  x$data <-
    x$data %>%
    dplyr::filter(length(.) > 1, .by = type)

  # Create plot with trend line
  trend_plot <-
    ggplot2::ggplot(x$data, aes(x = year,
                                y = diversity_val)) +
    geom_line(aes(colour = type),
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
          axis.text.y = if (suppress_y==TRUE) {
            element_blank()
          } else {
            element_text()
          },
          strip.text = element_text(face = "italic")
    )

  # Add a smoothed trend
  trend_plot <- trend_plot +
    stat_smooth(aes(colour = type),
                geom = "line",
                method = "loess",
                formula = "y ~ x",
                linetype = "dashed",
                alpha = 0.3,
                lwd = 1)

  if (facets == TRUE) {

    # Use facets to separate multiple species trends
    trend_plot <- trend_plot +
      facet_wrap(vars(type),
                 scales = facet_scales,
                 nrow = facet_rows,
                 ncol = facet_cols,
                 labeller = label_wrap_gen(width = facet_label_width)) +
      theme(legend.position = "none")

  }

  # Show plot
  trend_plot

}
