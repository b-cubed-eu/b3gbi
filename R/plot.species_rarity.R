#' @export
plot.species_rarity <- function(x,
                                species = NULL,
                                auto_title = NULL,
                                y_label_default = NULL,
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
                                facet_label_width = 60,
                                facet_scales = "free",
                                facet_rows = NULL,
                                facet_cols = NULL){

  stopifnot_error("Incorrect object class. Must be class 'species_rarity'.", inherits(x, "species_rarity"))

  stopifnot_error("Incorrect object class. Must be class 'indicator_ts'.", inherits(x, "indicator_ts"))

  # Check that species are selected and filter data
  if (is.null(species)) {

    # stop("Please enter the taxonKeys for the species you want to plot in numeric format.")
    stop("Please enter either the species names or the numeric taxonKeys for the species you want to plot.")

  } else if (is.numeric(species)) {

    # Get rarity for selected species
    x$data <-
      x$data %>%
      dplyr::filter(taxonKey %in% species) %>%
      {if(nrow(.) < 1)
        stop("No matching taxonKeys. Please check that you have entered them correctly.")
        else (.)
      } %>%
      dplyr::mutate(taxonKey = factor(taxonKey,
                                      levels = unique(taxonKey)))

  } else {

    # Get rarity for selected species
    x$data <-
      x$data %>%
      dplyr::filter(grepl(paste("^", species, collapse="|",sep=""), scientificName)) %>%
      { if(nrow(.) < 1)
        stop("No matching species. Please check that you have entered the names correctly.")
        else (.) } %>%
      dplyr::arrange(scientificName, grepl(paste("^", species, collapse="|",sep=""), scientificName))

  }

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
      title <- paste("Species Rarity (",
                     min_year,
                     "-",
                     max_year,
                     ")",
                     sep="")
    }
  }

  # Set type to NULL
  type = NULL

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
      geom_line(aes(colour = type),
                colour = linecolour,
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

    # Add a smoothed trend
    trend_plot <- trend_plot +
      geom_smooth(aes(fill = type),
                  fill = envelopecolour,
                  lwd = 1,
                  linetype = 0,
                  method = "loess",
                  formula = "y ~ x") +
      stat_smooth(aes(colour = type),
                  colour = trendlinecolour,
                  geom = "line",
                  method = "loess",
                  formula = "y ~ x",
                  linetype = "dashed",
                  alpha = 0.3,
                  lwd = 1)

    # Use facets to separate multiple species trends
    trend_plot <- trend_plot +
      facet_wrap(vars(scientificName),
                 scales = facet_scales,
                 nrow = facet_rows,
                 ncol = facet_cols,
                 labeller = label_wrap_gen(width = facet_label_width))

    # Show plot
    trend_plot

}
