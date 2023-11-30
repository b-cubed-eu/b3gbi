plot_ts <- function(data,
                    species = NULL,
                    suppress_y = "n",
                    title = "auto",
                    linecolour = NULL,
                    trendlinecolour = NULL,
                    envelopecolour = NULL,
                    auccolour = NULL,
                    gridoff = FALSE,
                    facet_label_width = 60,
                    facet_scales = "free") {

  # Get indicator type
  div_type <- dplyr::first(data$diversity_type, na_rm = TRUE)

  # Get default parameters for indicator
  div_types <- read.csv("diversity_types.csv",
                        na.strings = c(""))
  default_params <- div_types[div_types$label %in% div_type,]

  # y-axis title
  y_label <- default_params$legend

  # Check that species are selected if plotting species rarity
  # and that the number of species does not exceed max
  if (div_type == "species_rarity") {
    if (is.null(species)) stop("Please input taxonKeys to select species.")
    if (length(species) > 10) stop("Cannot plot more than 10 species at once.")

    # Filter data on selected species
    data <-
      data %>%
      dplyr::filter(taxonKey %in% species) %>%
      dplyr::mutate(taxonKey = factor(taxonKey, levels = unique(taxonKey)))
  }

  # Get start and end years
  first_year <- head(data$year, n=1)
  final_year <- tail(data$year, n=1)

  # Plot title
  if (!is.null(title)) {
    if (title == "auto") {
      title <- paste(default_params$title,
                     if (div_type != "cum_richness") " Trend",
                     if (div_type != "species_rarity") {
                     paste(" (",
                     first_year,
                     "-",
                     final_year,
                     ")",
                     sep = "")
                       },
                     sep="")
    }
  }

  # Set multiline flag to true if plotting by dataset or type
  if (div_type == "occ_by_type" |
      div_type == "occ_by_dataset") {
    multiline <- TRUE
  } else {
    multiline <- FALSE
  }

  if (multiline == FALSE) {

    # Set type to NULL
    type = NULL

    # Set colours
    if (is.null(linecolour)) linecolour = "darkorange"
    if (is.null(trendlinecolour)) trendlinecolour = "blue"
    if (is.null(envelopecolour)) envelopecolour = "lightsteelblue1"
    if (is.null(auccolour)) auccolour = "orange"

  }

  # Plot line graph
  trend_plot <-
    ggplot2::ggplot(data, aes(x = year,
                              y = diversity_val)) +
    geom_line(aes(colour = type),
              colour = linecolour,
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
          },
          strip.text = element_text(face = "italic")
    )


  if (div_type == "cum_richness") {

    # Plot area under the curve
    trend_plot <- trend_plot +
      geom_ribbon(aes(ymin = 0,
                      ymax = diversity_val),
                  fill = auccolour, alpha = 0.4)

  } else {

    # Plot smoothed trend
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

  }

  if (div_type == "species_rarity") {

    # Use facets to plot multiple species trends
    trend_plot <- trend_plot +
      facet_wrap(vars(scientificName),
                 scales = facet_scales,
                 labeller = label_wrap_gen(width = facet_label_width))

  }

  trend_plot

}
