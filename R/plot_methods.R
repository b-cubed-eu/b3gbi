#' @title Plot Occurrences Segregated by Dataset
#'
#' @description  Creates a time series plot of total occurrences, with the
#'  occurrences visually segregated by their dataset of origin. Requires an
#'  'indicator_ts' object.
#'
#' @param x An object containing occurrence data segregated by dataset. Must
#'   be of class 'occ_by_dataset' and an 'indicator_ts' object.
#' @param x_breaks (Optional) Integer giving desired number of breaks for the
#'  x-axis. (May not return exactly the number requested.)
#' @param facet_scales Controls y-axis scaling across facets. Use "free_y" to
#'  allow independent scaling, or "fixed" for a common scale.
#' @param facet_rows (Optional) Number of rows of facets.
#' @param facet_cols (Optional) Number of columns of facets.
#' @param facet_label_width (Optional) Controls the maximum width of facet
#'  labels.
#' @param max_datasets (Optional) Maximum number of datasets to include in the
#'  plot. Datasets are selected based on having the highest number of
#'  occurrences. *Note that increasing this too much could result in high memory
#'  usage and/or make the plot very difficult to read.
#' @param min_occurrences (Optional)  Minimum total number of occurrences for a
#'  dataset to be included in the plot.
#' @param ... Additional arguments passed to the internal plotting function
#'  (`plot_ts_seg`). See its documentation for details.
#'
#' @return A ggplot object representing a time series plot with occurrences
#'  segregated by dataset.
#'
#' @examples
#' # Assuming you have an 'indicator_ts' object named 'occ_by_dataset_ts'
#' plot.occ_by_dataset(occ_by_dataset_ts)
#'
#' # Only plot datasets with at least 100 occurrences:
#' plot.occ_by_dataset(occ_by_dataset_ts, min_occurrences = 100)
#'
#' @noRd
plot.occ_by_dataset <- function(x,
                                x_breaks = 6,
                                facet_scales = "free_y",
                                facet_rows = NULL,
                                facet_cols = NULL,
                                facet_label_width = 60,
                                max_datasets = 20,
                                min_occurrences = NULL,
                                ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'occ_by_dataset'.",
    inherits(x, "occ_by_dataset")
  )

  if (!inherits(x, "indicator_ts")) {
    stop("Incorrect object class. Must be class 'indicator_ts'.")
  }

  type <- diversity_val <- totalocc <- NULL

  # Set defaults
  y_label_default <- "Occurrences"
  auto_title <- "Total Occurrences (Segregated by Dataset)"

  # Filter data sets
  x$data$type <- factor(x$data$type, levels = unique(x$data$type))

  # Filter out data sets with insufficient data
  x$data <- x$data %>%
    dplyr::group_by(type) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup()

  if (!is.null(min_occurrences)) {
    # Filter out datasets with fewer than the minimum occurrences
    x$data <- x$data %>%
      dplyr::group_by(type) %>%
      dplyr::mutate(totalocc = sum(diversity_val)) %>%
      dplyr::filter(totalocc >= min_occurrences) %>%
      dplyr::ungroup() %>%
      dplyr::select(-totalocc)
  }

  # Select top datasets
  datasets <- x$data %>%
    dplyr::group_by(type) %>%
    dplyr::summarize(totalocc = sum(diversity_val), .groups = 'drop') %>%
    dplyr::slice_max(order_by = totalocc, n = max_datasets)

  x$data <- x$data %>%
    dplyr::filter(type %in% datasets$type)

  # Call the plot_ts function
  plot <- plot_ts(x,
                        y_label_default = y_label_default,
                        auto_title = auto_title,
                        x_breaks = x_breaks,
                        ...)

  # Facet the plot according to dataset type
  plot <- plot +
    facet_wrap(vars(type),
               scales = facet_scales,
               nrow = facet_rows,
               ncol = facet_cols,
               labeller = label_wrap_gen(width = facet_label_width)) +
    theme(legend.position = "none")

  return(plot)

}

#' @title Plot Occurrences Segregated by Type
#'
#' @description  Creates a time series plot of total occurrences, with the
#'  occurrences visually segregated by their type. Requires an 'indicator_ts'
#'  object.
#'
#' @param x An object containing  occurrence data segregated by type. Must
#'  be of class 'occ_by_type' and an 'indicator_ts' object.
#' @param x_breaks (Optional)  Integer giving desired number of breaks for the
#'  x-axis. (May not return exactly the number requested.)
#' @param facet_scales Controls y-axis scaling across facets.  Use "free_y" to
#'  allow independent scaling, or "fixed" for a common scale.
#' @param facet_rows (Optional) Number of rows of facets.
#' @param facet_cols (Optional) Number of columns of facets.
#' @param facet_label_width (Optional) Controls the maximum width of facet
#'  labels.
#' @param ... Additional arguments passed to the internal plotting function
#'  (`plot_ts_seg`). See its documentation for details.
#'
#' @return A ggplot object representing a time series plot with occurrences
#'  segregated by type.
#'
#' @examples
#' # Assuming you have an 'indicator_ts' object named 'occ_by_type_ts'
#' plot.occ_by_type(occ_by_type_ts)
#'
#' @noRd
plot.occ_by_type <- function(x,
                             x_breaks = 6,
                             facet_scales = "free_y",
                             facet_rows = NULL,
                             facet_cols = NULL,
                             facet_label_width = 60,
                             ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'occ_by_type'.",
    inherits(x, "occ_by_type")
  )

  if (!inherits(x, "indicator_ts")) {
    stop("Incorrect object class. Must be class 'indicator_ts'.")
  }

  type <- NULL

  # Set defaults
  y_label_default <- "Occurrences"
  auto_title <- "Total Occurrences (Segregated by Type)"

  # Filter types
  x$data$type <- factor(x$data$type, levels = unique(x$data$type))

  # Filter out types with insufficient data
  x$data <- x$data %>%
    dplyr::group_by(type) %>%
    dplyr::filter(dplyr::n() >= 2) %>%
    dplyr::ungroup()

  # Call the plot_ts function
  plot <- plot_ts(x,
                  y_label_default = y_label_default,
                  auto_title = auto_title,
                  x_breaks = x_breaks,
                  ...)

  # Facet the plot according to type
  plot <- plot +
    facet_wrap(vars(type),
               scales = facet_scales,
               nrow = facet_rows,
               ncol = facet_cols,
               labeller = label_wrap_gen(width = facet_label_width)) +
    theme(legend.position = "none")

  return(plot)

}

#' @export
plot.spec_range <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'spec_range'.",
    inherits(x, "spec_range")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Cells Occupied"
    auto_title <- paste("Species Range Size", sep = "")

    # Call generalized plot_ts function
    plot_species_ts(x,
                    y_label_default = y_label_default,
                    auto_title = auto_title,
                    ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    suppress_legend <- TRUE
    auto_title <- paste("Species Range", sep = "")

    # Call generalized plot_map function
    plot_species_map(x,
                     suppress_legend = suppress_legend,
                     auto_title = auto_title,
                     ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
      )

  }
}

#' @export
plot.spec_occ <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'spec_occ'.",
    inherits(x, "spec_occ") | inherits(x, "spec_range")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- paste("Species Occurrences", sep = "")

    # Call generalized plot_ts function
    plot_species_ts(x,
                    y_label_default = y_label_default,
                    auto_title = auto_title,
                    ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Occurrences"
    auto_title <- paste("Species Occurrences", sep = "")

    # Call generalized plot_map function
    plot_species_map(x,
                     leg_label_default = leg_label_default,
                     auto_title = auto_title,
                     ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
      )

  }
}


#' @export
plot.cum_richness <- function(x, envelopecolour = NULL, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'cum_richness'.",
    inherits(x, "cum_richness")
  )

  if (!inherits(x, "indicator_ts")) {
    stop("Incorrect object class. Must be class 'indicator_ts'.")
  }

  # Set defaults
  y_label_default <- "Cumulative Species Richness"
  auto_title <- "Cumulative Species Richness"

  # Call generalized plot_map function
  plot_ts(x,
          y_label_default = y_label_default,
          auto_title = auto_title,
          smoothed_trend = FALSE,
          ...)

}


#' @export
plot.pielou_evenness <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'pielou_evenness'.",
    inherits(x, "pielou_evenness")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Evenness"
    auto_title <- paste("Pielou's Evenness Trend", sep = "")

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Evenness"
    auto_title <- paste("Pielou's Evenness", sep = "")

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}


#' @export
plot.williams_evenness <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'williams_evenness'.",
    inherits(x, "williams_evenness")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Evenness"
    auto_title <- paste("Williams' Evenness Trend", sep = "")

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Evenness"
    auto_title <- paste("Williams' Evenness", sep = "")

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
      )

  }
}


#' @export
plot.tax_distinct <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'tax_distinct'.",
    inherits(x, "tax_distinct")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Taxonomic Distinctness"
    auto_title <- "Taxonomic Distinctness Trend"

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Taxonomic Distinctness"
    auto_title <- "Taxonomic Distinctness"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}


#' @export
plot.occ_density <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'occ_density'.",
    inherits(x, "occ_density")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Mean Occurrences \nper km^2"
    auto_title <- "Trend of Mean Occurrence Density"

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Occurrences \nper km^2"
    auto_title <- "Density of Occurrences"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}

#' @export
plot.newness <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'newness'.", inherits(x, "newness")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Mean Year of Occurrence"
    auto_title <- "Trend of Mean Year of Occurrence"

    # Call generalized plot_map function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Mean Year of \nOccurrence"
    auto_title <- "Mean Year of Occurrence"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_map'.")

  }
}


#' @export
plot.total_occ <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'total_occ'.",
    inherits(x, "total_occ")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- "Trend of Total Occurrences"

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Occurrences"
    auto_title <- "Total Occurrences"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}

#' @export
plot.area_rarity <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'area_rarity'.",
    inherits(x, "area_rarity")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Mean of Rarity (Summed by Cell)"
    auto_title <- "Area-Based Rarity Trend"

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Summed Rarity"
    auto_title <- "Area-Based Rarity"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}

#' @export
plot.ab_rarity <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'ab_rarity'.",
    inherits(x, "ab_rarity")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Mean of Rarity (Summed by Cell)"
    auto_title <- "Abundance-Based Rarity Trend"

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Summed Rarity"
    auto_title <- "Abundance-Based Rarity"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}

#' @export
plot.hill2 <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'hill2'.", inherits(x, "hill2")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Hill-Simpson Diversity"
    auto_title <-
      "Hill-Simpson Diversity Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Hill-Simpson Diversity"
    auto_title <-
      "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}

#' @export
plot.hill1 <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'hill1'.", inherits(x, "hill1")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Hill-Shannon Diversity"
    auto_title <-
      "Hill-Shannon Diversity Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Hill-Shannon Diversity"
    auto_title <-
      "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}

#' @export
plot.hill0 <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'hill0'.", inherits(x, "hill0")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness"
    auto_title <-
      "Species Richness Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Richness"
    auto_title <- "Species Richness (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}

#' @export
plot.obs_richness <- function(x, ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'obs_richness'.",
    inherits(x, "obs_richness")
  )

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness"
    auto_title <- "Observed Species Richness Trend"

    # Call generalized plot_ts function
    plot_ts(x,
            y_label_default = y_label_default,
            auto_title = auto_title,
            ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Richness"
    auto_title <- "Observed Species Richness"

    # Call generalized plot_map function
    plot_map(x,
             leg_label_default = leg_label_default,
             auto_title = auto_title,
             ...)

  } else {

    stop(
      "Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'."
    )

  }
}

#' @export
plot.occ_turnover <- function(x, auccolour = NULL,  ...) {

  stopifnot_error(
    "Incorrect object class. Must be class 'occ_turnover'.",
    inherits(x, "occ_turnover")
  )

  if (!inherits(x, "indicator_ts")) {
    stop("Incorrect object class. Must be class 'indicator_ts'.")
  }

  # Set defaults
  y_label_default <- "Occupancy Turnover"
  auto_title <- "Occupancy Turnover"

  # Call generalized plot_map function
  plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

}



#' @title Plot Biodiversity Indicator Trend
#'
#' @description  Creates a time series plot of a calculated biodiversity
#'   indicator, with an optional smoothed trendline, and visualizes uncertainty.
#'
#' @param x An 'indicator_ts' object containing a time series of indicator
#'  values.
#' @param min_year (Optional)  Earliest year to include in the plot.
#' @param max_year (Optional)  Latest year to include in the plot.
#' @param title (Optional) Plot title. Replace "auto" with your own title if you
#'  want a custom title or if calling the function manually.
#' @param auto_title (Optional) Text for automatic title generation, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param y_label_default (Optional) Default label for the y-axis, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param suppress_y (Optional) If TRUE, suppresses y-axis labels.
#' @param smoothed_trend (Optional) If TRUE, plot a smoothed trendline over time
#' (`stats::loess()`).
#' @param linecolour (Optional) Colour for the indicator line or points.
#'   Default is darkorange.
#' @param linealpha (Optional) Transparency for indicator line or points.
#'  Default is 0.8.
#' @param ribboncolour (Optional) Colour for the bootstrapped confidence
#'  intervals. Default is goldenrod1. Set to "NA" if you don't want to plot the
#'  CIs.
#' @param ribbonalpha (Optional) Transparency for indicator confidence interval
#'  ribbon (if ci_type = "ribbon"). Default is 0.2.
#' @param error_alpha (Optional) Transparency for indicator error bars (if
#'  ci_type = "error_bar"). Default is 1.
#' @param trendlinecolour (Optional) Colour for the smoothed trendline.
#'   Default is blue.
#' @param trendlinealpha (Optional) Transparency for the smoothed trendline.
#'  Default is 0.5.
#' @param envelopecolour (Optional) Colour for the uncertainty envelope.
#'   Default is lightsteelblue.
#' @param envelopealpha (Optional) Transparency for the smoothed trendline
#'  envelope. Default is 0.2.
#' @param smooth_cialpha (Optional) Transparency for the smoothed lines forming
#'  the edges of the trendline envelope. Default is 1.
#' @param point_line (Optional) Whether to plot the indicator as a line or a
#'  series of points. Options are "line" or "point". Default is "point".
#' @param pointsize (Optional) Size of the points if point_line = "point".
#'  Default is 2.
#' @param linewidth (Optional) Width of the line if point_line = "line".
#'  Default is 1.
#' @param ci_type (Optional) Whether to plot bootstrapped confidence intervals
#'  as a "ribbon" or "error_bars". Default is "error_bars".
#' @param error_width (Optional) Width of error bars if ci_type = "error_bars".
#'  Default is 1. Note that unlike the default 'width' parameter in
#'  geom_errorbar, 'error_width' is NOT dependent on the number of data points
#'  in the plot. It is automatically scaled to account for this. Therefore the
#'  width you select will be consistent relative to the plot width even if you
#'  change 'min_year' and 'max_year'.
#' @param error_thickness (Optional) Thickness of error bars if
#'  ci_type = "error_bars". Default is 1.
#' @param smooth_linetype (Optional) Type of line to plot for smoothed
#'  trendline. Default is "solid".
#' @param smooth_linewidth (Optional) Line width for smoothed trendline.
#'  Default is 1.
#' @param smooth_cilinewidth (Optional) Line width for smoothed trendline
#'  confidence intervals. Default is 1.
#' @param gridoff (Optional) If TRUE, hides gridlines.
#' @param x_label (Optional) Label for the x-axis.
#' @param y_label (Optional) Label for the y-axis.
#' @param x_expand (Optional)  Expansion factor to expand the x-axis beyond the
#'  data. Left and right values are required in the form of c(0.1, 0.2) or
#'  simply 0.1 to apply the same value to each side. Default is 0.05.
#' @param y_expand (Optional)  Expansion factor to expand the y-axis beyond the
#'  data. Lower and upper values are required in the form of c(0.1, 0.2) or
#'  simply 0.1 to apply the same value to the top and bottom. Default is 0.05.
#' @param x_breaks (Optional) Integer giving desired number of breaks for x
#'  axis. (May not return exactly the number requested.)
#' @param y_breaks (Optional) Integer giving desired number of breaks for y
#'  axis. (May not return exactly the number requested.)
#' @param wrap_length (Optional) Maximum title length before wrapping to a new
#'  line.
#'
#' @return A ggplot object representing the biodiversity indicator time series
#'  plot. Can be customized using ggplot2 functions.
#'
#' @examples
#' # default colours:
#' plot_ts(example_indicator_ts1,
#'         y_label = "Species Richness",
#'         title = "Observed Species Richness: Mammals in Denmark")
#'
#' # custom colours:
#' plot_ts(example_indicator_ts1,
#'         y_label = "Species Richness",
#'         title = "Observed Species Richness: Mammals in Denmark",
#'         linecolour = "thistle",
#'         trendlinecolour = "forestgreen",
#'         envelopecolour = "lightgreen")
#' @export
plot_ts <- function(x,
                    min_year = NULL,
                    max_year = NULL,
                    title = "auto",
                    auto_title = NULL,
                    y_label_default = NULL,
                    suppress_y = FALSE,
                    smoothed_trend = TRUE,
                    linecolour = NULL,
                    linealpha = 0.8,
                    ribboncolour = NULL,
                    ribbonalpha = 0.2,
                    error_alpha = 1,
                    trendlinecolour = NULL,
                    trendlinealpha = 0.5,
                    envelopecolour = NULL,
                    envelopealpha = 0.2,
                    smooth_cialpha = 1,
                    point_line = c("point",
                                   "line"),
                    pointsize = 2,
                    linewidth = 1,
                    ci_type = c("error_bars",
                                "ribbon"),
                    error_width = 1,
                    error_thickness = 1,
                    smooth_linetype = c("solid",
                                        "dashed",
                                        "dotted",
                                        "dotdash",
                                        "longdash",
                                        "twodash"),
                    smooth_linewidth = 1,
                    smooth_cilinewidth = 1,
                    gridoff = FALSE,
                    x_label = NULL,
                    y_label = NULL,
                    x_expand = 0.05,
                    y_expand = 0.05,
                    x_breaks = 10,
                    y_breaks = 6,
                    wrap_length = 60
                    ) {

  # Set variable definitions to NULL where required
  year <- diversity_val <- ul <- ll <- NULL

  # Match arguments
  point_line <- match.arg(point_line)
  ci_type <- match.arg(ci_type)
  smooth_linetype <- match.arg(smooth_linetype)

  # Check that object to plot is the correct class
  if (!inherits(x, "indicator_ts")) {
    stop("Incorrect object class. Must be class 'indicator_ts'.")
  }

  # Filter by min and max year if set
  filtered_data <- filter_data_years(x, min_year, max_year)

  # Retrieve filtered data and year range
  x$data <- filtered_data$data
  min_year <- filtered_data$min_year
  max_year <- filtered_data$max_year

  # Check there are at least 3 years to plot if smoothed trend is requested
  if ((max_year - min_year) < 2 && smoothed_trend == TRUE) {
    smoothed_trend <- FALSE
    message("Could not perform loess smooth due to insufficient time points.")
  }

  # Create plot title if title is set to "auto"
  if (!is.null(title) && title == "auto") {
    title <- create_auto_title(auto_title, min_year, max_year)
  }

  # Set some default options for plotting
  plot_options <- set_ts_plot_options(
    linecolour, ribboncolour, trendlinecolour, envelopecolour,
    x_label, y_label, y_label_default, x_expand, y_expand
  )

  # Unpack the default options
  list2env(plot_options, envir = environment())

  # Adjust error bar width according to number of years being plotted
  error_width <- (error_width * (max_year - min_year)) / 100

  # Create the plot
  plot <- create_ts_plot(
    data = x$data,
    point_line = point_line,
    ci_type = ci_type,
    smoothed_trend = smoothed_trend,
    pointsize = pointsize,
    linecolour = linecolour,
    linealpha = linealpha,
    linewidth = linewidth,
    ribboncolour = ribboncolour,
    ribbonalpha = ribbonalpha,
    error_alpha = error_alpha,
    error_width = error_width,
    error_thickness = error_thickness,
    trendlinecolour = trendlinecolour,
    trendlinealpha = trendlinealpha,
    envelopecolour = envelopecolour,
    envelopealpha = envelopealpha,
    smooth_cialpha = smooth_cialpha,
    smooth_linetype = smooth_linetype,
    smooth_linewidth = smooth_linewidth,
    smooth_cilinewidth = smooth_cilinewidth,
    gridoff = gridoff,
    suppress_y = suppress_y,
    x_label = x_label,
    y_label = y_label,
    title = title,
    x_expand = x_expand,
    y_expand = y_expand,
    x_breaks = x_breaks,
    y_breaks = y_breaks
  )

  # Wrap title if longer than wrap_length
  if (!is.null(title)) {
    plot <- plot +
      ggplot2::labs(title = wrapper(title, wrap_length))
  }

  # Exit function
  return(plot)

}

#' @title Plot Occurrence Trends or Range Size Trends for Individual Species
#'
#' @description  Creates time series plots of species occurrences or species
#'  range sizes, with an optional smoothed trendline, and visualizes
#'  uncertainty.
#'
#' @param x An 'indicator_ts' object containing time series of indicator values
#'   matched to species names and/or taxon keys.
#' @param species Species you want to map occurrences for. Can be either
#'  numerical taxonKeys or species names. Partial species names can be used
#'  (the function will try to match them).
#' @param single_plot (Optional) By default all species occurrence time series
#'  will be combined into a single multi-panel plot. Set this to FALSE to plot
#'  each species separately.
#' @param min_year (Optional)  Earliest year to include in the plot.
#' @param max_year (Optional)  Latest year to include in the plot.
#' @param title (Optional) Plot title. Replace "auto" with your own title if you
#'  want a custom title or if calling the function manually.
#' @param auto_title (Optional) Text for automatic title generation, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param y_label_default (Optional) Default label for the y-axis, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param suppress_y (Optional) If TRUE, suppresses y-axis labels.
#' @param smoothed_trend (Optional) If TRUE, plot a smoothed trendline over time
#' (`stats::loess()`).
#' @param linecolour (Optional) Colour for the indicator line or points.
#'   Default is darkorange.
#' @param linealpha (Optional) Transparency for indicator line or points.
#'  Default is 0.8.
#' @param ribboncolour (Optional) Colour for the bootstrapped confidence
#'  intervals. Default is goldenrod1. Set to "NA" if you don't want to plot the
#'  CIs.
#' @param ribbonalpha (Optional) Transparency for indicator confidence interval
#'  ribbon (if ci_type = "ribbon"). Default is 0.2.
#' @param error_alpha (Optional) Transparency for indicator error bars (if
#'  ci_type = "error_bar"). Default is 1.
#' @param trendlinecolour (Optional) Colour for the smoothed trendline.
#'   Default is blue.
#' @param trendlinealpha (Optional) Transparency for the smoothed trendline.
#'  Default is 0.5.
#' @param envelopecolour (Optional) Colour for the uncertainty envelope.
#'   Default is lightsteelblue.
#' @param envelopealpha (Optional) Transparency for the smoothed trendline
#'  envelope. Default is 0.2.
#' @param smooth_cialpha (Optional) Transparency for the smoothed lines forming
#'  the edges of the trendline envelope. Default is 1.
#' @param point_line (Optional) Whether to plot the indicator as a line or a
#'  series of points. Options are "line" or "point". Default is "point".
#' @param pointsize (Optional) Size of the points if point_line = "point".
#'  Default is 2.
#' @param linewidth (Optional) Width of the line if point_line = "line".
#'  Default is 1.
#' @param ci_type (Optional) Whether to plot bootstrapped confidence intervals
#'  as a "ribbon" or "error_bars". Default is "error_bars".
#' @param error_width (Optional) Width of error bars if ci_type = "error_bars".
#'  Default is 1. Note that unlike the default 'width' parameter in
#'  geom_errorbar, 'error_width' is NOT dependent on the number of data points
#'  in the plot. It is automatically scaled to account for this. Therefore the
#'  width you select will be consistent relative to the plot width even if you
#'  change 'min_year' and 'max_year'.
#' @param error_thickness (Optional) Thickness of error bars if
#'  ci_type = "error_bars". Default is 1.
#' @param smooth_linetype (Optional) Type of line to plot for smoothed
#'  trendline. Default is "solid".
#' @param smooth_linewidth (Optional) Line width for smoothed trendline. Default
#'  is 1.
#' @param smooth_cilinewidth (Optional) Line width for smoothed trendline
#'  confidence intervals. Default is 1.
#' @param gridoff (Optional) If TRUE, hides gridlines.
#' @param x_label (Optional) Label for the x-axis.
#' @param y_label (Optional) Label for the y-axis.
#' @param x_expand (Optional)  Expansion factor to expand the x-axis beyond the
#'  data. Left and right values are required in the form of c(0.1, 0.2) or
#'  simply 0.1 to apply the same value to both sides. Default is 0.05.
#' @param y_expand (Optional)  Expansion factor to expand the y-axis beyond the
#'  data. Lower and upper values are required in the form of c(0.1, 0.2) or
#'  simply 0.1 to apply the same value to the top and bottom. Default is 0.05.
#' @param x_breaks (Optional) Integer giving desired number of breaks for x
#'  axis. (May not return exactly the number requested.)
#' @param y_breaks (Optional) Integer giving desired number of breaks for y
#'  axis. (May not return exactly the number requested.)
#' @param wrap_length (Optional) Maximum title length before wrapping to a new
#'  line.
#'
#' @return A ggplot object representing species range or occurrence time series
#'  plot(s). Can be customized using ggplot2 functions.
#'
#' @examples
#' spec_occ_ts_mammals_denmark <- spec_occ_ts(example_cube_1,
#'                                         level = "country",
#'                                         region = "Denmark")
#' # default colours:
#' plot_species_ts(spec_occ_ts_mammals_denmark, c(2440728, 4265185))
#'
#' # custom colours:
#' plot_species_ts(spec_occ_ts_mammals_denmark, c(2440728, 4265185),
#'         linecolour = "thistle",
#'         trendlinecolour = "forestgreen",
#'         envelopecolour = "lightgreen")
#' @export
plot_species_ts <- function(x,
                            species,
                            single_plot = TRUE,
                            min_year = NULL,
                            max_year = NULL,
                            title = "auto",
                            auto_title = NULL,
                            y_label_default = NULL,
                            suppress_y = FALSE,
                            smoothed_trend = TRUE,
                            linecolour = NULL,
                            linealpha = 0.8,
                            ribboncolour = NULL,
                            ribbonalpha = 0.2,
                            error_alpha = 1,
                            trendlinecolour = NULL,
                            trendlinealpha = 0.5,
                            envelopecolour = NULL,
                            envelopealpha = 0.2,
                            smooth_cialpha = 1,
                            point_line = c("point",
                                           "line"),
                            pointsize = 2,
                            linewidth = 1,
                            ci_type = c("error_bars",
                                        "ribbon"),
                            error_width = 1,
                            error_thickness = 1,
                            smooth_linetype = c("solid",
                                                "dashed",
                                                "dotted",
                                                "dotdash",
                                                "longdash",
                                                "twodash"),
                            smooth_linewidth = 1,
                            smooth_cilinewidth = 1,
                            gridoff = FALSE,
                            x_label = NULL,
                            y_label = NULL,
                            x_expand = 0.05,
                            y_expand = 0.05,
                            x_breaks = 10,
                            y_breaks = 6,
                            wrap_length = 60
                            ) {

  # Set variable definitions to NULL where required
  year <- ll <- ul <- diversity_val <- NULL

  # Match arguments
  point_line <- match.arg(point_line)
  ci_type <- match.arg(ci_type)
  smooth_linetype <- match.arg(smooth_linetype)

  # Check that object to plot is the correct class
  if (!inherits(x, "indicator_ts")) {
    stop("Incorrect object class. Must be class 'indicator_ts'.")
  }

  # Filter by min and max year if set
  filtered_data <- filter_data_years(x, min_year, max_year)

  # Unpack filtered data and year range
  x$data <- filtered_data$data
  min_year <- filtered_data$min_year
  max_year <- filtered_data$max_year

  # Check there are at least 3 years to plot if smoothed trend is requested
  if ((max_year - min_year) < 2 && smoothed_trend == TRUE) {
    smoothed_trend <- FALSE
    message("Could not perform loess smooth due to insufficient time points.")
  }

  # Check that species argument has been provided
  if (is.null(species)) {
    stop(paste0(
      "Please enter either the species names or the numeric taxonKeys for the ",
      "species you want to plot."
    ))
  }

  # Split data by species
  split_so <- if (is.numeric(species)) {
    get_occurrences_and_split(x$data, "taxonKey", species)
  } else {
    get_occurrences_and_split(x$data, "scientificName", species)
  }

  # Get unique species names for plot labelling
  sci_names <- split_so %>% purrr::map(function(x) unique(x$scientificName))

  # Create plot title if title is set to "auto"
  if (!is.null(title) && title == "auto") {
      title <- create_auto_title(auto_title, min_year, max_year)
  }

  # Set some defaults for plotting
  plot_options <- set_ts_plot_options(
    linecolour, ribboncolour, trendlinecolour, envelopecolour,
    x_label, y_label, y_label_default, x_expand, y_expand
  )

  # Unpack the default options
  list2env(plot_options, envir = environment())

  # Adjust error bar width according to number of years being plotted
  error_width <- (error_width * (max_year - min_year)) / 100

  # Create the plots for each species
  plot <- purrr::map(split_so, ~ create_ts_plot(
    data = .x,
    point_line = point_line,
    ci_type = ci_type,
    smoothed_trend = smoothed_trend,
    pointsize = pointsize,
    linecolour = linecolour,
    linealpha = linealpha,
    linewidth = linewidth,
    ribboncolour = ribboncolour,
    ribbonalpha = ribbonalpha,
    error_alpha = error_alpha,
    error_width = error_width,
    error_thickness = error_thickness,
    trendlinecolour = trendlinecolour,
    trendlinealpha = trendlinealpha,
    envelopecolour = envelopecolour,
    envelopealpha = envelopealpha,
    smooth_cialpha = smooth_cialpha,
    smooth_linewidth = smooth_linewidth,
    smooth_cilinewidth = smooth_cilinewidth,
    smooth_linetype = smooth_linetype,
    gridoff = gridoff,
    suppress_y = suppress_y,
    x_label = x_label,
    y_label = y_label,
    title = title,
    x_expand = x_expand,
    y_expand = y_expand,
    x_breaks = x_breaks,
    y_breaks = y_breaks
  ))

  # Name each plot with the corresponding species name
  names(plot) <- sci_names

  # Combine plots using wrap_plots function from patchwork
  if ((length(plot) > 0 && single_plot == TRUE) || length(plot) == 1) {
    plot <- patchwork::wrap_plots(plot) +
      plot_annotation_int(title = wrapper(title, title_wrap_length),
                          theme = theme(plot.title = element_text(size = 20)))
  # Or create each plot separately if single_plot is FALSE
  } else if (length(plot) > 1 && single_plot == FALSE) {
    message(paste0(
      "Option single_plot set to false. Creating separate plot for each ",
      "species.\n\n"
    ))
  }

  # Exit function
  return(plot)

}



#' @title Plot Biodiversity Indicator Map
#'
#' @description Creates a map visualization of a calculated biodiversity
#'  indicator, providing customization options.
#'
#' @param x An 'indicator_map' object containing indicator values associated
#'  with map grid cells.
#' @param title (Optional) Plot title. Replace "auto" with your own title if you
#'  want a custom title or if calling the function manually.
#' @param auto_title (Optional) Text for automatic title generation, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param leg_label_default (Optional) Default label for the legend, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param xlims  (Optional) Custom x-axis limits.
#' @param ylims (Optional) Custom y-axis limits.
#' @param trans (Optional) Scale transformation for the fill gradient
#'   (e.g., 'log').
#' @param bcpower (Optional) Power parameter for the Box-Cox, modulus, or
#'   Yeo-Johnson transformations.
#' @param breaks (Optional) Break points for the legend scale.
#' @param labels (Optional) Labels for legend scale break points.
#' @param crop_to_grid (Optional) If TRUE, the grid will determine the edges of
#'  the map. Default is FALSE.
#' @param panel_bg (Optional) Background colour for the map panel.
#' @param land_fill_colour (Optional) Colour for the land area outside of the
#'  grid (if surround = TRUE). Default is "grey85".
#' @param legend_title (Optional) Title for the plot legend.
#' @param legend_limits (Optional) Limits for the legend scale.
#' @param legend_title_wrap_length (Optional) Maximum legend title length before
#'  wrapping to a new line.
#' @param title_wrap_length (Optional) Maximum title length before wrapping to a
#'  new line.
#' @param visible_gridlines (Optional) Show gridlines between cells. Default is
#'  TRUE.
#' @param layers (Optional) Additional rnaturalearth layers to plot, e.g.
#'  c("reefs", "playas").
#' @param scale (Optional) Scale of Natural Earth data ("small", "medium", or
#'  "large"). Default is 'medium'.
#'
#' @return A ggplot object representing the biodiversity indicator map.
#' Can be customized using ggplot2 functions.
#'
#' @examples
#' evenness_map <- pielou_evenness_map(example_cube_1,
#'                                     level = "country",
#'                                     region = "Denmark")
#' plot_map(x = evenness_map,
#'          title = "Map of Species Evenness in Denmark",
#'          legend_title = "Evenness")
#'
#' @export
plot_map <- function(x,
                     title = "auto",
                     auto_title = NULL,
                     leg_label_default = NULL,
                     xlims = NULL,
                     ylims = NULL,
                     trans = NULL,
                     bcpower = NULL,
                     breaks = NULL,
                     labels = NULL,
                     crop_to_grid = FALSE,
                     panel_bg = NULL,
                     land_fill_colour = NULL,
                     legend_title = NULL,
                     legend_limits = NULL,
                     legend_title_wrap_length = 10,
                     title_wrap_length = 60,
                     visible_gridlines = TRUE,
                     layers = NULL,
                     scale = c("medium", "small", "large")
) {

  # Set variable definitions to NULL where required
  diversity_val <- geometry <- NULL

  # Match arguments
  scale <- match.arg(scale)

  # Get plot title (if set to "auto")
  title <- if (title == "auto") auto_title else title

  # Prepare data and layers for plotting
  map_data_list <- prepare_map_for_plot(
    x, xlims, ylims, layers, scale, crop_to_grid
  )

  # Unpack the list
  list2env(map_data_list, envir= environment())

  # Create plot using the helper function
  plot <- create_map_plot(
    data = x$data,
    map_surround = map_surround,
    layer_list = layer_list,
    land_fill_colour = land_fill_colour,
    panel_bg = panel_bg,
    trans = trans,
    bcpower = bcpower,
    breaks = breaks,
    labels = labels,
    legend_limits = legend_limits,
    map_level = x$map_level,
    visible_gridlines = visible_gridlines,
    crop_to_grid = crop_to_grid,
    map_lims = map_lims,
    projection = x$projection,
    leg_label_default = leg_label_default,
    legend_title = legend_title,
    legend_title_wrap_length = legend_title_wrap_length,
    suppress_legend = FALSE, # Always FALSE for plot_map
    title_label = wrapper(title, title_wrap_length),
    title_face = "plain"
  )

  # Exit function
  return(plot)

}


#' @title Plot Occurrence Map or Range Map of Individual Species
#'
#' @description Creates map visualizations of species ranges or species
#'  occurrences, providing customization options.
#'
#' @param x An 'indicator_map' object containing indicator values associated
#'  with map grid cells.
#' @param species Species you want to map occurrences for. Can be either
#'  numerical taxonKeys or species names. Partial species names can be used
#'  (the function will try to match them).
#' @param single_plot (Optional) By default all species occurrence time series
#'  will be combined into a single multi-panel plot. Set this to FALSE to plot
#'  each species separately.
#' @param title (Optional) Plot title. Replace "auto" with your own title if you
#'  want a custom title or if calling the function manually.
#' @param auto_title (Optional) Text for automatic title generation, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param leg_label_default (Optional) Default label for the legend, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param suppress_legend (Optional) Do not show legend. This should be set to
#'  true when plotting species ranges, as all cell values are 1.
#' @param xlims (Optional) Custom x-axis limits.
#' @param ylims (Optional) Custom y-axis limits.
#' @param trans (Optional) Scale transformation for the fill gradient
#'   (e.g., 'log').
#' @param bcpower (Optional) Power parameter for the Box-Cox, modulus, or
#'   Yeo-Johnson transformations.
#' @param breaks (Optional) Break points for the legend scale.
#' @param labels (Optional) Labels for legend scale break points.
#' @param crop_to_grid (Optional) If TRUE, the grid will determine the edges of
#'  the map. Default is FALSE.
#' @param panel_bg (Optional) Background colour for the map panel.
#' @param land_fill_colour (Optional) Colour for the land area outside of the
#'  grid (if surround = TRUE). Default is "grey85".
#' @param legend_title (Optional) Title for the plot legend.
#' @param legend_limits (Optional) Limits for the legend scale.
#' @param legend_title_wrap_length (Optional) Maximum legend title length
#'  before wrapping to a new line.
#' @param title_wrap_length (Optional) Maximum title length before wrapping to
#'  a new line.
#' @param spec_name_wrap_length (Optional) Maximum species name length before
#'  wrapping to a new line.
#' @param visible_gridlines (Optional) Show gridlines between cells.
#'  Default is TRUE.
#' @param layers (Optional) Additional rnaturalearth layers to plot, e.g.
#'  c("reefs", "playas").
#' @param scale (Optional) Scale of Natural Earth data ("small", "medium",
#'  or "large"). Default is 'medium'.
#'
#' @return A ggplot object representing the map of species range or occurrences.
#' Can be customized using ggplot2 functions.
#'
#' @examples
#' spec_occ_mammals_denmark <- spec_occ_map(example_cube_1,
#'                                     level = "country",
#'                                     region = "Denmark")
#' plot_species_map(x = spec_occ_mammals_denmark, c(2440728, 4265185))
#'
#' @export
plot_species_map <- function(x,
                             species,
                             title = "auto",
                             auto_title = NULL,
                             leg_label_default = NULL,
                             suppress_legend = FALSE,
                             xlims = NULL,
                             ylims = NULL,
                             trans = NULL,
                             bcpower = NULL,
                             breaks = NULL,
                             labels = NULL,
                             crop_to_grid = FALSE,
                             single_plot = TRUE,
                             panel_bg = NULL,
                             land_fill_colour = NULL,
                             legend_title = NULL,
                             legend_limits = NULL,
                             legend_title_wrap_length = 10,
                             title_wrap_length = 60,
                             spec_name_wrap_length = 40,
                             visible_gridlines = TRUE,
                             layers = NULL,
                             scale = c("medium", "small", "large")
                             ) {

  # Set variable definitions to NULL where required
  geometry <- diversity_val <- NULL

  # Match arguments
  scale <- match.arg(scale)

  # Get plot title (if set to "auto")
  title <- if (title == "auto") auto_title else title

  # Check that species argument has been provided
  if (is.null(species)) {
    stop(paste0(
      "Please enter either the species names or the numeric taxonKeys for ",
      "the species you want to plot."
    ))
  }

  # Split data by species
  split_so <- if (is.numeric(species)) {
    get_occurrences_and_split(x$data, "taxonKey", species)
  } else {
    get_occurrences_and_split(x$data, "scientificName", species)
  }

  # Get unique species names for plot labeling
  sci_names <- split_so %>% purrr::map(function(y) unique(y$scientificName))

   # Prepare data and layers for plotting
  map_data_list <- prepare_map_for_plot(
    x, xlims, ylims, layers, scale, crop_to_grid
  )

  # Unpack the list
  list2env(map_data_list, envir= environment())

  # Create the plots for each species using the helper function
  plot <- purrr::map(seq_along(sci_names), function(i) {
    create_map_plot(
      data = split_so[[i]],
      map_surround = map_surround,
      layer_list = layer_list,
      land_fill_colour = land_fill_colour,
      panel_bg = panel_bg,
      trans = trans,
      bcpower = bcpower,
      breaks = breaks,
      labels = labels,
      legend_limits = legend_limits,
      map_level = x$map_level,
      visible_gridlines = visible_gridlines,
      crop_to_grid = crop_to_grid,
      map_lims = map_lims,
      projection = x$projection,
      leg_label_default = leg_label_default,
      legend_title = legend_title,
      legend_title_wrap_length = legend_title_wrap_length,
      suppress_legend = suppress_legend,
      title_label = wrapper(sci_names[[i]], spec_name_wrap_length),
      title_face = "italic"
    )
  })

  # Name each plot with the corresponding species name
  names(plot) <- sci_names

  # Combine plots using wrap_plots function from patchwork
  if ((length(plot) > 0 && single_plot == TRUE) || length(plot) == 0) {
    plot <- patchwork::wrap_plots(plot) +
      plot_annotation_int(title = wrapper(title, title_wrap_length),
                          theme = theme(plot.title = element_text(size = 20)))
  # Or create each plot separately if single_plot is FALSE
  } else if (length(plot) > 1 && single_plot == FALSE) {
    message(paste0(
      "Option single_plot set to false. Creating separate plot for each ",
      "species.\n\n"
    ))
  }

  # Exit function
  return(plot)

}
