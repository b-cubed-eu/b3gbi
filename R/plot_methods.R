#' @title Plot Occurrences Segregated by Dataset
#'
#' @description  Creates a time series plot of total occurrences, with the
#'   occurrences visually segregated by their dataset of origin. Requires an
#'   'indicator_ts' object.
#'
#' @param x An object containing occurrence data segregated by dataset. Must
#'   be of class 'occ_by_dataset' and an 'indicator_ts' object.
#' @param x_breaks (Optional) Integer giving desired number of breaks for the x-axis.
#'   (May not return exactly the number requested.)
#' @param facet_scales Controls y-axis scaling across facets. Use "free_y" to
#'   allow independent scaling, or "fixed" for a common scale.
#' @param facet_rows (Optional) Number of rows of facets.
#' @param facet_cols (Optional) Number of columns of facets.
#' @param facet_label_width (Optional) Controls the maximum width of facet labels.
#' @param max_datasets (Optional) Maximum number of datasets to include in the plot.
#'   Datasets are selected based on having the highest number of occurrences.
#'   *Note that increasing this too much could result in high memory usage and/or
#'   make the plot very difficult to read.
#' @param min_occurrences (Optional)  Minimum total number of occurrences for a
#'   dataset to be included in the plot.
#' @param ... Additional arguments passed to the internal plotting function
#'   (`plot_ts_seg`). See its documentation for details.
#'
#' @return A ggplot object representing a time series plot with occurrences
#'    segregated by dataset.
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
                                ...){

  stopifnot_error("Incorrect object class. Must be class 'occ_by_dataset'.", inherits(x, "occ_by_dataset"))
  if (!inherits(x, "indicator_ts")) stop("Incorrect object class. Must be class 'indicator_ts'.")

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
  trend_plot <- plot_ts(x,
                        y_label_default = y_label_default,
                        auto_title = auto_title,
                        x_breaks = x_breaks,
                        ...)

  # Facet the plot according to dataset type
  trend_plot <- trend_plot +
    facet_wrap(vars(type),
               scales = facet_scales,
               nrow = facet_rows,
               ncol = facet_cols,
               labeller = label_wrap_gen(width = facet_label_width)) +
    theme(legend.position = "none")

  return(trend_plot)
}

#' @title Plot Occurrences Segregated by Type
#'
#' @description  Creates a time series plot of total occurrences, with the
#'   occurrences visually segregated by their type. Requires an 'indicator_ts'
#'   object.
#'
#' @param x An object containing  occurrence data segregated by type. Must
#'   be of class 'occ_by_type' and an 'indicator_ts' object.
#' @param x_breaks (Optional)  Integer giving desired number of breaks for the x-axis.
#'   (May not return exactly the number requested.)
#' @param facet_scales Controls y-axis scaling across facets.  Use "free_y" to
#'   allow independent scaling, or "fixed" for a common scale.
#' @param facet_rows (Optional) Number of rows of facets.
#' @param facet_cols (Optional) Number of columns of facets.
#' @param facet_label_width (Optional) Controls the maximum width of facet labels.
#' @param ... Additional arguments passed to the internal plotting function
#'   (`plot_ts_seg`). See its documentation for details.
#'
#' @return A ggplot object representing a time series plot with occurrences
#'    segregated by type.
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
                             ...){

  stopifnot_error("Incorrect object class. Must be class 'occ_by_type'.", inherits(x, "occ_by_type"))
  if (!inherits(x, "indicator_ts")) {stop("Incorrect object class. Must be class 'indicator_ts'.")}

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
  trend_plot <- plot_ts(x,
                        y_label_default = y_label_default,
                        auto_title = auto_title,
                        x_breaks = x_breaks,
                        ...)

  # Facet the plot according to type
  trend_plot <- trend_plot +
    facet_wrap(vars(type),
               scales = facet_scales,
               nrow = facet_rows,
               ncol = facet_cols,
               labeller = label_wrap_gen(width = facet_label_width)) +
    theme(legend.position = "none")

  return(trend_plot)
}

#' @export
plot.spec_range <- function(x, ...) {

  stopifnot_error("Incorrect object class. Must be class 'spec_range'.", inherits(x, "spec_range"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Cells Occupied"
    auto_title <- paste("Species Range Size", sep = "")

    # Call generalized plot_ts function
    plot_species_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    suppress_legend <- TRUE
    auto_title <- paste("Species Range", sep = "")

    # Call generalized plot_map function
    plot_species_map(x, suppress_legend = suppress_legend, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }

}

#' @export
plot.spec_occ <- function(x, ...) {

  stopifnot_error("Incorrect object class. Must be class 'spec_occ'.", inherits(x, "spec_occ") | inherits(x, "spec_range"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- paste("Species Occurrences", sep = "")

    # Call generalized plot_ts function
    plot_species_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Occurrences"
    auto_title <- paste("Species Occurrences", sep = "")

    # Call generalized plot_map function
    plot_species_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }

}


#' @export
plot.cum_richness <- function(x,
                              envelopecolour = NULL,
                              ...){

  stopifnot_error("Incorrect object class. Must be class 'cum_richness'.", inherits(x, "cum_richness"))

  if (!inherits(x, "indicator_ts")) {stop("Incorrect object class. Must be class 'indicator_ts'.")}

    # Set defaults
    y_label_default <- "Cumulative Species Richness"
    auto_title <- "Cumulative Species Richness"

    # Call generalized plot_map function
    trend_plot <- plot_ts(x,
                          y_label_default = y_label_default,
                          auto_title = auto_title,
                          smoothed_trend = FALSE,
                          ...)

    # if (is.null(envelopecolour)) envelopecolour = "lightsteelblue"
    #
    #
    # # Colour the area under the curve
    #  trend_plot <- trend_plot +
    #    geom_ribbon(aes(ymin = 0,
    #                    ymax = diversity_val),
    #                fill = envelopecolour, alpha = 0.3)
    # Show plot
    trend_plot

}


#' @export
plot.pielou_evenness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'pielou_evenness'.", inherits(x, "pielou_evenness"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Evenness"
    auto_title <- paste("Pielou's Evenness Trend", sep = "")

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Evenness"
  auto_title <- paste("Pielou's Evenness", sep = "")

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}


#' @export
plot.williams_evenness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'williams_evenness'.", inherits(x, "williams_evenness"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Evenness"
    auto_title <- paste("Williams' Evenness Trend", sep = "")

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Evenness"
    auto_title <- paste("Williams' Evenness", sep = "")

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}


#' @export
plot.tax_distinct <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'tax_distinct'.", inherits(x, "tax_distinct"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Taxonomic Distinctness"
    auto_title <- "Taxonomic Distinctness Trend"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Taxonomic Distinctness"
  auto_title <- "Taxonomic Distinctness"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}


#' @export
plot.occ_density <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'occ_density'.", inherits(x, "occ_density"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Mean Occurrences \nper km^2"
    auto_title <- "Trend of Mean Occurrence Density"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Occurrences \nper km^2"
    auto_title <- "Density of Occurrences"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @export
plot.newness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'newness'.", inherits(x, "newness"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Mean Year of Occurrence"
    auto_title <- "Trend of Mean Year of Occurrence"

    # Call generalized plot_map function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Mean Year of \nOccurrence"
    auto_title <- "Mean Year of Occurrence"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_map'.")

  }
}


#' @export
plot.total_occ <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'total_occ'.", inherits(x, "total_occ"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- "Trend of Total Occurrences"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Occurrences"
  auto_title <- "Total Occurrences"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @export
plot.area_rarity <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'area_rarity'.", inherits(x, "area_rarity"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Mean of Rarity (Summed by Cell)"
    auto_title <- "Area-Based Rarity Trend"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Summed Rarity"
  auto_title <- "Area-Based Rarity"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @export
plot.ab_rarity <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'ab_rarity'.", inherits(x, "ab_rarity"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Mean of Rarity (Summed by Cell)"
    auto_title <- "Abundance-Based Rarity Trend"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Summed Rarity"
  auto_title <- "Abundance-Based Rarity"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @noRd
plot.rarefied <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'rarefied'.", inherits(x, "rarefied"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness Index"
    auto_title <- "Indexed Species Richness Trend (Estimated by Sample Size-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

  # Set defaults
  leg_label_default <- "Richness"
  auto_title <- "Species Richness (Estimated by Sample Size-Based Rarefaction)"

  # Call generalized plot_map function
  plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @export
plot.hill2 <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'hill2'.", inherits(x, "hill2"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Hill-Simpson Diversity"
    auto_title <- "Hill-Simpson Diversity Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Hill-Simpson Diversity"
    auto_title <- "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @export
plot.hill1 <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'hill1'.", inherits(x, "hill1"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Hill-Shannon Diversity"
    auto_title <- "Hill-Shannon Diversity Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Hill-Shannon Diversity"
    auto_title <- "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @export
plot.hill0 <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'hill0'.", inherits(x, "hill0"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness"
    auto_title <- "Species Richness Trend (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Richness"
    auto_title <- "Species Richness (Estimated by Coverage-Based Rarefaction)"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @export
plot.obs_richness <- function(x, ...){

  stopifnot_error("Incorrect object class. Must be class 'obs_richness'.", inherits(x, "obs_richness"))

  if (inherits(x, "indicator_ts")) {

    # Set defaults
    y_label_default <- "Species Richness"
    auto_title <- "Observed Species Richness Trend"

    # Call generalized plot_ts function
    plot_ts(x, y_label_default = y_label_default, auto_title = auto_title, ...)

  } else if (inherits(x, "indicator_map")) {

    # Set defaults
    leg_label_default <- "Richness"
    auto_title <- "Observed Species Richness"

    # Call generalized plot_map function
    plot_map(x, leg_label_default = leg_label_default, auto_title = auto_title, ...)

  } else {

    stop("Incorrect object class. Must be class 'indicator_ts' or 'indicator_map'.")

  }
}

#' @export
plot.occ_turnover <- function(x,
                              auccolour = NULL,
                              ...){

  stopifnot_error("Incorrect object class. Must be class 'occ_turnover'.", inherits(x, "occ_turnover"))

  if (!inherits(x, "indicator_ts")) {stop("Incorrect object class. Must be class 'indicator_ts'.")}

  # Set defaults
  y_label_default <- "Occupancy Turnover"
  auto_title <- "Occupancy Turnover"

  # Call generalized plot_map function
  trend_plot <- plot_ts(x,
                        y_label_default = y_label_default,
                        auto_title = auto_title,
                        ...)

  # if (is.null(auccolour)) auccolour = "orange"
  #
  # # Colour the area under the curve
  # trend_plot <- trend_plot +
  #   geom_ribbon(aes(ymin = 0,
  #                   ymax = diversity_val),
  #               fill = auccolour, alpha = 0.4)

  # Show plot
  trend_plot

}

#' @title Plot Biodiversity Indicator Map
#'
#' @description Creates a map visualization of a calculated biodiversity indicator,
#'   providing customization options.
#'
#' @param x An 'indicator_map' object containing indicator values associated with
#'   map grid cells.
#' @param title Plot title. Replace "auto" with your own title if you want a
#'   custom title or if calling the function manually.
#' @param auto_title Text for automatic title generation, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param leg_label_default Default label for the legend, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param xlims  (Optional) Custom x-axis limits.
#' @param ylims (Optional) Custom y-axis limits.
#' @param trans (Optional) Scale transformation for the fill gradient
#'   (e.g., 'log').
#' @param bcpower (Optional) Power parameter for the Box-Cox, modulus, or
#'   Yeo-Johnson transformations.
#' @param breaks (Optional) Break points for the legend scale.
#' @param labels (Optional) Labels for legend scale break points.
#' @param crop_to_grid If TRUE, the grid will determine the edges of the map.Overrides
#'    Europe_crop_EEA. Default is FALSE.
#' @param panel_bg (Optional) Background colour for the map panel.
#' @param land_fill_colour (Optional) Colour for the land area outside of the grid
#'    (if surround = TRUE). Default is "grey85".
#' @param legend_title (Optional) Title for the plot legend.
#' @param legend_limits (Optional) Limits for the legend scale.
#' @param legend_title_wrap_length Maximum legend title length before wrapping to a new line.
#' @param title_wrap_length Maximum title length before wrapping to a new line.
#' @param visible_gridlines Show gridlines between cells. Default is TRUE.
#' @param layers Additional rnaturalearth layers to plot, e.g. c("reefs", "playas").
#' @param scale Scale of Natural Earth data ("small", "medium", or "large"). Default is 'medium'.
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
                     scale = "medium"
) {

  # Set variable definitions to NULL where required
  diversity_val <- geometry <- scalerank <- featurecla <- NULL

  # Check that object to plot is the correct class
  if (!inherits(x, "indicator_map")) {
    stop("Incorrect object class. Must be class 'indicator_map'.")
  }

  # Add default land layer to layers
  layers <- c("admin_0_countries", layers)

  # Get map limits
  map_lims <- x$coord_range

  # Override horizontal axis limits if custom limits provided by user
  if (!is.null(xlims)) {
    if (is.vector(xlims) && length(xlims)==2) {
      map_lims["xmin"] <- xlims[1]
      map_lims["xmax"] <- xlims[2]
    } else {
      stop("Please provide numeric xlims values in the form of c(1,2)")
    }
  }

  # Override vertical axis limits if custom limits provided by user
  if (!is.null(ylims)) {
    if (is.vector(ylims) && length(ylims)==2) {
      map_lims["ymin"] <- ylims[1]
      map_lims["ymax"] <- ylims[2]
    } else {
      stop("Please provide numeric ylims values in the form of c(1,2)")
    }
  }

  # Get world data to plot surrounding land
  map_data_sf <- rnaturalearth::ne_countries(scale = scale,
                                             returnclass = "sf") %>%
    sf::st_as_sf()
  map_surround <- map_data_sf %>%
    sf::st_transform(crs = x$projection) %>%
    sf::st_make_valid()

  # Get plot title (if set to "auto")
  title <- if (title == "auto") auto_title else title

  # Set up transformation if specified
  if (!is.null(trans)) {
    if (trans == "boxcox") {
      trans <- scales::transform_boxcox(p = bcpower)
    } else if (trans == "modulus") {
      trans <- scales::transform_modulus(p = bcpower)
    } else if (trans == "yj") {
      trans <- scales::transform_yj(p = bcpower)
    }
  }

  # Define function to modify legend
  cust_leg <- function(scale.params = list()) {
    do.call("scale_fill_gradient", modifyList(
      list(low = "gold", high = "firebrick4", na.value = "transparent"),
      scale.params)
    )
  }

  # Set default value for land_fill_colour if NULL
  land_fill_colour <- dplyr::coalesce(land_fill_colour, "grey85")

  # Prepare map data with correct layers, CRS, bounding box, etc.
  map_data_list <- prepare_map_data(data = x$data,
                                    projection = x$projection,
                                    map_lims = map_lims,
                                    xlims = xlims,
                                    ylims = ylims,
                                    map_data_sf = map_data_sf,
                                    layers = layers,
                                    scale = scale,
                                    crop_to_grid = crop_to_grid
  )

  # Get map data and layer list
  map_surround <- map_data_list$map_surround
  layer_list <- map_data_list$layer_list

  # Create plot in steps using ggplot
  ###################################

  # Step 1: Create blank plot to fill
  diversity_plot <- ggplot2::ggplot(x$data)

  # Step 2: Add the land data
  diversity_plot <- diversity_plot +
    ggplot2::geom_sf(
      data = map_surround,
      fill = land_fill_colour,
      colour = "black",
      aes(geometry = geometry),
      inherit.aes = FALSE
    )

  # Step 3: Add additional layers, with ocean and lakes in blue
  for (i in names(layer_list)) {
    layer_data <- layer_list[[i]]
    if (i %in% c("ocean", "lakes")) {
      fill_colour <- "#92c5f0"
    } else {
      fill_colour <- "transparent"
    }
    diversity_plot <- diversity_plot +
      ggplot2::geom_sf(data = layer_data,
                       aes(geometry = geometry),
                       fill = fill_colour,
                       colour = "black",
                       inherit.aes = FALSE)
  }

  # Step 4: Add indicator values
  diversity_plot <- diversity_plot +
    geom_sf(aes(fill = diversity_val,
                geometry = geometry),
            colour = "transparent") +
    cust_leg(list(trans = trans,
                  breaks = breaks,
                  labels = labels,
                  limits = legend_limits)) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = dplyr::coalesce(panel_bg,
                                                             "#92c5f0")),
      if(x$map_level == "country") {
        panel.grid.major = element_blank()
        panel.grid.minor = element_blank()
      }
    ) +
    # Wrap legend title if longer than user-specified wrap length
    labs(fill = if(!is.null(legend_title)) wrapper(legend_title,
                                                   legend_title_wrap_length)
         else wrapper(leg_label_default,
                      legend_title_wrap_length))

  # Step 5: Re-create lines from layers
  # This ensures that indicator values are plotted on top of layer fill (e.g.,
  # oceans and lakes) but lines are still visible.
  for (i in names(layer_list)) {
    layer_data <- layer_list[[i]]
    diversity_plot <- diversity_plot +
      ggplot2::geom_sf(data = layer_data,
                       aes(geometry = geometry),
                       fill = "transparent",
                       colour = "black",
                       inherit.aes = FALSE)
  }

  # Step 6: Add gridlines between cells if not set to false
  if (visible_gridlines == TRUE) {
    # plot gridlines
    diversity_plot$layers <- c(
      diversity_plot$layers,
      ggplot2::geom_sf(aes(geometry = geometry),
                       colour = "black",
                       linewidth = 0.1,
                       fill = "transparent"
      )[[1]]
    )
  }

  # Step 7: Expand the plot if crop_to_grid is not set
  expand_val <- if (crop_to_grid) FALSE else TRUE
  diversity_plot <- diversity_plot +
    coord_sf(
      crs = x$projection,
      xlim = c(map_lims["xmin"],
               map_lims["xmax"]),
      ylim = c(map_lims["ymin"],
               map_lims["ymax"]),
      expand = expand_val)

  # Step 8: Wrap title if longer than user-specified wrap length
  if(!is.null(title)) {
    diversity_plot <-
      diversity_plot +
      labs(title = wrapper(title, title_wrap_length))
  }

  # Exit function
  return(diversity_plot)
}



#' @title Plot Biodiversity Indicator Trend
#'
#' @description  Creates a time series plot of a calculated biodiversity
#'   indicator, with an optional smoothed trendline, and visualizes uncertainty.
#'
#' @param x An 'indicator_ts' object containing a time series of indicator values.
#' @param min_year (Optional)  Earliest year to include in the plot.
#' @param max_year (Optional)  Latest year to include in the plot.
#' @param title Plot title. Replace "auto" with your own title if you want a
#'   custom title or if calling the function manually.
#' @param auto_title Text for automatic title generation, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param y_label_default Default label for the y-axis, provided by an appropriate
#'   S3 method (if calling the function manually, leave as NULL).
#' @param suppress_y If TRUE, suppresses y-axis labels.
#' @param smoothed_trend If TRUE, plot a smoothed trendline over time
#' (`stats::loess()`).
#' @param linecolour (Optional) Colour for the indicator line or points.
#'   Default is darkorange.
#' @param linealpha Transparency for indicator line or points. Default is 0.8.
#' @param ribboncolour (Optional) Colour for the bootstrapped confidence intervals.
#'   Default is goldenrod1. Set to "NA" if you don't want to plot the CIs.
#' @param ribbonalpha Transparency for indicator confidence interval ribbon (if
#'   ci_type = "ribbon"). Default is 0.2.
#' @param error_alpha Transparency for indicator error bars (if ci_type = "error_bar").
#'   Default is 1.
#' @param trendlinecolour (Optional) Colour for the smoothed trendline.
#'   Default is blue.
#' @param trendlinealpha Transparency for the smoothed trendline. Default is 0.5.
#' @param envelopecolour (Optional) Colour for the uncertainty envelope.
#'   Default is lightsteelblue.
#' @param envelopealpha Transparency for the smoothed trendline envelope. Default is 0.2.
#' @param smooth_cialpha Transparency for the smoothed lines forming the edges of the
#'   trendline envelope. Default is 1.
#' @param point_line Whether to plot the indicator as a line or a series of points.
#'   Options are "line" or "point". Default is "point".
#' @param pointsize Size of the points if point_line = "point". Default is 2.
#' @param linewidth Width of the line if point_line = "line". Default is 1.
#' @param ci_type Whether to plot bootstrapped confidence intervals as a "ribbon"
#'   or "error_bars". Default is "error_bars".
#' @param error_width Width of error bars if ci_type = "error_bars". Default is 1.
#'   Note that unlike the default 'width' parameter in geom_errorbar, 'error_width' is NOT
#'   dependent on the number of data points in the plot. It is automatically scaled to
#'   account for this. Therefore the width you select will be consistent relative to the
#'   plot width even if you change 'min_year' and 'max_year'.
#' @param error_thickness Thickness of error bars if ci_type = "error_bars". Default is 1.
#' @param smooth_linetype Type of line to plot for smoothed trendline. Default is "solid".
#' @param smooth_linewidth Line width for smoothed trendline. Default is 1.
#' @param smooth_cilinewidth Line width for smoothed trendline confidence intervals.
#'   Default is 1.
#' @param gridoff  If TRUE, hides gridlines.
#' @param x_label Label for the x-axis.
#' @param y_label Label for the y-axis.
#' @param x_expand (Optional)  Expansion factor to expand the x-axis beyond the data.
#'   Left and right values are required in the form of c(0.1, 0.2) or simply 0.1 to
#'   apply the same value to each side. Default is 0.05.
#' @param y_expand (Optional)  Expansion factor to expand the y-axis beyond the data.
#'   Lower and upper values are required in the form of c(0.1, 0.2) or simply 0.1 to
#'   apply the same value to the top and bottom. Default is 0.05.
#' @param x_breaks Integer giving desired number of breaks for x axis.
#'   (May not return exactly the number requested.)
#' @param y_breaks Integer giving desired number of breaks for y axis.
#'   (May not return exactly the number requested.)
#' @param wrap_length  Maximum title length before wrapping to a new line.
#'
#' @return A ggplot object representing the biodiversity indicator time series plot.
#' Can be customized using ggplot2 functions.
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

  year <- diversity_val <- ul <- ll <- NULL

  point_line <- match.arg(point_line)
  ci_type <- match.arg(ci_type)
  smooth_linetype <- match.arg(smooth_linetype)

  if (!inherits(x, "indicator_ts")) {
    stop("Incorrect object class. Must be class 'indicator_ts'.")
  }

  # Filter by min and max year if set
  if (!is.null(min_year) || !is.null(max_year)) {
    # Set min and max year
    min_year <- ifelse(is.null(min_year), x$first_year, min_year)
    max_year <- ifelse(is.null(max_year), x$last_year, max_year)
    x$data <-
      x$data %>%
      dplyr::filter(year >= min_year) %>%
      dplyr::filter(year <= max_year)

    if (length(x$data$year) < 1) {
      stop("No data available for the selected years. Please check your input.")
    }
  } else {
    min_year <- x$first_year
    max_year <- x$last_year
  }

  if ((max_year - min_year) < 2 && smoothed_trend == TRUE) {
    smoothed_trend <- FALSE
    message("Could not perform loess smooth due to insufficient time points.")
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
  if (is.null(ribboncolour)) ribboncolour = "goldenrod1"
  if (is.null(trendlinecolour)) trendlinecolour = "blue"
  if (is.null(envelopecolour)) envelopecolour = "lightsteelblue1"

  # Set axis titles
  if (is.null(x_label)) x_label = "Year"
  if (is.null(y_label)) y_label = y_label_default

  # Set axis limits
  if (is.null(x_expand)) x_expand = c(0, 0)
  if (is.null(y_expand)) y_expand = c(0, 0)

  # Adjust error bar width according to number of years being plotted
  error_width = (error_width * (max_year - min_year)) / 100

  if ("ll" %in% colnames(x$data) & "ul" %in% colnames(x$data)) {

    # Remove NAs from CIs
    x$data$ll <- ifelse(is.na(x$data$ll), x$data$diversity_val, x$data$ll)
    x$data$ul <- ifelse(is.na(x$data$ul), x$data$diversity_val, x$data$ul)

  }

  # Convert years to factor
 # x$data$year <- as.factor(x$data$year)

  # Create basis of plot
  trend_plot <-
    ggplot2::ggplot(x$data, aes(x = year,
                                y = diversity_val))

  # Add smooth trends (LOESS) if specified
  if (smoothed_trend == TRUE) {
    # Add a smoothed trend
    trend_plot <- trend_plot +
      geom_smooth(
        colour = alpha(trendlinecolour, trendlinealpha),
        lwd = smooth_linewidth,
        linetype = smooth_linetype,
        method = "loess",
        formula = "y ~ x",
        se = FALSE)

    # Add smooth trends for confidence limits if available
    if ("ll" %in% colnames(x$data) && "ul" %in% colnames(x$data)) {
      trend_plot <- trend_plot +
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
  if ("ll" %in% colnames(x$data) && "ul" %in% colnames(x$data)) {
    if (ci_type == "error_bars") {
      trend_plot <- trend_plot +
        geom_errorbar(aes(ymin = ll, ymax = ul),
                      colour = ribboncolour,
                      alpha = error_alpha,
                      width = error_width,
                      linewidth = error_thickness)
    } else {
      trend_plot <- trend_plot +
        geom_ribbon(aes(ymin = ll, ymax = ul),
                    alpha = ribbonalpha,
                    fill = ribboncolour)
    }

  }

  if (point_line == "point") {
    trend_plot <- trend_plot +
      geom_point(colour = linecolour,
                 alpha = linealpha,
                 size = pointsize)
  } else {
    trend_plot <- trend_plot +
      geom_line(aes(group = 1),
                colour = linecolour,
                alpha = linealpha,
                lwd = linewidth)
  }

  trend_plot <- trend_plot +
    scale_x_continuous(breaks = breaks_pretty_int(n = x_breaks),
                       expand = expansion(mult = x_expand)) +
    scale_y_continuous(breaks = breaks_pretty_int(n = y_breaks),
                       expand = expansion(mult = y_expand)) +
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
          strip.text = element_text(face = "italic"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
    )

  # Wrap title if longer than wrap_length
  if(!is.null(title)) {
    # wrapper <- function(x, ...)
    # {
    #   paste(strwrap(x, ...), collapse = "\n")
    # }
    trend_plot <-
      trend_plot +
      labs(title = wrapper(title, wrap_length))
  }

  # Show plot
  trend_plot

}

#' @title Plot Occurrence Trends or Range Size Trends for Individual Species
#'
#' @description  Creates time series plots of species occurrences or species range
#'   sizes, with an optional smoothed trendline, and visualizes uncertainty.
#'
#' @param x An 'indicator_ts' object containing time series of indicator values
#'   matched to species names and/or taxon keys.
#' @param species Species you want to map occurrences for. Can be either numerical
#'   taxonKeys or species names. Partial species names can be used (the function
#'   will try to match them).
#' @param single_plot By default all species occurrence time series will be combined
#'   into a single multi-panel plot. Set this to FALSE to plot each species separately.
#' @param min_year (Optional)  Earliest year to include in the plot.
#' @param max_year (Optional)  Latest year to include in the plot.
#' @param title Plot title. Replace "auto" with your own title if you want a
#'   custom title or if calling the function manually.
#' @param auto_title Text for automatic title generation, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param y_label_default Default label for the y-axis, provided by an appropriate
#'   S3 method (if calling the function manually, leave as NULL).
#' @param suppress_y If TRUE, suppresses y-axis labels.
#' @param smoothed_trend If TRUE, plot a smoothed trendline over time
#' (`stats::loess()`).
#' @param linecolour (Optional) Colour for the indicator line or points.
#'   Default is darkorange.
#' @param linealpha Transparency for indicator line or points. Default is 0.8.
#' @param ribboncolour (Optional) Colour for the bootstrapped confidence intervals.
#'   Default is goldenrod1. Set to "NA" if you don't want to plot the CIs.
#' @param ribbonalpha Transparency for indicator confidence interval ribbon (if
#'   ci_type = "ribbon"). Default is 0.2.
#' @param error_alpha Transparency for indicator error bars (if ci_type = "error_bar").
#'   Default is 1.
#' @param trendlinecolour (Optional) Colour for the smoothed trendline.
#'   Default is blue.
#' @param trendlinealpha Transparency for the smoothed trendline. Default is 0.5.
#' @param envelopecolour (Optional) Colour for the uncertainty envelope.
#'   Default is lightsteelblue.
#' @param envelopealpha Transparency for the smoothed trendline envelope. Default is 0.2.
#' @param smooth_cialpha Transparency for the smoothed lines forming the edges of the
#'   trendline envelope. Default is 1.
#' @param point_line Whether to plot the indicator as a line or a series of points.
#'   Options are "line" or "point". Default is "point".
#' @param pointsize Size of the points if point_line = "point". Default is 2.
#' @param linewidth Width of the line if point_line = "line". Default is 1.
#' @param ci_type Whether to plot bootstrapped confidence intervals as a "ribbon"
#'   or "error_bars". Default is "error_bars".
#' @param error_width Width of error bars if ci_type = "error_bars". Default is 1.
#'   Note that unlike the default 'width' parameter in geom_errorbar, 'error_width' is NOT
#'   dependent on the number of data points in the plot. It is automatically scaled to
#'   account for this. Therefore the width you select will be consistent relative to the
#'   plot width even if you change 'min_year' and 'max_year'.
#' @param error_thickness Thickness of error bars if ci_type = "error_bars". Default is 1.
#' @param smooth_linetype Type of line to plot for smoothed trendline. Default is "solid".
#' @param smooth_linewidth Line width for smoothed trendline. Default is 1.
#' @param smooth_cilinewidth Line width for smoothed trendline confidence intervals.
#'   Default is 1.
#' @param gridoff  If TRUE, hides gridlines.
#' @param x_label Label for the x-axis.
#' @param y_label Label for the y-axis.
#' @param x_expand (Optional)  Expansion factor to expand the x-axis beyond the data.
#'   Left and right values are required in the form of c(0.1, 0.2) or simply 0.1 to
#'   apply the same value to both sides. Default is 0.05.
#' @param y_expand (Optional)  Expansion factor to expand the y-axis beyond the data.
#'   Lower and upper values are required in the form of c(0.1, 0.2) or simply 0.1 to
#'   apply the same value to the top and bottom. Default is 0.05.
#' @param x_breaks Integer giving desired number of breaks for x axis.
#'   (May not return exactly the number requested.)
#' @param y_breaks Integer giving desired number of breaks for y axis.
#'   (May not return exactly the number requested.)
#' @param wrap_length  Maximum title length before wrapping to a new line.
#'
#' @return A ggplot object representing species range or occurrence time series plot(s).
#'   Can be customized using ggplot2 functions.
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
                            species = NULL,
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

  year <- taxonKey <- . <- scientificName <- ll <- ul <- diversity_val <- NULL

  point_line <- match.arg(point_line)
  ci_type <- match.arg(ci_type)
  smooth_linetype <- match.arg(smooth_linetype)


  if (!inherits(x, "indicator_ts")) {
    stop("Incorrect object class. Must be class 'indicator_ts'.")
  }

  # Filter by min and max year if set
  if (!is.null(min_year) || !is.null(max_year)) {
      # Set min and max year
      min_year <- ifelse(is.null(min_year), x$first_year, min_year)
      max_year <- ifelse(is.null(max_year), x$last_year, max_year)
      x$data <-
        x$data %>%
        dplyr::filter(year >= min_year) %>%
        dplyr::filter(year <= max_year)

      if (length(x$data$year) < 1) {
        stop("No data available for the selected years. Please check your input.")
      }
  } else {
    min_year <- x$first_year
    max_year <- x$last_year
  }


  if (is.null(species)) {

    stop("Please enter either the species names or the numeric taxonKeys for the species you want to plot.")

  } else if (is.numeric(species)) {

    # Get occurrences for selected species
    species_occurrences <-
      x$data %>%
      dplyr::filter(taxonKey %in% species) %>%
      {if(nrow(.) < 1)
        stop("No matching taxonKeys. Please check that you have entered them correctly.")
        else (.)
      } %>%
      dplyr::mutate(taxonKey = factor(taxonKey,
                                      levels = unique(taxonKey)))

    split_so <-
      species_occurrences %>%
      dplyr::group_split(taxonKey)

  } else {

    # Get occurrences for selected species
    species_occurrences <-
      x$data %>%
      dplyr::filter(grepl(paste("^", species, collapse="|",sep=""), scientificName)) %>%
      { if(nrow(.) < 1)
        stop("No matching species. Please check that you have entered the names correctly.")
        else (.) } %>%
      dplyr::arrange(scientificName, grepl(paste("^", species, collapse="|",sep=""), scientificName))

    split_so <-
      species_occurrences %>%
      dplyr::group_split(scientificName)

  }

  sci_names <-
    split_so %>%
    purrr::map(~unique(.$scientificName))

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
  if (is.null(ribboncolour)) ribboncolour = "goldenrod1"
  if (is.null(trendlinecolour)) trendlinecolour = "blue"
  if (is.null(envelopecolour)) envelopecolour = "lightsteelblue1"

  # Set axis titles
  if (is.null(x_label)) x_label = "Year"
  if (is.null(y_label)) y_label = y_label_default

  # Set axis limits
  if (is.null(x_expand)) x_expand = c(0, 0)
  if (is.null(y_expand)) y_expand = c(0, 0)

  # Adjust error bar width according to number of years being plotted
  error_width = (error_width * (max_year - min_year)) / 100

  # Create bootstrapped confidence intervals if columns present
  if ("ll" %in% colnames(x$data) & "ul" %in% colnames(x$data)) {

    # Remove NAs from CIs
    x$data$ll <- ifelse(is.na(x$data$ll), x$data$diversity_val, x$data$ll)
    x$data$ul <- ifelse(is.na(x$data$ul), x$data$diversity_val, x$data$ul)

      if (ci_type == "error_bars") {
        ci_ribbon <- list(
          geom_errorbar(aes(ymin = ll, ymax = ul),
                        colour = ribboncolour,
                        alpha = error_alpha,
                        width = error_width,
                        linewidth = error_thickness)
        )
      } else {
        ci_ribbon <- list(
          geom_ribbon(aes(ymin = ll, ymax = ul),
                      alpha = ribbonalpha,
                      fill = ribboncolour)
        )
      }

  } else {
    ci_ribbon <- list()
  }

  # Create smoothed trend if desired
  if (smoothed_trend == TRUE) {

    # Include smooth trends for confidence limits if available
    if ("ll" %in% colnames(x$data) & "ul" %in% colnames(x$data)) {
      smoothing <- list(
         geom_smooth(
           colour = alpha(trendlinecolour, trendlinealpha),
           lwd = smooth_linewidth,
           linetype = smooth_linetype,
           method = "loess",
           formula = "y ~ x",
           se = FALSE),

         geom_ribbon(aes(ymin = predict(loess(ll ~ year)),
                         ymax = predict(loess(ul ~ year))),
                     alpha = envelopealpha,
                     fill = envelopecolour),

          geom_smooth(
            aes(y = ul),
            colour = alpha(envelopecolour, smooth_cialpha),
            lwd = smooth_cilinewidth,
            linetype = "dashed",
            method = "loess",
            formula = "y ~ x",
            se = FALSE),

          geom_smooth(
            aes(y = ll),
            colour = alpha(envelopecolour, smooth_cialpha),
            lwd = smooth_cilinewidth,
            linetype = "dashed",
            method = "loess",
            formula = "y ~ x",
            se = FALSE)
      )
    } else {

        smoothing <- list(
          geom_smooth(
            colour = alpha(trendlinecolour, 0.5),
            lwd = 1,
            linetype = smooth_linetype,
            method = "loess",
            formula = "y ~ x",
            se = FALSE)
        )
    }

  } else {
    smoothing <- list()
  }

  # Create plot with trend line
  trend_plot <-
    purrr::map2(split_so,
                sci_names,
                function(x., y) {

                  if (point_line == "point") {
                    specplot <- ggplot2::ggplot(x., aes(x = year,
                                                        y = diversity_val)) +
                      geom_point(colour = linecolour,
                                 size = pointsize,
                                 alpha = linealpha)
                  } else {
                    specplot <- ggplot2::ggplot(x., aes(x = year,
                                                        y = diversity_val)) +
                      geom_line(colour = linecolour,
                                lwd = linewidth,
                                alpha = linealpha)
                  }

                  specplot <- specplot +
                    scale_x_continuous(breaks = breaks_pretty_int(n = x_breaks),
                                       expand = expansion(mult = x_expand)) +
                    scale_y_continuous(breaks = breaks_pretty_int(n = y_breaks),
                                       expand = expansion(mult = y_expand)) +
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
                    ) +
                    labs(title = y) +
                    #theme_elements +
                    smoothing +
                    ci_ribbon

                  return(specplot)
                })

  names(trend_plot) <- sci_names

  # Combine plots using wrap_plots function from patchwork
  if ((length(trend_plot) > 0 & single_plot == TRUE) ||
       length(trend_plot) == 1) {
    trend_plot <- patchwork::wrap_plots(trend_plot) +
      plot_annotation_int(title = title,
                          theme = theme(plot.title = element_text(size = 20)))
  } else if (length(trend_plot) > 1 & single_plot == FALSE) {
    cat("Option single_plot set to false. Creating separate plot for each species.\n\n")
  }

  return(trend_plot)

}


#' @title Plot Occurrence Map or Range Map of Individual Species
#'
#' @description Creates map visualizations of species ranges or species occurrences,
#'   providing customization options.
#'
#' @param x An 'indicator_map' object containing indicator values associated with
#'   map grid cells.
#' @param species Species you want to map occurrences for. Can be either numerical
#'   taxonKeys or species names. Partial species names can be used (the function
#'   will try to match them).
#' @param title Plot title. Replace "auto" with your own title if you want a
#'   custom title or if calling the function manually.
#' @param auto_title Text for automatic title generation, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param leg_label_default Default label for the legend, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param suppress_legend Do not show legend. This should be set to true when
#'   plotting species ranges, as all cell values are 1.
#' @param xlims  (Optional) Custom x-axis limits.
#' @param ylims (Optional) Custom y-axis limits.
#' @param trans (Optional) Scale transformation for the fill gradient
#'   (e.g., 'log').
#' @param bcpower (Optional) Power parameter for the Box-Cox, modulus, or
#'   Yeo-Johnson transformations.
#' @param breaks (Optional) Break points for the legend scale.
#' @param labels (Optional) Labels for legend scale break points.
#' @param crop_to_grid If TRUE, the grid will determine the edges of the map.Overrides
#'    Europe_crop_EEA. Default is FALSE.
#' @param single_plot By default all species occurrence time series will be combined
#'    into a single multi-panel plot. Set this to FALSE to plot each species separately.
#' @param panel_bg  (Optional) Background colour for the map panel.
#' @param land_fill_colour (Optional) Colour for the land area outside of the grid
#'    (if surround = TRUE). Default is "grey85".
#' @param legend_title (Optional) Title for the plot legend.
#' @param legend_limits (Optional) Limits for the legend scale.
#' @param legend_title_wrap_length Maximum legend title length before wrapping to a new line.
#' @param title_wrap_length Maximum title length before wrapping to a new line.
#' @param spec_name_wrap_length Maximum species name length before wrapping to a new line.
#' @param visible_gridlines Show gridlines between cells. Default is TRUE.
#' @param layers Additional rnaturalearth layers to plot, e.g. c("reefs", "playas").
#' @param scale Scale of Natural Earth data ("small", "medium", or "large"). Default is 'medium'.
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
                             species = NULL,
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
                             scale = "medium"
) {

  taxonKey <- . <- scientificName <- geometry <- diversity_val <- NULL


  if (!inherits(x, "indicator_map")) {
    stop("Incorrect object class. Must be class 'indicator_map'.")
  }

  if (is.null(species)) {

    stop("Please enter either the species names or the numeric taxonKeys for the species you want to plot.")

  } else if (is.numeric(species)) {

    # Get occurrences for selected species
    species_occurrences <-
      x$data %>%
      dplyr::filter(taxonKey %in% species) %>%
      {if(nrow(.) < 1)
        stop("No matching taxonKeys. Please check that you have entered them correctly.")
        else (.)
      } %>%
      dplyr::mutate(taxonKey = factor(taxonKey,
                                      levels = unique(taxonKey)))

    split_so <-
      species_occurrences %>%
      dplyr::group_split(taxonKey)

  } else {

    # Get occurrences for selected species
    species_occurrences <-
      x$data %>%
      dplyr::filter(grepl(paste("^", species, collapse="|",sep=""), scientificName)) %>%
      { if(nrow(.) < 1)
        stop("No matching species. Please check that you have entered the names correctly.")
        else (.) } %>%
      dplyr::arrange(scientificName, grepl(paste("^", species, collapse="|",sep=""), scientificName))

    split_so <-
      species_occurrences %>%
      dplyr::group_split(scientificName)

  }

  sci_names <-
    split_so %>%
    purrr::map(~unique(.$scientificName))

  # Add country borders to user supplied layers, as they are mandatory
  layers <- c("admin_0_countries", layers)

  # Get map limits
  map_lims <- x$coord_range

  if (!is.null(xlims)) {
    if (is.vector(xlims) && length(xlims)==2) {
      map_lims["xmin"] <- xlims[1]
      map_lims["xmax"] <- xlims[2]
    } else {
      stop("Please provide numeric xlims values in the form of c(1,2)")
    }
  }

  if (!is.null(ylims)) {
    if (is.vector(ylims) && length(ylims)==2) {
      map_lims["ymin"] <- ylims[1]
      map_lims["ymax"] <- ylims[2]
    } else {
      stop("Please provide numeric ylims values in the form of c(1,2)")
    }
  }

  # Get world data to plot surrounding land if surround flag is set
  map_data_sf <- rnaturalearth::ne_countries(scale = "medium",
                                             returnclass = "sf") %>%
    sf::st_as_sf()

  # Get plot title (if set to "auto")
  title <- if (title == "auto") auto_title else title

  if (!is.null(trans)) {
    if (trans == "boxcox") {
      trans <- scales::transform_boxcox(p = bcpower)
    } else if (trans == "modulus") {
      trans <- scales::transform_modulus(p = bcpower)
    } else if (trans == "yj") {
      trans <- scales::transform_yj(p = bcpower)
    }
  }

  # Define function to modify legend
  cust_leg <- function(scale.params = list()) {
    do.call("scale_fill_gradient", modifyList(
      list(low = "gold", high = "firebrick4", na.value = "grey95"),
      scale.params)
    )
  }

  # Set default value for land_fill_colour if NULL
  land_fill_colour <- dplyr::coalesce(land_fill_colour, "grey85")

  map_data_list <- prepare_map_data(data = x$data,
                                   projection = x$projection,
                                   map_lims = map_lims,
                                   xlims = xlims,
                                   ylims = ylims,
                                   map_data_sf = map_data_sf,
                                   layers = layers,
                                   scale = scale,
                                   crop_to_grid = crop_to_grid)

  map_surround <- map_data_list$map_surround
  layer_list <- map_data_list$layer_list

  diversity_plot <- list()
  for (i in 1:length(sci_names)) {

    diversity_plot[[i]] <- ggplot2::ggplot(x$data)

    diversity_plot[[i]] <- diversity_plot[[i]] +
      ggplot2::geom_sf(
        data = map_surround,
        fill = land_fill_colour,
        colour = "black",
        aes(geometry = geometry),
        inherit.aes = FALSE
      )

    for (j in names(layer_list)) {
      layer_data <- layer_list[[j]]
      if (j %in% c("ocean", "lakes")) {
        fill_colour <- "#92c5f0"
      } else {
        fill_colour <- "transparent"
      }
      diversity_plot[[i]] <- diversity_plot[[i]] +
        ggplot2::geom_sf(data = layer_data,
                         aes(geometry = geometry),
                         fill = fill_colour,
                         colour = "black",
                         inherit.aes = FALSE)

      ## Here goes the main function ##
      diversity_plot[[i]] <- diversity_plot[[i]] +
        ggplot2::geom_sf(data = split_so[[i]],
                         aes(fill = diversity_val,
                             geometry = geometry),
                         colour = "transparent") +
        cust_leg(list(trans = trans,
                      breaks = breaks,
                      labels = labels,
                      limits = legend_limits)) +
        theme_bw() +
        theme(
          panel.background = element_rect(fill = dplyr::coalesce(panel_bg,
                                                                 "#92c5f0")),
          if(x$map_level == "country") {
            panel.grid.major = element_blank()
            panel.grid.minor = element_blank()
          },
          legend.text = element_text(),
          strip.text = element_text(face = "italic"),
          plot.title = element_text(face = "italic")
        ) +
        labs(title = wrapper(sci_names[[i]], spec_name_wrap_length),
             fill = if(!is.null(legend_title)) wrapper(legend_title,
                                                       legend_title_wrap_length)
             else wrapper(leg_label_default, legend_title_wrap_length)) +
        if(suppress_legend==TRUE) {
          theme(legend.position = "none")
        }

      for (j in names(layer_list)) {
        layer_data <- layer_list[[j]]
        diversity_plot[[i]] <- diversity_plot[[i]] +
          ggplot2::geom_sf(data = layer_data,
                           aes(geometry = geometry),
                           fill = "transparent",
                           colour = "black",
                           inherit.aes = FALSE)
      }

      if (visible_gridlines == TRUE) {
        # plot gridlines
        diversity_plot[[i]]$layers <- c(
          diversity_plot[[i]]$layers,
          ggplot2::geom_sf(aes(geometry = geometry),
                           colour = "black",
                           linewidth = 0.1,
                           fill = "transparent"
          )[[1]]
        )
      }

      # set expand_val to expand the plot if crop_to_grid is not set
      expand_val <- if (crop_to_grid) FALSE else TRUE

      diversity_plot[[i]] <- diversity_plot[[i]] +
        coord_sf(
          crs = x$projection,
          xlim = c(map_lims["xmin"],
                   map_lims["xmax"]),
          ylim = c(map_lims["ymin"],
                   map_lims["ymax"]),
          expand = expand_val)
    }
  }

  # # Check for custom x and y limits and adjust map if found
  # if(any(!is.null(xlims)) & any(!is.null(ylims))) {
  #   for (i in 1:length(diversity_plot)) {
  #     diversity_plot[[i]] <-
  #       suppressMessages(diversity_plot[[i]] + coord_sf(xlim = xlims,
  #                                                       ylim = ylims))
  #   }
  # }

  names(diversity_plot) <- sci_names

  # Combine plots using wrap_plots function from patchwork
  if ((length(diversity_plot) > 0 && single_plot == TRUE) ||
      length(diversity_plot) == 0) {
    diversity_plot <- patchwork::wrap_plots(diversity_plot) +
      plot_annotation_int(title = wrapper(title, title_wrap_length),
                          theme = theme(plot.title = element_text(size = 20)))
  } else if (length(diversity_plot) > 1 & single_plot == FALSE) {
    cat("Option single_plot set to false. Creating separate plot for each species.\n\n")
  }

  # Exit function
  return(diversity_plot)
}
