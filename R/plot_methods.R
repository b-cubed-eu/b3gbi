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

  # Set defaults
  y_label_default <- "Occurrences"
  auto_title <- "Total Occurrences (Segregated by Dataset)"

  # Set type as a factor and remove any types with only 1 occurrence
  x$data$type <- factor(x$data$type,
                        levels = unique(x$data$type))

  # Filter out datasets with no occurrences or too few to plot
  x$data <-
    x$data %>%
    dplyr::mutate(numrows = ifelse(n() > 0, n(), 0), .by = type) %>%
    dplyr::filter(numrows >= 2) %>%
    dplyr::select(-numrows)

  if (!is.null(min_occurrences)) {
  # Filter out datasets with fewer than the minimum occurrences, if min_occurrences parameter set
    x$data <-
      x$data %>%
      dplyr::mutate(totalocc = sum(diversity_val), .by = type) %>%
      dplyr::filter(totalocc >= min_occurrences) %>%
      dplyr::select(-totalocc)
  }

  # Keep only n datasets with the most occurrences, where n is determined by the max_datasets parameter
  datasets <-
    x$data %>%
    dplyr::summarize(totalocc = sum(diversity_val), .by = type) %>%
    dplyr::slice_max(order_by = totalocc, n = max_datasets)

  x$data <-
    x$data %>%
    dplyr::filter(type %in% datasets$type)

  # Call generalized plot_map function
  trend_plot <- plot_ts(x,
                        y_label_default = y_label_default,
                        auto_title = auto_title,
                        x_breaks = x_breaks,
                        ...)

    # Use facets to separate multiple species trends
    trend_plot <- trend_plot +
      facet_wrap(vars(type),
                 scales = facet_scales,
                 nrow = facet_rows,
                 ncol = facet_cols,
                 labeller = label_wrap_gen(width = facet_label_width)) +
      theme(legend.position = "none")

  # Show plot
  trend_plot
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

    # Set defaults
    y_label_default <- "Occurrences"
    auto_title <- "Total Occurrences (Segregated by Type)"

    # Set type as a factor and remove any types with only 1 occurrence
    x$data$type <- factor(x$data$type,
                          levels = unique(x$data$type))

    # Filter out types with too few or no occurrences
    x$data <-
      x$data %>%
      dplyr::mutate(numrows = ifelse(n() > 0, n(), 0), .by = type) %>%
      dplyr::filter(numrows >= 2) %>%
      dplyr::select(-numrows)

    # Call generalized plot_map function
    trend_plot <- plot_ts(x,
                          y_label_default = y_label_default,
                          auto_title = auto_title,
                          x_breaks = x_breaks,
                          ...)

      # Use facets to separate multiple species trends
      trend_plot <- trend_plot +
        facet_wrap(vars(type),
                   scales = facet_scales,
                   nrow = facet_rows,
                   ncol = facet_cols,
                   labeller = label_wrap_gen(width = facet_label_width)) +
        theme(legend.position = "none")

    # Show plot
    trend_plot
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

    if (is.null(envelopecolour)) envelopecolour = "lightsteelblue"


    # Colour the area under the curve
     trend_plot <- trend_plot +
       geom_ribbon(aes(ymin = 0,
                       ymax = diversity_val),
                   fill = envelopecolour, alpha = 0.3)
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
#' @param breaks (Optional) Break points for the legend scale.
#' @param labels (Optional) Labels for legend scale break points.
#' @param Europe_crop_EEA If TRUE, crops maps of Europe using the EPSG:3035 CRS
#'    to exclude far-lying islands (default is TRUE, but does not affect other maps
#'    or projections). Will not work if crop_to_grid is set to TRUE.
#' @param crop_to_grid If TRUE, the grid will determine the edges of the map.Overrides
#'    Europe_crop_EEA. Default is FALSE.
#' @param surround If TRUE, includes surrounding land area in gray when plotting
#'    at the country or continent level. If FALSE, all surrounding area will be colored
#'    ocean blue (or whatever colour you set manually using panel_bg). Default is TRUE.
#' @param panel_bg (Optional) Background colour for the map panel.
#' @param land_fill_colour (Optional) Colour for the land area outside of the grid
#'    (if surround = TRUE). Default is "grey85".
#' @param legend_title (Optional) Title for the plot legend.
#' @param legend_limits (Optional) Limits for the legend scale.
#' @param legend_title_wrap_length Maximum legend title length before wrapping to a new line.
#' @param title_wrap_length Maximum title length before wrapping to a new line.
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
                     breaks = NULL,
                     labels = NULL,
                     Europe_crop_EEA = TRUE,
                     crop_to_grid = FALSE,
                     surround = TRUE,
                     panel_bg = NULL,
                     land_fill_colour = NULL,
                     legend_title = NULL,
                     legend_limits = NULL,
                     legend_title_wrap_length = 10,
                     title_wrap_length = 60
                     ) {

  # Get map limits
  map_lims <- x$coord_range

  # Crop map of Europe to leave out far-lying islands (if flag set)
  if (Europe_crop_EEA == TRUE &
      x$map_level == "continent" &
      x$map_region == "Europe" &
      x$projection == "EPSG:3035" &
      crop_to_grid == FALSE)
  {

    # Set attributes as spatially constant to avoid warnings
    sf::st_agr(x$data) <- "constant"

    # Manually set cropped limits
    map_lims <- c(2600000, 1600000, 7000000, 6000000)
    names(map_lims) <- c("xmin", "ymin", "xmax", "ymax")

  }

  # Get world data to plot surrounding land if surround flag is set
  if (surround == TRUE) {
    map_surround <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = x$projection)

  # Otherwise make all the surroundings ocean blue (unless a different colour is specified)
  } else {
    if (is.null(panel_bg)) { panel_bg = "#92c5f0" }
  }

  # Define function to wrap title and legend title if too long
  wrapper <- function(x, ...)
  {
    paste(strwrap(x, ...), collapse = "\n")
  }

  # Get plot title (if set to "auto")
  if (!is.null(title)) {
    if (title == "auto") {
      title <- auto_title
    }
  }

  # Define function to modify legend
  cust_leg <- function(scale.params = list()) {
    do.call("scale_fill_gradient", modifyList(
      list(low = "gold", high = "firebrick4", na.value = "grey95"),
      scale.params)
    )
  }

  # Plot map
  diversity_plot <- ggplot2::ggplot(x$data) +
    geom_sf(aes(fill = diversity_val,
                geometry = geometry),
            colour = "black") +
    cust_leg(list(trans = trans,
                  breaks = breaks,
                  labels = labels,
                  limits = legend_limits)) +
    coord_sf(
      xlim = c(map_lims["xmin"],
               map_lims["xmax"]),
      ylim = c(map_lims["ymin"],
               map_lims["ymax"]),
      if(crop_to_grid == TRUE) {
        expand = FALSE
      } else {
        expand = TRUE
      }
    ) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = if(!is.null(panel_bg)) panel_bg
                                      else "#92c5f0"),
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


  land_fill_colour <- ifelse(is.null(land_fill_colour), "grey85", land_fill_colour)

  # If surround flag set, add surrounding countries to map
  if (surround == TRUE) {
    diversity_plot$layers <- c(geom_sf(data = map_surround, fill = land_fill_colour)[[1]], diversity_plot$layers)
  }

  # Check for custom x and y limits and adjust map if found
  if(any(!is.null(xlims)) & any(!is.null(ylims))) {
    diversity_plot <-
      diversity_plot + coord_sf(xlim = xlims,
                                ylim = ylims)

  }

  # Wrap title if longer than user-specified wrap length
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
#' @param title Plot title. Replace "auto" with your own title if you want a
#'   custom title or if calling the function manually.
#' @param auto_title Text for automatic title generation, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param y_label_default Default label for the y-axis, provided by an appropriate
#'   S3 method (if calling the function manually, leave as NULL).
#' @param suppress_y If TRUE, suppresses y-axis labels.
#' @param smoothed_trend If TRUE, plot a smoothed trendline.
#' @param linecolour (Optional) Colour for the indicator line.
#'   Default is darkorange. Set to "NA" if you don't want to plot the indicator line.
#' @param ribboncolour (Optional) Colour for the bootstrapped confidence intervals.
#'   Default is goldenrod1. Set to "NA" if you don't want to plot the CIs.
#' @param trendlinecolour (Optional) Colour for the smoothed trendline.
#'   Default is blue. Set to "NA" if you don't want to plot the trend.
#' @param envelopecolour (Optional) Colour for the uncertainty envelope.
#'   Default is lightsteelblue. Set to "NA" if you don't want to plot the trend
#'   uncertainty.
#' @param gridoff  If TRUE, hides gridlines.
#' @param x_label Label for the x-axis.
#' @param y_label Label for the y-axis.
#' @param min_year (Optional)  Earliest year to include in the plot.
#' @param max_year (Optional)  Latest year to include in the plot.
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
                    title = "auto",
                    auto_title = NULL,
                    y_label_default = NULL,
                    suppress_y = FALSE,
                    smoothed_trend = TRUE,
                    linecolour = NULL,
                    ribboncolour = NULL,
                    trendlinecolour = NULL,
                    envelopecolour = NULL,
                    gridoff = FALSE,
                    x_label = NULL,
                    y_label = NULL,
                    min_year = NULL,
                    max_year = NULL,
                    x_breaks = 10,
                    y_breaks = 6,
                    wrap_length = 60
                    ) {

  # Filter by min and max year if set
  if (!is.null(min_year) | !is.null(max_year)) {
    min_year <- ifelse(is.null(min_year), x$first_year, min_year)
    max_year <- ifelse(is.null(max_year), x$last_year, max_year)
    x$data <-
      x$data %>%
      dplyr::filter(year >= min_year) %>%
      dplyr::filter(year <= max_year)
  } else {
    min_year <- x$first_year
    max_year <- x$last_year
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

  # Create plot with trend line
  trend_plot <-
    ggplot2::ggplot(x$data, aes(x = year,
                                y = diversity_val)) +
    geom_line(colour = linecolour,
              lwd = 1) +
    scale_x_continuous(breaks = breaks_pretty_int(n = x_breaks)) +
    scale_y_continuous(breaks = breaks_pretty_int(n = y_breaks)) +
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

  if ("ll" %in% colnames(x$data) & "ul" %in% colnames(x$data)) {

    trend_plot <- trend_plot +
      geom_ribbon(aes(ymin = ll,
                    ymax = ul),
                alpha = 0.3,
                fill = ribboncolour)

  }

  if (smoothed_trend == TRUE) {

    # Add a smoothed trend
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
  }

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

#' @title Plot Occurrence Trends or Range Size Trends for Individual Species
#'
#' @description  Creates time series plots of species occurrences or species range
#' sizes, with an optional smoothed trendline, and visualizes uncertainty.
#'
#' @param x An 'indicator_ts' object containing time series of indicator values
#'   matched to species names and/or taxon keys.
#' @param species Species you want to map occurrences for. Can be either numerical
#'   taxonKeys or species names. Partial species names can be used (the function
#'   will try to match them).
#' @param title Plot title. Replace "auto" with your own title if you want a
#'   custom title or if calling the function manually.
#' @param auto_title Text for automatic title generation, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param y_label_default Default label for the y-axis, provided by an appropriate
#'   S3 method (if calling the function manually, leave as NULL).
#' @param suppress_y If TRUE, suppresses y-axis labels.
#' @param smoothed_trend If TRUE, plot a smoothed trendline.
#' @param linecolour (Optional) Colour for the indicator line.
#'   Default is darkorange.
#' @param ribboncolour (Optional) Colour for the bootstrapped confidence intervals.
#'   Default is goldenrod1. Set to "NA" if you don't want to plot the CIs.
#' @param trendlinecolour (Optional) Colour for the smoothed trendline.
#'   Default is blue.
#' @param envelopecolour (Optional) Colour for the uncertainty envelope.
#'   Default is lightsteelblue.
#' @param gridoff  If TRUE, hides gridlines.
#' @param x_label Label for the x-axis.
#' @param y_label Label for the y-axis.
#' @param min_year (Optional)  Earliest year to include in the plot.
#' @param max_year (Optional)  Latest year to include in the plot.
#' @param x_breaks Integer giving desired number of breaks for x axis.
#'   (May not return exactly the number requested.)
#' @param y_breaks Integer giving desired number of breaks for y axis.
#'   (May not return exactly the number requested.)
#' @param title_wrap_length  Maximum title length before wrapping to a new line.
#' @param single_plot By default all species occurrence time series will be combined
#'   into a single multi-panel plot. Set this to FALSE to plot each species separately.
#'
#' @return A ggplot object representing species range or occurrence time series plot(s).
#' Can be customized using ggplot2 functions.
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
                            y_label_default = NULL,
                            auto_title = NULL,
                            title = "auto",
                            min_year = NULL,
                            max_year = NULL,
                            smoothed_trend = TRUE,
                            linecolour = NULL,
                            ribboncolour = NULL,
                            trendlinecolour = NULL,
                            envelopecolour = NULL,
                            single_plot = TRUE,
                            x_label = NULL,
                            y_label = NULL,
                            x_breaks = 10,
                            y_breaks = 6,
                            suppress_y = FALSE,
                            gridoff = FALSE,
                            title_wrap_length = 60
                            ) {

  # Filter by min and max year if set
  if (!is.null(min_year) | !is.null(max_year)) {
    min_year <- ifelse(is.null(min_year), x$first_year, min_year)
    max_year <- ifelse(is.null(max_year), x$last_year, max_year)
    x$data <-
      x$data %>%
      dplyr::filter(year >= min_year) %>%
      dplyr::filter(year <= max_year)
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

  # Create bootstrapped confidence intervals if columns present
  if ("ll" %in% colnames(x$data) & "ul" %in% colnames(x$data)) {

    ci_ribbon <- list(

      geom_ribbon(aes(ymin = ll,
                      ymax = ul),
                  alpha = 0.3,
                  fill = ribboncolour)
    )
  } else {
    ci_ribbon <- list()
  }

  # Create smoothed trend if desired
  if (smoothed_trend == TRUE) {
    smoothing <- list(
      geom_smooth(fill = envelopecolour,
                  lwd = 1,
                  linetype = 0,
                  method = "loess",
                  formula = "y ~ x"),
      stat_smooth(colour = trendlinecolour,
                  geom = "line",
                  method = "loess",
                  formula = "y ~ x",
                  linetype = "dashed",
                  alpha = 0.3,
                  lwd = 1)
    )
  } else {
    smoothing <- list()
  }

  # Create plot with trend line
  trend_plot <-
    purrr::map2(split_so,
                sci_names,
                function(x., y) {
                  ggplot2::ggplot(x., aes(x = year,
                                          y = diversity_val)) +
                    geom_line(colour = linecolour,
                              lwd = 1) +
                    scale_x_continuous(breaks = breaks_pretty_int(n = x_breaks)) +
                    scale_y_continuous(breaks = breaks_pretty_int(n = y_breaks)) +
                    labs(x = x_label, y = y_label,
                         title = title) +
                    theme_minimal() +
                    theme(plot.title = element_text(hjust = 0.5, face = "italic"),
                          text = element_text(size = 14),
                          if (gridoff == TRUE) { panel.grid.major = element_blank() },
                          panel.grid.minor = element_blank(),
                          if (suppress_y==TRUE) { axis.text.y = element_blank() },
                          strip.text = element_text(face = "italic")
                    ) +
                    labs(title = y) +
                    ci_ribbon +
                    smoothing
                })

  names(trend_plot) <- sci_names

  # Combine plots using wrap_plots function from patchwork
  if (length(trend_plot) > 1 & single_plot == TRUE) {
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
#' @param suppress_legend Do not show legend. This should be set to true when
#'   plotting species ranges, as all cell values are 1.
#' @param leg_label_default Default label for the legend, provided by an
#'   appropriate S3 method (if calling the function manually, leave as NULL).
#' @param xlims  (Optional) Custom x-axis limits.
#' @param ylims (Optional) Custom y-axis limits.
#' @param trans (Optional) Scale transformation for the fill gradient
#'   (e.g., 'log').
#' @param breaks (Optional) Break points for the legend scale.
#' @param labels (Optional) Labels for legend scale break points.
#' @param Europe_crop_EEA If TRUE, crops maps of Europe using the EPSG:3035 CRS
#'    to exclude far-lying islands (default is TRUE, but does not affect other maps
#'    or projections).
#' @param crop_to_grid If TRUE, the grid will determine the edges of the map.Overrides
#'    Europe_crop_EEA. Default is FALSE.
#' @param surround  If TRUE, includes surrounding land area in gray when plotting
#'    at the country or continent level. If FALSE, all surrounding area will be coloured
#'    ocean blue (or whatever colour you set manually using panel_bg). Default is TRUE.
#' @param panel_bg  (Optional) Background colour for the map panel.
#' @param land_fill_colour (Optional) Colour for the land area outside of the grid
#'    (if surround = TRUE). Default is "grey85".
#' @param legend_title (Optional) Title for the plot legend.
#' @param legend_limits (Optional) Limits for the legend scale.
#' @param legend_title_wrap_length Maximum legend title length before wrapping to a new line.
#' @param title_wrap_length Maximum title length before wrapping to a new line.
#' @param single_plot By default all species occurrence time series will be combined
#'    into a single multi-panel plot. Set this to FALSE to plot each species separately.
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
                             leg_label_default = NULL,
                             auto_title = NULL,
                             suppress_legend = FALSE,
                             title = "auto",
                             xlims = NULL,
                             ylims = NULL,
                             trans = NULL,
                             breaks = NULL,
                             labels = NULL,
                             Europe_crop_EEA = TRUE,
                             crop_to_grid = FALSE,
                             surround = TRUE,
                             single_plot = TRUE,
                             panel_bg = NULL,
                             land_fill_colour = NULL,
                             legend_title = NULL,
                             legend_limits = NULL,
                             legend_title_wrap_length = 10,
                             title_wrap_length = 60
                             ) {

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

  # Get map limits
  map_lims <- x$coord_range

  # Crop map of Europe to leave out far-lying islands (if flag set)
  if (Europe_crop_EEA == TRUE &
      x$map_level == "continent" &
      x$map_region == "Europe" &
      x$projection == "EPSG:3035" &
      crop_to_grid == TRUE)
  {

    # Set attributes as spatially constant to avoid warnings
    sf::st_agr(x$data) <- "constant"

    # Manually set cropped limits
    map_lims <- c(2600000, 1600000, 7000000, 6000000)
    names(map_lims) <- c("xmin", "ymin", "xmax", "ymax")

  }

  # Get world data to plot surrounding land if surround flag is set
  if (surround == TRUE) {
    map_surround <- rnaturalearth::ne_countries(scale = "medium",
                                                returnclass = "sf") %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = "EPSG:3035")

  # Otherwise make the surroundings ocean blue (unless another colour is specified)
  } else {
    if (is.null(panel_bg)) { panel_bg = "#92c5f0" }
  }

  # Define function to wrap title and legend title if too long
  wrapper <- function(x, ...)
  {
    paste(strwrap(x, ...), collapse = "\n")
  }

  # Get plot title (if set to "auto")
  if (!is.null(title)) {
    if (title == "auto") {
      title <- auto_title
    }
  }

  # Define function to modify legend
  cust_leg <- function(scale.params = list()) {
    do.call("scale_fill_gradient", modifyList(
      list(low = "gold", high = "firebrick4", na.value = "grey95"),
      scale.params)
    )
  }

  # Plot on map
  diversity_plot <-
    purrr::map2(split_so,
                sci_names,
                function(x., y) {
                  ggplot2::ggplot(x.) +
                    geom_sf(data = x$data,
                            aes(geometry = geometry),
                            fill = "grey95") +
                    geom_sf(aes(fill = diversity_val,
                                geometry = geometry)) +
                    cust_leg(list(trans = trans,
                                  breaks = breaks,
                                  labels = labels,
                                  limits = legend_limits)) +
                    coord_sf(
                      xlim = c(map_lims["xmin"],
                               map_lims["xmax"]),
                      ylim = c(map_lims["ymin"],
                               map_lims["ymax"]),
                      if (crop_to_grid == TRUE) {
                        expand = FALSE
                      } else {
                        expand = TRUE
                      }
                    ) +
                    #scale_x_continuous() +
                    theme_bw()+
                    theme(
                      panel.background = element_rect(fill = panel_bg),
                      if(x$map_level == "country") {
                        panel.grid.major = element_blank()
                        panel.grid.minor = element_blank()
                      },
                      legend.text = element_text(),
                      strip.text = element_text(face = "italic"),
                      plot.title = element_text(face = "italic")
                    ) +
                    labs(title = y,
                         fill = if(!is.null(legend_title)) wrapper(legend_title,
                                                                   legend_title_wrap_length)
                         else wrapper(leg_label_default,
                                      legend_title_wrap_length)) +
                    if(suppress_legend==TRUE) {
                      theme(legend.position = "none")
                    }
                })

  land_fill_colour <- ifelse(is.null(land_fill_colour), "grey85", land_fill_colour)

  # If surround flag is set, add surrounding countries to the map
  if (surround == TRUE) {
    for (i in 1:length(diversity_plot)) {
      diversity_plot[[i]]$layers <- c(geom_sf(data = map_surround,
                                              fill = land_fill_colour)[[1]],
                                      diversity_plot[[i]]$layers)
    }
  }

  names(diversity_plot) <- sci_names

  # Combine plots using wrap_plots function from patchwork
  if (length(diversity_plot) > 1 & single_plot == TRUE) {
    diversity_plot <- patchwork::wrap_plots(diversity_plot) +
      plot_annotation_int(title = title,
                          theme = theme(plot.title = element_text(size = 20)))
  } else if (length(diversity_plot) > 1 & single_plot == FALSE) {
    cat("Option single_plot set to false. Creating separate plot for each species.\n\n")
  }


  # Exit function
  return(diversity_plot)

}

