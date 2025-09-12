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
  geometry <- diversity_val <- map_surround <- layer_list <- map_lims <- NULL

  # Match arguments
  scale <- match.arg(scale)

  # Check that the object is the correct class
  wrong_class(x, "indicator_map", reason = "incorrect")

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
