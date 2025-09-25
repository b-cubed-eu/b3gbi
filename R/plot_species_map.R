#' @title Plot Occurrence Map or Range Map of Individual Species
#'
#' @description Creates map visualizations of species ranges or species
#'  occurrences. Requires an indicator_map object created using the
#'  \code{spec_occ_map()} or \code{spec_range_map()} functions as input. To plot
#'  multi-species indicators (e.g., species richness or evenness), use the
#'  \code{plot_map()} function instead.
#'
#' @inheritParams plot_map
#'
#' @param x An 'indicator_map' object containing indicator values for individual
#'  species associated with map grid cells. This object is typically created
#'  using the \code{spec_occ_map()} or \code{spec_range_map()} functions.
#'  This is a required parameter with no default.
#' @param species Species you want to map occurrences for. Can be either
#'  numerical taxonKeys or species names. Partial species names can be used
#'  (the function will try to match them). This is a required parameter with
#'  no default.
#' @param single_plot (Optional) If TRUE, all species occurrence time series
#'  will be combined into a single multi-panel plot. Set this to FALSE to plot
#'  each species separately. Default is TRUE.
#' @param suppress_legend (Optional) Do not show legend. This defaults to FALSE
#'  but will be forcibly set to TRUE when plotting species ranges, as all cell
#'  values are 1.
#' @param spec_name_wrap_length (Optional) Maximum species name length before
#'  wrapping to a new line. Default: 40 characters.
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
                             single_plot = TRUE,
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
                             crop_by_region = FALSE,
                             ocean_fill_colour = NULL,
                             land_fill_colour = NULL,
                             grid_fill_colour = NULL,
                             grid_line_colour = NULL,
                             grid_outline_colour = NULL,
                             grid_line_width = NULL,
                             grid_outline_width = NULL,
                             grid_fill_transparency = NULL,
                             grid_line_transparency = NULL,
                             legend_title = NULL,
                             legend_limits = NULL,
                             legend_title_wrap_length = 10,
                             title_wrap_length = 60,
                             spec_name_wrap_length = 40,
                             visible_gridlines = TRUE,
                             visible_grid_outline = TRUE,
                             complete_grid_outline = TRUE,
                             layers = NULL,
                             layer_colours = NULL,
                             layer_fill_colours = NULL,
                             scale = c("medium", "small", "large")
) {

  # Set variable definitions to NULL where required
  geometry <- diversity_val <- map_surround <- layer_list <- map_lims <- NULL

  # Match arguments
  scale <- match.arg(scale)

  # Check that the object is the correct class
  wrong_class(x, "indicator_map", reason = "incorrect")

  # Set suppress_legend to TRUE if plotting a range map
  if (inherits(x, "spec_range_map")) suppress_legend <- TRUE

  if (is.null(crop_to_grid)) {
    crop_to_grid <- if (x$map_level == "cube") TRUE else FALSE
  }

  if (crop_by_region == TRUE &&
      any(x$map_region %in% c("cube", "world"))) {
    stop("crop_by_region is set to TRUE, but no region was specified when
         calculating the indicator_map. Please recalculate the indicator_map
         with a region specified.")
  }

  # Check that layers, layer_colours, and layer_fill_colours are same length
  if (length(layer_colours) != length(layers) && !is.null(layer_colours)) {
    stop("If layer_colours is provided, it must be the same length as layers.")
  }
  if (length(layer_fill_colours) != length(layers) &&
      !is.null(layer_fill_colours)) {
    stop(
      "If layer_fill_colours is provided, it must be the same length as layers."
    )
  }

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
    x, xlims, ylims, layers, scale, crop_to_grid, crop_by_region
  )

  # Unpack the list
  list2env(map_data_list, envir= environment())

  # Create the plots for each species using the helper function
  plot <- purrr::map(seq_along(sci_names), function(i) {
    create_map_plot(
      data = split_so[[i]],
      map_surround = map_surround,
      layer_list = layer_list,
      layer_colours = layer_colours,
      layer_fill_colours = layer_fill_colours,
      land_fill_colour = land_fill_colour,
      ocean_fill_colour = ocean_fill_colour,
      grid_fill_colour = grid_fill_colour,
      grid_line_colour = grid_line_colour,
      grid_outline_colour = grid_outline_colour,
      grid_line_width = grid_line_width,
      grid_outline_width = grid_outline_width,
      grid_fill_transparency = grid_fill_transparency,
      grid_line_transparency = grid_line_transparency,
      trans = trans,
      bcpower = bcpower,
      breaks = breaks,
      labels = labels,
      legend_limits = legend_limits,
      map_level = x$map_level,
      visible_gridlines = visible_gridlines,
      visible_grid_outline = visible_grid_outline,
      complete_grid_outline = complete_grid_outline,
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
