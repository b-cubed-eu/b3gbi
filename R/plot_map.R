#' @title Plot Biodiversity Indicator Map
#'
#' @description Creates a map visualization of a calculated multi-species
#'  biodiversity indicator. Requires an indicator_map object as input. To plot
#'  single-species indicators, use the \code{plot_species_map()} function
#'  instead.
#'
#' @param x An 'indicator_map' object containing multi-species indicator values
#'  associated with map grid cells. This is a required parameter with no
#'  default.
#' @param title (Optional) Plot title. Replace "auto" with your own title if you
#'  want a custom title or if calling the function manually.
#' @param auto_title (Optional) Text for automatic title generation, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param leg_label_default (Optional) Default label for the legend, provided by
#'  an appropriate S3 method (if calling the function manually, leave as NULL).
#' @param xlims (Optional) Custom longitude limits in decimal degrees (WGS84),
#'  as c(min, max). Should be supplied together with ylims. If only xlims is
#'  supplied, the values are instead interpreted in the units of the
#'  indicator_map's coordinate reference system and combined with the map's
#'  own y limits.
#' @param ylims (Optional) Custom latitude limits in decimal degrees (WGS84),
#'  as c(min, max). Should be supplied together with xlims. If only ylims is
#'  supplied, the values are instead interpreted in the units of the
#'  indicator_map's coordinate reference system and combined with the map's
#'  own x limits.
#' @param trans (Optional) Scale transformation for the fill gradient. Can be
#'  any transformation accepted by \code{ggplot2::scale_fill_gradient()} (e.g.,
#'  'log', 'log10' or 'sqrt'), or one of the special values 'boxcox',
#'  'modulus' or 'yj' (Yeo-Johnson), which use the power parameter given in
#'  bcpower.
#' @param bcpower (Optional) Power parameter for the Box-Cox, modulus, or
#'   Yeo-Johnson transformations (used only when trans is 'boxcox', 'modulus'
#'   or 'yj').
#' @param breaks (Optional) Break points for the legend scale.
#' @param labels (Optional) Labels for legend scale break points.
#' @param output_crs (Optional) Coordinate Reference System (CRS) for the output
#'  map. Can be specified as an EPSG code (e.g., 4326) or a proj4string. If
#'  NULL (default), the original CRS of the indicator_map object will be used.
#' @param crop_to_grid (Optional) If TRUE, the grid will determine the edges of
#'  the map. If FALSE, a buffer will be added around the grid. If NULL
#'  (default), will be set to TRUE if map_level is "cube", otherwise FALSE.
#' @param crop_by_region (Optional) If TRUE, the map extent is set to the
#'  bounding box of the region that was specified when calculating the
#'  indicator_map (e.g. the country or continent), instead of the extent of
#'  the grid. This requires that a region was specified, i.e. that the
#'  indicator_map was not calculated with level = "cube" or level = "world".
#'  Default is FALSE.
#' @param ocean_fill_colour (Optional) Colour for the ocean (plot background)
#'  outside of the grid. Default is "#92c5f0" (light blue).
#' @param land_fill_colour (Optional) Colour for the land area outside of the
#'  grid. Default is "grey85".
#' @param grid_fill_colour (Optional) Colour for empty grid cells (non-empty
#'  grid cells will be coloured according to their indicator value). Default is
#'  "transparent".
#' @param grid_line_colour (Optional) Colour for the grid lines. Default is
#'  "black". If visible_gridlines is set to FALSE, this setting will have no
#'  effect.
#' @param grid_line_width (Optional) Width of the grid lines. If NULL
#'  (default), 0.5 for ISEA3H grids and 0.1 otherwise.
#' @param grid_fill_transparency (Optional) Transparency of the grid fill colour
#'  for empty grid cells (0 = fully transparent, 1 = fully opaque). If
#'  visible_gridlines is set to TRUE, default is 0.2. Otherwise, default is 0.
#'  Note that this setting does NOT apply to grid cells with indicator values,
#'  and has no visible effect while grid_fill_colour is "transparent".
#' @param grid_line_transparency (Optional) Transparency of the grid line colour
#'  (0 = fully transparent, 1 = fully opaque). Default is 0.5. If
#'  visible_gridlines is set to FALSE, this setting will have no effect.
#'  *Note that this setting does NOT apply to the grid outline!
#' @param legend_title (Optional) Title for the plot legend.
#' @param legend_limits (Optional) Limits for the legend scale.
#' @param legend_title_wrap_length (Optional) Maximum legend title length before
#'  wrapping to a new line.
#' @param title_wrap_length (Optional) Maximum title length before wrapping to a
#'  new line.
#' @param visible_gridlines (Optional) Show gridlines between cells. Default is
#'  TRUE.
#' @param visible_panel_gridlines (Optional) Show ggplot panel gridlines.
#'  Default is FALSE.
#' @param map_expansion_factor (Optional) Factor to expand the map limits
#' beyond the grid limits. This does NOT expand the boundaries of the plot, it
#' only affects where the crop is applied. If this value is too small, some
#' land may be visibly cut off due to map distortion caused by projections. A
#' larger value will extend the bounding box for cropping to prevent this.
#' Must be a positive number. Default is 0.1. This should be enough for most
#' projections, but you can increase this value if you are using an extreme
#' projection and find that some land is visibly cut off.
#' @param layers (Optional) Additional rnaturalearth layers to plot, e.g.
#'  c("reefs", "playas").
#' @param layer_colours (Optional) Outline colours for the additional layers,
#'  given in the same order as 'layers' (one colour per layer; must be the
#'  same length as 'layers'). If NULL (default), all layer outlines are black.
#' @param layer_fill_colours (Optional) Fill colours for the additional layers,
#'  given in the same order as 'layers' (must be the same length as 'layers').
#'  If NULL (default), layers are unfilled, except "ocean" and "lakes", which
#'  are filled light blue.
#' @param scale (Optional) Scale of Natural Earth data ("small", "medium", or
#'  "large"). Default is 'medium'.
#' @param filter_outliers (Optional) If TRUE, removes geographical outliers
#'  from the plot extent using an Interquartile Range (IQR) method based on the
#'  centroid coordinates of the indicator_map cells. This is particularly useful
#'  for discrete global grids like ISEA3H where stray data points from coordinate
#'  errors can force the map bounds to expand globally. Default is FALSE.
#'
#' @return A ggplot object representing the biodiversity indicator map.
#' Can be customized using ggplot2 functions.
#'
#' @examples
#' \donttest{
#' evenness_map <- pielou_evenness_map(example_cube_1,
#'   level = "country",
#'   region = "Denmark"
#' )
#' plot_map(
#'   x = evenness_map,
#'   title = "Map of Species Evenness in Denmark",
#'   legend_title = "Evenness"
#' )
#' }
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
                     output_crs = NULL,
                     crop_to_grid = NULL,
                     crop_by_region = FALSE,
                     ocean_fill_colour = NULL,
                     land_fill_colour = NULL,
                     grid_fill_colour = NULL,
                     grid_line_colour = NULL,
                     grid_line_width = NULL,
                     grid_fill_transparency = NULL,
                     grid_line_transparency = NULL,
                     legend_title = NULL,
                     legend_limits = NULL,
                     legend_title_wrap_length = 10,
                     title_wrap_length = 60,
                     visible_gridlines = TRUE,
                     visible_panel_gridlines = FALSE,
                     map_expansion_factor = 0.1,
                     layers = NULL,
                     layer_colours = NULL,
                     layer_fill_colours = NULL,
                     scale = c("medium", "small", "large"),
                     filter_outliers = FALSE) {
  # Set variable definitions to NULL where required
  diversity_val <- geometry <- map_surround <- layer_list <- map_lims <- NULL

  # Match arguments
  scale <- match.arg(scale)

  # Set CRS for the plot
  if (!is.null(output_crs)) {
    # If output CRS is provided, check if it is valid and then use it
    check_crs_units(output_crs)
    projection <- output_crs
  } else {
    # Otherwise, use the projection from the indicator_map object
    projection <- x$projection
  }

  # Check that the object is the correct class
  wrong_class(x, "indicator_map", reason = "incorrect")

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

  # Name layer colours by layer so each colour is matched to its layer
  if (!is.null(layer_colours)) names(layer_colours) <- layers
  if (!is.null(layer_fill_colours)) names(layer_fill_colours) <- layers

  # Get plot title (if set to "auto")
  title <- if (title == "auto") auto_title else title

  # Filter spatial outliers if requested
  if (filter_outliers) {
    # Extract coordinates from the geometry centroids
    coords <- do.call(rbind, lapply(sf::st_geometry(x$data), function(geom) {
      sf::st_coordinates(sf::st_centroid(geom))
    }))

    qx <- stats::quantile(coords[, 1], probs = c(0.25, 0.75), na.rm = TRUE)
    qy <- stats::quantile(coords[, 2], probs = c(0.25, 0.75), na.rm = TRUE)
    iqr_x <- qx[2] - qx[1]
    iqr_y <- qy[2] - qy[1]

    # Standard 1.5 IQR could be too restrictive for clustered maps, use 3 IQR for extreme outliers
    keep <- coords[, 1] >= (qx[1] - 3 * iqr_x) &
      coords[, 1] <= (qx[2] + 3 * iqr_x) &
      coords[, 2] >= (qy[1] - 3 * iqr_y) &
      coords[, 2] <= (qy[2] + 3 * iqr_y)

    x$data <- x$data[keep, ]

    # Recalculate original bbox based on the filtered data
    x$coord_range <- sf::st_bbox(x$data)[c("xmin", "ymin", "xmax", "ymax")]
    x$original_bbox <- sf::st_as_sfc(sf::st_bbox(x$data))
  }

  # Prepare data and layers for plotting
  map_data_list <- prepare_map_for_plot(
    x, xlims, ylims, layers, scale, crop_to_grid, crop_by_region, projection,
    map_expansion_factor
  )

  # Unpack the list
  list2env(map_data_list, envir = environment())

  # Set default grid line width based on grid type
  if (is.null(grid_line_width)) {
    grid_line_width <- if (!is.null(x$grid_type) && x$grid_type == "isea3h") 0.5 else 0.1
  }

  # Create plot using the helper function
  plot <- create_map_plot(
    data = x$data,
    map_surround = map_surround,
    layer_list = layer_list,
    layer_colours = layer_colours,
    layer_fill_colours = layer_fill_colours,
    land_fill_colour = land_fill_colour,
    ocean_fill_colour = ocean_fill_colour,
    grid_fill_colour = grid_fill_colour,
    grid_line_colour = grid_line_colour,
    grid_line_width = grid_line_width,
    grid_fill_transparency = grid_fill_transparency,
    grid_line_transparency = grid_line_transparency,
    trans = trans,
    bcpower = bcpower,
    breaks = breaks,
    labels = labels,
    legend_limits = legend_limits,
    map_level = x$map_level,
    visible_gridlines = visible_gridlines,
    visible_panel_gridlines = visible_panel_gridlines,
    crop_to_grid = crop_to_grid,
    map_lims = map_lims,
    map_lims_original = x$coord_range,
    projection = projection,
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
