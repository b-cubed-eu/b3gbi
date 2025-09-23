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
                     grid_fill_colour = NULL,
                     grid_line_colour = NULL,
                     grid_outline_colour = NULL,
                     grid_line_width = NULL,
                     grid_outline_width = NULL,
                     grid_fill_transparency = NULL,
                     legend_title = NULL,
                     legend_limits = NULL,
                     legend_title_wrap_length = 10,
                     title_wrap_length = 60,
                     visible_gridlines = TRUE,
                     layers = NULL,
                     layer_colours = NULL,
                     layer_fill_colours = NULL,
                     scale = c("medium", "small", "large")
) {

  # Set variable definitions to NULL where required
  diversity_val <- geometry <- map_surround <- layer_list <- map_lims <- NULL

  # Match arguments
  scale <- match.arg(scale)

  # Check that the object is the correct class
  wrong_class(x, "indicator_map", reason = "incorrect")

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
    layer_colours = layer_colours,
    layer_fill_colours = layer_fill_colours,
    land_fill_colour = land_fill_colour,
    grid_fill_colour = grid_fill_colour,
    grid_line_colour = grid_line_colour,
    grid_outline_colour = grid_outline_colour,
    grid_line_width = grid_line_width,
    grid_outline_width = grid_outline_width,
    grid_fill_transparency = grid_fill_transparency,
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
