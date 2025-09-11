#' @noRd
prepare_map_for_plot <- function(x, xlims, ylims, layers, scale, crop_to_grid) {

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
    if (is.vector(xlims) && length(xlims) == 2) {
      map_lims["xmin"] <- xlims[1]
      map_lims["xmax"] <- xlims[2]
    } else {
      stop("Please provide numeric xlims values in the form of c(1,2)")
    }
  }

  # Override vertical axis limits if custom limits provided by user
  if (!is.null(ylims)) {
    if (is.vector(ylims) && length(ylims) == 2) {
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

  # Return map limits, surrounding map data, and layer list
  return(list(
    map_lims = map_lims,
    map_surround = map_surround,
    layer_list = layer_list
  ))

}
