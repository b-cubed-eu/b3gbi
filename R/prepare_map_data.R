#' @noRd
prepare_map_data <- function(data,
                             projection,
                             map_lims,
                             xlims,
                             ylims,
                             map_data_sf,
                             layers,
                             scale,
                             crop_to_grid,
                             map_region,
                             map_level,
                             map_type,
                             crop_by_region) {

  # Set variables to NULL
  scalerank <- featurecla <- geometry <- . <- NULL

  # Define a lat/long CRS (WGS84)
  latlong_crs <- sf::st_crs(4326)

  # Determine bounding box for cropping:
  # ... from bbox of map data for stored regions if crop_by_region set
  if (crop_by_region == TRUE) {
    map_data_region <- download_ne_data(region = map_region,
                                    level = map_level,
                                    ne_scale = scale,
                                    ne_type = map_type)
    latlong_extent <- sf::st_bbox(map_data_region)
    map_lims <- latlong_extent

  # ... from user-supplied xlims and ylims in lat/long if provided
  } else if (!is.null(xlims) && !is.null(ylims)) {
    xylims <- c(xlims[1], ylims[1], xlims[2], ylims[2])
    names(xylims) <- c("xmin", "ymin", "xmax", "ymax")
    latlong_extent <- sf::st_bbox(xylims, crs = latlong_crs)
  #  latlong_extent <- sf::st_set_crs(latlong_extent, latlong_crs)

    # ... or from the map limits in the projected CRS if crop_by_region set
  } else if (crop_by_region == TRUE) {
    latlong_extent <- sf::st_bbox(map_data_sf)

    # ... or from the indicator data in lat/long if using a km-based CRS other
    # than EPSG:3035
  } else if (check_crs_units(projection) == "km"
             && sf::st_crs(projection)$input != "EPSG:3035") {
    # Get bounding box of the indicator map data
    utm_bbox <- sf::st_bbox(data)
    utm_crs <- sf::st_crs(data)
    # Transform data bbox to lat/long to define the extent
    bbox_polygon_utm <- sf::st_as_sfc(utm_bbox, crs = utm_crs)
    bbox_latlong <- sf::st_transform(bbox_polygon_utm, crs = latlong_crs)
    latlong_extent <- sf::st_bbox(bbox_latlong)

    # ... or from existing map limits
  } else {
    latlong_extent <- sf::st_bbox(map_lims, crs = latlong_crs)
  }

  latlong_extent <- sf::st_transform(latlong_extent, crs = projection)

  # Expand the bounding box by 10% in all directions if not cropping to grid
  # and no xlims/ylims were provided
#  if (!crop_to_grid && is.null(xlims) && is.null(ylims)) {
    expand_percent <- 0.1
    lon_range <- latlong_extent["xmax"] - latlong_extent["xmin"]
    lat_range <- latlong_extent["ymax"] - latlong_extent["ymin"]
    latlong_extent <- sf::st_bbox(c(
      latlong_extent["xmin"] - (expand_percent * lon_range),
      latlong_extent["ymin"] - (expand_percent * lat_range),
      latlong_extent["xmax"] + (expand_percent * lon_range),
      latlong_extent["ymax"] + (expand_percent * lat_range)
    ), crs = latlong_crs)
#  }

  sf::st_agr(map_data_sf) <- "constant"
  # Crop to the above-determined bounding box
  map_surround <- map_data_sf %>%
    sf::st_transform(crs = projection) %>%
    sf::st_make_valid() %>%
    sf::st_crop(latlong_extent)

  # Crop layers if provided
  layer_list <- list()
  if (!is.null(layers)) {
    for (i in seq_along(layers)) {
      layer_data <- add_ne_layer(layers[i], scale, latlong_extent)
      if (!is.null(layer_data) && nrow(layer_data) > 0) {
        layer_list[[i]] <- layer_data
        names(layer_list)[[i]] <- layers[i]
      }
    }
  }

  # Transform additional layers
  if (!is.null(layers)) {
    layer_list <- lapply(layer_list, function(x) {
      sf::st_transform(x, crs = projection)
    })
  }

  # Return a list containing the final projected map data and layers
  return(list(
    map_surround = map_surround,
    layer_list = layer_list,
    map_lims = map_lims
  ))
}
