prepare_map_data <- function(data,
                             projection,
                             map_lims,
                             xlims,
                             ylims,
                             map_data_sf,
                             layers,
                             scale,
                             crop_to_grid) {

  map_surround <- map_data_sf %>%
    sf::st_transform(crs = projection) %>%
    sf::st_make_valid()

  if (check_crs_units(projection) == "km" &&
      sf::st_crs(projection)$input != "EPSG:3035") {

    # Get bounding box of the indicator map data in UTM
    utm_bbox <- sf::st_bbox(data)
    utm_crs <- sf::st_crs(data)

    # Define a lat/long CRS (WGS84)
    latlong_crs <- sf::st_crs(4326)

    # Create a bounding box polygon in UTM
    bbox_polygon_utm <- sf::st_as_sfc(utm_bbox, crs = utm_crs)

    # Transform the bounding box to lat/long
    bbox_latlong <- sf::st_transform(bbox_polygon_utm, crs = latlong_crs)
    latlong_extent <- sf::st_bbox(bbox_latlong)

    # Get user-supplied xlims and ylims in lat/long if provided
    if (!is.null(xlims) && !is.null(ylims)) {
      xylims <- c(xlims[1], ylims[1], xlims[2], ylims[2])
      names(xylims) <- c("xmin", "ymin", "xmax", "ymax")
      latlong_extent <- sf::st_bbox(xylims)
      latlong_extent <- sf::st_set_crs(latlong_extent, latlong_crs)
    }

    # Conditionally expand the lat/long bounding box
    if (!crop_to_grid && is.null(xlims) && is.null(ylims)) {
      expand_percent <- 0.1
      lon_range <- latlong_extent["xmax"] - latlong_extent["xmin"]
      lat_range <- latlong_extent["ymax"] - latlong_extent["ymin"]
      expanded_latlong_bbox <- sf::st_bbox(c(
        latlong_extent["xmin"] - (expand_percent * lon_range),
        latlong_extent["ymin"] - (expand_percent * lat_range),
        latlong_extent["xmax"] + (expand_percent * lon_range),
        latlong_extent["ymax"] + (expand_percent * lat_range)
      ), crs = latlong_crs)

      latlongbbox_transformed <- sf::st_transform(sf::st_as_sfc(expanded_latlong_bbox),
                                                  crs = "ESRI:54012")
      # Crop the world map in lat/long to the expanded extent
      surrounding_countries_latlong <- map_data_sf %>%
        sf::st_make_valid() %>%
        sf::st_transform(crs = "ESRI:54012") %>%
        dplyr::group_by(scalerank, featurecla) %>%
        dplyr::summarize(geometry = sf::st_crop(geometry, latlongbbox_transformed)) %>%
        dplyr::filter(!sf::st_is_empty(geometry)) %>% # Filter out empty geometries
        sf::st_make_valid() %>%
        sf::st_transform(crs = sf::st_crs(expanded_latlong_bbox))

      layer_list <- list()
      if(!is.null(layers)) {
        for (i in 1:length(layers)) {
          layer_data <- add_NE_layer(layers[i],
                                     scale,
                                     expanded_latlong_bbox)
          # Project the layer
          layer_data <- sf::st_transform(layer_data,
                                         crs = projection)

          if (!is.null(layer_data) && nrow(layer_data) > 0) {
            layer_list[[i]] <- layer_data
            names(layer_list)[[i]] <- layers[i]
          }
        }
      }
    } else {

      # Project latlong extent for cropping
      latlong_extent_transformed <- sf::st_transform(sf::st_as_sfc(latlong_extent),
                                                     crs = "ESRI:54012")

      # Crop to the original extent if not expanding
      surrounding_countries_latlong <- map_data_sf %>%
        sf::st_make_valid() %>%
        sf::st_transform(crs = "ESRI:54012") %>%
        dplyr::group_by(scalerank, featurecla) %>%
        dplyr::summarize(geometry = sf::st_crop(geometry, latlong_extent_transformed)) %>%
        dplyr::filter(!sf::st_is_empty(geometry)) %>% # Filter out empty geometries
        sf::st_make_valid() %>%
        sf::st_transform(crs = sf::st_crs(latlong_extent))

      layer_list <- list()
      if (!is.null(layers)) {
        for (i in 1:length(layers)) {
          layer_data <- add_NE_layer(layers[i],
                                     scale,
                                     latlong_extent)
          # Project the layer
          layer_data <- sf::st_transform(layer_data,
                                         crs = projection)

          if (!is.null(layer_data) && nrow(layer_data) > 0) {
            layer_list[[i]] <- layer_data
            names(layer_list)[[i]] <- layers[i]
          }
        }
      }
    }

    # Transform the cropped surrounding countries to the UTM CRS
    surrounding_countries_utm <-
      sf::st_transform(surrounding_countries_latlong, crs = utm_crs)

    map_surround <- surrounding_countries_utm

  } else {

    map_surround <- sf::st_transform(map_surround, crs = projection) %>%
      sf::st_make_valid()

    # If crop to grid is set to FALSE
    if (!crop_to_grid && is.null(xlims) && is.null(ylims)) {
      # Define percentage for expansion
      expand_percent <- 0.1  # 10% expansion

      # Calculate the amount to expand
      x_range <- map_lims["xmax"] - map_lims["xmin"]
      y_range <- map_lims["ymax"] - map_lims["ymin"]

      # Compute new expanded limits
      expanded_lims <- sf::st_as_sfc(sf::st_bbox(c(
        map_lims["xmin"] - (expand_percent * x_range),
        map_lims["ymin"] - (expand_percent * y_range),
        map_lims["xmax"] + (expand_percent * x_range),
        map_lims["ymax"] + (expand_percent * y_range)
      )))
    } else {
      expanded_lims <- map_lims
    }

      # Get bounding box with expanded limits
      bbox <- sf::st_as_sfc(sf::st_bbox(expanded_lims)) %>%
        sf::st_set_crs(., sf::st_crs(map_surround))
      bbox_lims <- sf::st_bbox(bbox)

      layer_list <- list()
      if (!is.null(layers)) {
        for (i in 1:length(layers)) {
          layer_data <- add_NE_layer(layers[i],
                                     scale,
                                     bbox_lims)

          if (!is.null(layer_data) && nrow(layer_data) > 0) {
            layer_list[[i]] <- layer_data
            names(layer_list)[[i]] <- layers[i]
          }
        }
      }

    sf::st_agr(map_surround) = "constant"
    bbox_t <- sf::st_transform(bbox, crs = "ESRI:54012")

    map_surround <- sf::st_transform(map_surround, crs = "ESRI:54012") %>%
      sf::st_intersection(bbox_t) %>%
      sf::st_transform(crs = projection)
  }
  map_data_list <- list(
    map_surround = map_surround,
    layer_list = layer_list
  )
  return(map_data_list)
}
