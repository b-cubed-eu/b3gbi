#' Retrieve Map Data from rnaturalearth
#'
#' Downloads and prepares map data from the rnaturalearth package at
#' different geographic scales (country, continent, or world).
#'
#' @param projected_crs The projected CRS to convert the cropped map to.
#' @param latlong_bbox A bbox object in latitude and longitude ("EPSG:4326") to
#'  use for cropping the downloaded map data (required if level = 'cube').
#' @param region (Optional) The specific region to retrieve data for (required
#'  unless level = 'cube' or 'world').
#' @param level (Optional) The desired geographic scale: 'country', 'continent',
#'  'geounit', 'sovereignty', 'world', or 'cube'. Cube refers to the geographic
#'  extent of the data cube.
#' @param ne_type (Optional) The type of Natural Earth data to download:
#'  'countries', 'map_units', 'sovereignty', or 'tiny_countries'. (Default:
#'  'countries')
#' @param ne_scale (Optional) The scale of Natural Earth data to download:
#'  'small' - 110m, 'medium' - 50m, or 'large' - 10m. (Default: 'medium')
#' @param include_ocean (Optional) Include oceans if TRUE. Set to
#'  "buffered_coast" to instead create a buffer around the land area. If set to
#'  FALSE occurrences that fall outside of land boundaries will not be included
#'  in the analysis.
#' @param buffer_dist_km (Optional) Distance to buffer around the land if you
#'  choose "buffered_coast" for the include_ocean parameter.
#' @return An sf object containing the map data, transformed to the
#'  appropriate projection.
#'
#' @noRd
get_ne_data <- function(projected_crs,
                        latlong_bbox = NULL,
                        region = NULL,
                        level = "cube",
                        ne_type = "countries",
                        ne_scale = "medium",
                        include_land = TRUE,
                        include_ocean = TRUE,
                        buffer_dist_km = NULL
) {

  x <- . <- NULL

  if (ne_scale == "large" && ne_type == "tiny_countries") {
    stop("tiny_countries are only available for medium (50 km) or small (110 km)
          scale maps")
  }

  if (level == "cube" && is.null(latlong_bbox)) {
    stop("A bounding box in EPSG:4326 (latitude and longitude) is required when
         level = 'cube'.")
  }

  if (!(level == "cube" || level == "world") && is.null(region)) {
    stop("You must provide a region unless level is set to 'cube' or 'world'.")
  }

  if (is.null(projected_crs)) {
    stop("No projected CRS provided.")
  }

  # Download the map data
  map_data <- download_ne_data(region = region,
                               level = level,
                               ne_scale = ne_scale,
                               ne_type = ne_type)

  # Project and validate the map
  map_data_projected <-
    map_data %>%
    sf::st_transform(crs = "ESRI:54012") %>%
    sf::st_make_valid()

  # Add a buffer around the bbox to ensure full coverage
  expand_percent <- 0.5 # 50% buffer
  lng_range <- unname(latlong_bbox["xmax"] - latlong_bbox["xmin"])
  lat_range <- unname(latlong_bbox["ymax"] - latlong_bbox["ymin"])
  min_lon <- unname(latlong_bbox["xmin"] - (expand_percent * lng_range))
  max_lon <- unname(latlong_bbox["xmax"] + (expand_percent * lng_range))
  min_lat <- unname(latlong_bbox["ymin"] - (expand_percent * lat_range))
  max_lat <- unname(latlong_bbox["ymax"] + (expand_percent * lat_range))

  # Create a bbox object
  latlong_extent <- c("xmin" = min_lon,
                      "xmax" = max_lon,
                      "ymin" = min_lat,
                      "ymax" = max_lat) %>%
    sf::st_bbox(crs = sf::st_crs(4326))

  # Project the extent
  extent_projected <- latlong_extent %>%
    sf::st_as_sfc() %>%
    sf::st_transform(crs = "ESRI:54012")

  if (level == "cube") {

    # Set attributes as spatially constant to avoid warnings when clipping
    sf::st_agr(map_data_projected) <- "constant"

    # Crop and validate and union the world map to avoid issues with overlaps
    # Added defensive check before cropping to prevent empty results for non-contiguous regions
    # Ensure extent_projected is sfc for st_intersects
    extent_sfc <- if (inherits(extent_projected, "bbox")) {
      sf::st_as_sfc(extent_projected)
    } else {
      extent_projected
    }

    if (any(sf::st_intersects(map_data_projected, extent_sfc, sparse = FALSE))) {
      map_data_projected <- map_data_projected %>%
        sf::st_crop(extent_projected) %>%
        sf::st_make_valid() %>%
        sf::st_union() %>%
        sf::st_as_sf()
    } else {
      # If no intersection, return an empty sf object with same structure
      # This matches the behavior of st_crop but avoids its potential errors
      map_data_projected <- sf::st_as_sf(sf::st_sfc(), crs = "ESRI:54012")
    }

    # Get the bbox of the cropped map data
    extent_projected <- sf::st_bbox(map_data_projected)

  } else {

    # Validate and union the map data to avoid issues with overlaps
    map_data_projected <- sf::st_make_valid(map_data_projected) %>%

      sf::st_union() %>%
      sf::st_as_sf()

    # Use the full extent of the projected map data
    extent_projected <- sf::st_bbox(map_data_projected)

  }

  # Create a polygon from the extent
  if (is_sf_empty(map_data_projected)) {
    # If land is empty (e.g. all in ocean), use the buffered latlong extent as the total extent
    # ensure it's converted to sfc if it was a bbox
    extent_projected_polygon <- if (inherits(extent_projected, "bbox")) {
      sf::st_as_sfc(extent_projected)
    } else {
      extent_projected
    }
  } else {
    extent_projected_polygon <- if (inherits(extent_projected, "bbox")) {
      sf::st_as_sfc(extent_projected)
    } else {
      extent_projected
    }
  }

  # Create ocean layer by subtracting land from the extent
  map_data_ocean <- sf::st_difference(extent_projected_polygon,
                                      map_data_projected)

  # Save the new layer for later use before removing unwanted land
  map_data_save <- map_data_ocean

  if (include_ocean == TRUE) {

    if (level != "cube") {
      # Remove land outside of the users chosen region from the ocean layer
      extra_land_data <- download_ne_data(region = NULL,
                                          level = "cube",
                                          ne_scale = ne_scale,
                                          ne_type = ne_type) %>%
        sf::st_transform(crs = "ESRI:54012") %>%
        sf::st_make_valid() %>%
        sf::st_union() %>%
        sf::st_as_sf()
      unwanted_land <- sf::st_intersection(map_data_save, extra_land_data)
      map_data_ocean <- sf::st_difference(map_data_save, unwanted_land)

    }

    # Merge the land with the oceans
    map_data_combined <- union_helper(map_data_projected, map_data_ocean) %>%
      sf::st_as_sf()

  } else if (is.character(include_ocean) &&
             include_ocean == "buffered_coast") {

    message(
      paste0(
        "Buffering land by ",
        buffer_dist_km,
        " km to include coastal water areas."
      )
    )

    if (!level %in% c("country", "sovereignty", "geounit") ||
        length(region) > 1) {
      warning(
        paste0(
          "Buffering the coastal areas may produce unexpected results when ",
          "used with level = 'cube' (default) or with areas encompassing ",
          "multiple countries, as it may create overlapping buffers or ",
          "buffer areas that you did not intend. These undesirable effects ",
          "might be mitigated by reducing the buffer distance and/or ",
          "setting visible_gridlines = FALSE when plotting."
        )
      )
    }

    # Create a buffer around the land
    if (!is_sf_empty(map_data_projected)) {
      map_data_combined <- sf::st_buffer(map_data_projected,
                                         dist = buffer_dist_km * 1000) %>%
        sf::st_make_valid()
    } else {
      map_data_combined <- map_data_projected # Still empty
    }

  } else {

    # just use the land layer
    map_data_combined <- map_data_projected

  }

  if (include_land == FALSE) {
    # just use the ocean layer
    map_data_combined <- map_data_ocean
  }

  if (level != "cube") {
    # Added defensive check before final cropping
    extent_sfc <- if (inherits(extent_projected, "bbox")) {
      sf::st_as_sfc(extent_projected)
    } else {
      extent_projected
    }

    if (any(sf::st_intersects(map_data_combined, extent_sfc, sparse = FALSE))) {
      map_data_combined <- map_data_combined %>%
        sf::st_crop(extent_projected) %>%
        sf::st_make_valid() %>%
        sf::st_union() %>%
        sf::st_as_sf()
    } else {
       # Should only happen if user region is outside world or something extreme
       map_data_combined <- sf::st_as_sf(sf::st_sfc(), crs = "ESRI:54012")
    }
  }

  # convert to the desired projected CRS
  map_data_combined <- sf::st_transform(map_data_combined, crs = projected_crs)

  # also convert the saved layer
  map_data_save <- sf::st_union(map_data_save, map_data_projected) %>%
    sf::st_transform(crs = projected_crs) %>%
    sf::st_as_sf()

  return(list(combined = map_data_combined,
              saved = map_data_save))

}