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
                        buffer_dist_km = NULL) {
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

  # store s2 setting for use later
  original_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(original_s2)))

  # During testing, return a fast dummy instead of downloading NE data.
  # This avoids 30+ minute test runs from Natural Earth downloads.
  # The real function is tested in test-get_NE_data.R which unsets this env var.
  if (isTRUE(as.logical(Sys.getenv("B3GBI_TESTING")))) {
    if (!is.null(latlong_bbox)) {
      bbox_poly <- sf::st_as_sfc(sf::st_bbox(latlong_bbox, crs = 4326))
    } else {
      bbox_poly <- sf::st_as_sfc(
        sf::st_bbox(c(xmin = -180, ymin = -90, xmax = 180, ymax = 90), crs = 4326)
      )
    }
    bbox_proj <- tryCatch(
      sf::st_transform(bbox_poly, projected_crs),
      error = function(e) bbox_poly
    )
    return(list(
      combined = sf::st_sf(geometry = bbox_proj),
      saved = sf::st_sf(geometry = bbox_proj)
    ))
  }

  # Download the map data
  map_data <-
    download_ne_data(
      region = region,
      level = level,
      ne_scale = ne_scale,
      ne_type = ne_type
    ) %>%
    sf::st_make_valid()

  # Add a buffer around the bbox to ensure full coverage
  expand_percent <- 0.5 # 50% buffer
  lng_range <- unname(latlong_bbox["xmax"] - latlong_bbox["xmin"])
  lat_range <- unname(latlong_bbox["ymax"] - latlong_bbox["ymin"])

  expand_lng <- max(expand_percent * lng_range, 0.1)
  expand_lat <- max(expand_percent * lat_range, 0.1)

  # cap lat/long to valid ranges
  min_lon <- max(unname(latlong_bbox["xmin"] - expand_lng), -180)
  max_lon <- min(unname(latlong_bbox["xmax"] + expand_lng), 180)
  min_lat <- max(unname(latlong_bbox["ymin"] - expand_lat), -90)
  max_lat <- min(unname(latlong_bbox["ymax"] + expand_lat), 90)

  # Create a bbox object
  latlong_extent <- c(
    "xmin" = min_lon,
    "xmax" = max_lon,
    "ymin" = min_lat,
    "ymax" = max_lat
  ) %>%
    sf::st_bbox(crs = sf::st_crs(4326))
  latlong_extent_sfc <- sf::st_as_sfc(latlong_extent)

  # Check intersection in 4326 (spherical) which is more robust
  # For 'cube' level, we crop the map in 4326 first to avoid projection artifacts
  if (level == "cube") {
    # Crop in 4326
    # Indicate attributes are constant to prevent st_crop warnings
    sf::st_agr(map_data) <- "constant"

    suppressMessages(sf::sf_use_s2(FALSE))

    map_data <- tryCatch(
      map_data %>%
        sf::st_crop(latlong_extent) %>%
        sf::st_make_valid(),
      error = function(e) {
        # st_crop can fail on invalid geometries; fall back to uncropped
        map_data
      }
    )
    suppressMessages(sf::sf_use_s2(original_s2))

    has_intersection <- nrow(map_data) > 0
  } else {
    has_intersection <- any(suppressMessages(
      sf::st_intersects(map_data, latlong_extent_sfc, sparse = FALSE))
    )
  }

  # Project the (possibly cropped) map
  map_data_projected <-
    map_data %>%
    sf::st_transform(crs = "ESRI:54012") %>%
    sf::st_make_valid()

  # Project the extent
  cube_extent_projected <- latlong_extent_sfc %>%
    sf::st_transform(crs = "ESRI:54012")

  if (level == "cube") {
    # Set attributes as spatially constant to avoid warnings when clipping
    sf::st_agr(map_data_projected) <- "constant"

    # Union the world map to avoid issues with overlaps
    if (has_intersection) {
      map_data_projected <- map_data_projected %>%
        sf::st_union() %>%
        sf::st_cast("POLYGON", warn = FALSE) %>%
        sf::st_as_sf()
    } else {
      # If no intersection, return an empty sf object with same structure
      map_data_projected <- sf::st_as_sf(sf::st_sfc(), crs = "ESRI:54012")
    }

    # Get the bbox of the cropped map data
    map_bbox_projected <- sf::st_bbox(map_data_projected)
  } else {
    # Validate and union the map data to avoid issues with overlaps
    map_data_projected <- sf::st_make_valid(map_data_projected) %>%
      sf::st_union() %>%
      sf::st_as_sf()

    # Use the full extent of the projected map data
    map_bbox_projected <- sf::st_bbox(map_data_projected)
  }

  # Create a polygon from the extent
  if (is_sf_empty(map_data_projected) || level == "cube") {
    # If land is empty (e.g. all in ocean), use the buffered latlong extent as the total extent
    extent_projected_polygon <- cube_extent_projected
  } else {
    extent_projected_polygon <- sf::st_as_sfc(map_bbox_projected)
  }

  # Create ocean layer by subtracting land from the extent
  # Disable s2 to avoid topology errors with complex geometries
  suppressMessages(sf::sf_use_s2(FALSE))

  map_data_ocean <- sf::st_difference(
    extent_projected_polygon,
    map_data_projected
  ) %>%
    sf::st_make_valid()
    suppressMessages(sf::sf_use_s2(original_s2))

  # Save the new layer for later use before removing unwanted land
  map_data_save <- map_data_ocean

  if (include_ocean == TRUE) {
    if (level != "cube") {
      # Remove land outside of the users chosen region from the ocean layer
      extra_land_data <- download_ne_data(
        region = NULL,
        level = "cube",
        ne_scale = ne_scale,
        ne_type = ne_type
      ) %>%
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
        dist = buffer_dist_km * 1000
      ) %>%
        sf::st_make_valid()
    } else {
      map_data_combined <- map_data_projected # Still empty
    }
  } else {
    # just use the land layer
    # Buffer slightly to capture coastal grid cells that may not perfectly
    # intersect simplified Natural Earth coastlines
    if (!is_sf_empty(map_data_projected)) {
      map_data_combined <- sf::st_buffer(map_data_projected, dist = 100) %>% # 100m buffer
        sf::st_make_valid()
    } else {
      map_data_combined <- map_data_projected
    }
  }

  if (include_land == FALSE) {
    # just use the ocean layer — ensure it's an sf object, not sfc, and valid
    map_data_combined <- sf::st_as_sf(map_data_ocean) %>%
      sf::st_make_valid()
  }

  if (level != "cube") {
    # Added defensive check before final cropping
    final_extent_sfc <- if (is_sf_empty(map_data_projected)) {
      cube_extent_projected
    } else {
      sf::st_as_sfc(map_bbox_projected)
    }
    if (any(sf::st_intersects(map_data_combined, final_extent_sfc, sparse = FALSE))) {
      # Indicate attributes are constant to prevent st_crop warnings
      sf::st_agr(map_data_combined) <- "constant"
      map_data_combined <- map_data_combined %>%
        sf::st_crop(final_extent_sfc) %>%
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

  return(list(
    combined = map_data_combined,
    saved = map_data_save
  ))
}
