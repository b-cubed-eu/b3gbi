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

  map_data <- download_ne_data(region = region,
                               level = level,
                               ne_scale = ne_scale,
                               ne_type = ne_type)

  # Project and validate the map
  map_data_projected <-
    map_data %>%
    sf::st_transform(crs = "ESRI:54012") %>%
    sf::st_make_valid()

  if (level == "cube") {

    expand_percent <- 0.5 # 10% buffer
    lng_range <- unname(latlong_bbox["xmax"] - latlong_bbox["xmin"])
    lat_range <- unname(latlong_bbox["ymax"] - latlong_bbox["ymin"])

    # Find bounding box
    min_lon <- unname(latlong_bbox["xmin"] - (expand_percent * lng_range))
    max_lon <- unname(latlong_bbox["xmax"] + (expand_percent * lng_range))
    min_lat <- unname(latlong_bbox["ymin"] - (expand_percent * lat_range))
    max_lat <- unname(latlong_bbox["ymax"] + (expand_percent * lat_range))

    latlong_extent <- c("xmin" = min_lon,
                        "xmax" = max_lon,
                        "ymin" = min_lat,
                        "ymax" = max_lat)

    # Project the extent
    extent_projected <-
      sf::st_bbox(latlong_extent, crs = "EPSG:4326") %>%
      sf::st_as_sfc() %>%
      sf::st_transform(crs = "ESRI:54012")

    # Set attributes as spatially constant to avoid warnings when clipping
    sf::st_agr(map_data_projected) <- "constant"

    # Crop the world map
    map_data_projected <- sf::st_crop(map_data_projected,
                                      extent_projected) %>%
      sf::st_make_valid() %>%
      sf::st_union() %>%
      sf::st_as_sf()

    extent_projected <- sf::st_bbox(map_data_projected)

  } else {

    extent_projected <- sf::st_bbox(map_data_projected)

    map_data_projected <- sf::st_make_valid(map_data_projected) %>%
      sf::st_union() %>%
      sf::st_as_sf()

  }

  if (include_ocean == TRUE) {

    extent_projected_polygon <- sf::st_as_sfc(extent_projected)

    map_data_ocean <- sf::st_difference(extent_projected_polygon,
                                        map_data_projected)

    # Validate oceans
    map_data_ocean <- map_data_ocean %>%
      sanitize_geometries() %>%
      sf::st_make_valid()

    # Remove empty geometries
    map_data_projected <- map_data_projected %>%
      sanitize_geometries() %>%
      sf::st_make_valid() %>%
      dplyr::filter(!is.na(sf::st_geometry(.))) %>%
      dplyr::filter(!sf::st_is_empty(.))

    # # Check if the land data is empty before performing the union
    # if (nrow(map_data_projected) == 0 || include_land == FALSE) {
    #   # If there's no land in the bounding box, return just the ocean layer
    #   map_data_projected <- map_data_ocean
  #  } else {
      # Otherwise, perform the union as normal
      map_data_ocean_ <- sf::st_as_sf(map_data_ocean)
      # map_data_projected <- bind_rows(map_data_projected,
      #                                 map_data_ocean_)
      # map_data_projected <- map_data_projected %>%
      #   dplyr::group_by() %>%
      #   dplyr::reframe(geometry = sf::st_union(x)) %>%
      #   dplyr::ungroup() %>%
      #   sf::st_as_sf()

      # convert to the desired projected CRS
      map_data_ocean_projected <- sf::st_transform(map_data_ocean_,
                                                   crs = projected_crs)

  #  }

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

    map_data_ocean <- sf::st_buffer(map_data_projected,
                                    dist = buffer_dist_km * 1000)

    # Validate oceans
    map_data_ocean_ <- sf::st_make_valid(map_data_ocean)

    # convert to the desired projected CRS
    map_data_ocean_projected <- sf::st_transform(map_data_ocean_,
                                                 crs = projected_crs)

    # Merge the land with the oceans
    # map_data_projected <- sf::st_union(map_data_projected,
    #                                    map_data_ocean_)

  } else {

    map_data_ocean_projected <- NULL

  }

  # convert to the desired projected CRS
  map_data_projected <- sf::st_transform(map_data_projected,
                                         crs = projected_crs)

  return(list(land = map_data_projected, ocean = map_data_ocean_projected))

}
