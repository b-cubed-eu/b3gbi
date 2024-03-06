#' Retrieve Map Data from rnaturalearth
#'
#' Downloads and prepares map data from the rnaturalearth package at
#' different geographic scales (country, continent, or world).
#'
#' @param level  The desired geographic scale: "country", "continent", or "world".
#' @param region  The specific region to retrieve data for (required when
#'  `level = "country"` or `level = "continent"`).
#' @return An sf object containing the map data, transformed to the
#'   EPSG:3035 projection.
#'
#' @examples
#' # Download country-level data for France
#' france_map <- get_NE_data(level = "country", region = "France")
#'
#' # Get continent-level data for Africa
#' africa_map <- get_NE_data(level = "continent", region = "Africa")
#'
#' # Retrieve a map of the entire world
#' world_map <- get_NE_data(level = "world")
#' @noRd
get_NE_data <- function(level, region) {

  # Download and prepare Natural Earth map data
  if (level == "country") {

    map_data <- rnaturalearth::ne_countries(scale = "medium",
                                            country = region,
                                            returnclass = "sf") %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = "EPSG:3035")

  } else if (level == "continent") {

    map_data <- rnaturalearth::ne_countries(scale = "medium",
                                            continent = region,
                                            returnclass = "sf") %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = "EPSG:3035")

  } else if (level == "world") {

    map_data <- rnaturalearth::ne_countries(scale = "medium",
                                            returnclass = "sf") %>%
      sf::st_as_sf() %>%
      sf::st_transform(crs = "EPSG:3035")

  }

  return(map_data)

}

