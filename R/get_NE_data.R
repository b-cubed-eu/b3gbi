# Retrieve map data from rnaturalearth
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

