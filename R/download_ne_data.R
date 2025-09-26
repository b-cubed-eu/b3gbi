#' @noRd
download_ne_data <- function(region = NULL,
                             level = "country",
                             ne_scale = "medium",
                             ne_type = "countries") {

  # Download and prepare Natural Earth map data
  if (level == "country") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            country = region,
                                            type = ne_type,
                                            returnclass = "sf")

  } else if (level == "continent") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            continent = region,
                                            returnclass = "sf")

  } else if (level == "sovereignty") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            type = ne_type,
                                            sovereignty = region,
                                            returnclass = "sf")

  } else if (level == "geounit") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            type = ne_type,
                                            geounit = region,
                                            returnclass = "sf")

  } else if (level == "world" || level == "cube") {

    map_data <- rnaturalearth::ne_countries(scale = ne_scale,
                                            returnclass = "sf")

  } else {
    stop("Level not recognized. Must be country, continent, geounit,
         sovereignty, world, or cube.")
  }
}
