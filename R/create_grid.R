#' Create a Spatial Grid for Mapping
#'
#' Generates a grid of polygons covering a specified geographic area,
#' suitable for mapping data retrieved with the rnaturalearth package.
#'
#' @param map_data A spatial object (e.g., an sf object) representing the
#'   geographic area of interest.  Obtained from rnaturalearth.
#' @param cs1 Cell length in kilometers.
#' @param cs2 Cell width in kilometers.
#' @return An sf object containing the grid polygons, with attributes:
#'   * `cellid`: A unique ID for each grid cell.
#'   * `area_km2`: Area of each grid cell in square kilometers.
#'
#' @examples
#' # Get some map data
#' germany_map <- rnaturalearth::ne_countries(country = "Germany", scale = "medium", returnclass = "sf")
#' # Change projection to EPSG:3035 (works well with metric grid size)
#' germany_map <- sf::st_transform(germany_map, crs = "EPSG:3035")
#' # Calculate a 100km x 100km grid and plot it
#' germany_grid <- create_grid(germany_map, cs1 = 100, cs2 = 100)
#' plot(Germany_grid)
#' @noRd
create_grid <- function(map_data,
                        level,
                        cell_size = NULL) {

  if (!is.null(cell_size)) {

    cell_size <- round(cell_size)

  } else {

    cell_size <- ifelse(level == "world", 100,
                        ifelse(level == "continent", 100,
                               ifelse(level == "country", 10)))

  }

  # Make a grid across the map area
  grid <- map_data %>%
    sf::st_make_grid(cellsize = c(cell_size * 1000, cell_size * 1000)) %>%
    sf::st_intersection(map_data) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_sf() %>%
    dplyr::mutate(cellid = dplyr::row_number())

  # Add area column to grid
  grid$area_km2 <-
    grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  return(grid)

}

