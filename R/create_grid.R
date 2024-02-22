#' @noRd
create_grid <- function(map_data, cs1, cs2) {

  # Make a grid across the map area
  grid <- map_data %>%
    sf::st_make_grid(cellsize = c(cs1 * 1000, cs2 * 1000)) %>%
    sf::st_intersection(map_data) %>%
    sf::st_cast("MULTIPOLYGON") %>%
    sf::st_sf() %>%
    dplyr::mutate(cellid = row_number())

  # Add area column to grid
  grid$area_km2 <-
    grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  return(grid)

}

