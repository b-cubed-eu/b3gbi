#' Create a Spatial Grid for Mapping
#'
#' Generates a grid of polygons covering a specified geographic area,
#' suitable for mapping data retrieved with the rnaturalearth package.
#'
#' @param bbox An sf bbox object provided by compute_indicate_workflow.
#' @param cell_size Cell length in kilometers.
# #' @param grid_units Cell size unit type: "km" or "degrees".
#' @param projected_crs The projected CRS the grid should be converted to.
#' @param make_valid (Optional) Run sf::st_make_valid() on grid. TRUE or FALSE.
#' @return An sf object containing the grid polygons, with attributes:
#'   * `cellid`: A unique ID for each grid cell.
#'   * `area`: Area of each grid cell.
#'
#' @examples
#' # Get some map data
#' germany_map <- rnaturalearth::ne_countries(country = "Germany",
#'                                            scale = "medium",
#'                                            returnclass = "sf")
#' # Change projection to EPSG:3035 (works well with metric grid size)
#' germany_map <- sf::st_transform(germany_map,
#'                                 crs = "EPSG:3035")
#' # Calculate a 100km x 100km grid and plot it
#' germany_grid <- create_grid(germany_map,
#'                             cell_size = 10)
#' plot(germany_grid)
#' @noRd
create_grid <- function(bbox,
                        cell_size,
                        projected_crs,
                        make_valid = FALSE) {

  offset_x <- bbox["xmin"]
  offset_y <- bbox["ymin"]

  # Make a grid across the cube
  grid <- bbox %>%
    sf::st_make_grid(cellsize = c(cell_size, cell_size),
                     offset = c(offset_x, offset_y)) %>%
    sf::st_sf() %>%
    dplyr::mutate(cellid = dplyr::row_number())

  # Validate grid if make_valid set to TRUE
  if (make_valid == TRUE) {
    grid <- sf::st_make_valid(grid)
  }

  # Transform grid to projected crs
  grid <- sf::st_transform(grid, projected_crs)

  # Add area column to grid
  grid$area <-
    grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  return(grid)

}
