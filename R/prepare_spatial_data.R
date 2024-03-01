#' @noRd
prepare_spatial_data <- function(data, grid) {

  # Set map limits
  # map_lims <- sf::st_buffer(grid, dist = 1000) %>%
  #   sf::st_bbox()

  # Scale coordinates of occurrences so the number of digits matches map
   data <-
     data %>%
     dplyr::mutate(xcoord = xcoord * 1000,
                   ycoord = ycoord * 1000)

 # data[, xcoord := xcoord * 1000][, ycoord := ycoord * 1000]

  # Convert the x and y columns to the correct format for plotting with sf
  # occ_sf <- sf::st_as_sf(data_scaled,
  #                        coords = c("xcoord", "ycoord"),
  #                        crs = "EPSG:3035")

  occ_sf <- sf::st_as_sf(data,
                         coords = c("xcoord", "ycoord"),
                         crs = "EPSG:3035")

  # Set attributes as spatially constant to avoid warnings
  sf::st_agr(grid) <- "constant"
  sf::st_agr(occ_sf) <- "constant"

  # Calculate intersection between occurrences and grid cells
  occ_grid_int <- sf::st_intersection(occ_sf, grid, left = TRUE)

  # Add cell numbers to occurrence data
  # data <-
  #   data_scaled %>%
  #   dplyr::inner_join(occ_grid_int) %>%
  #   suppressMessages() %>%
  #   dplyr::arrange(cellid)

  data <-
    data %>%
    dplyr::inner_join(occ_grid_int) %>%
    suppressMessages() %>%
    dplyr::arrange(cellid)

  #
  # # Remove grid cells with areas smaller than 20% of the largest one
  # grid <-
  #   grid %>%
  #   filter(area_km2 > 0.2 * max(area_km2))
  #
  # # Remove same grid cells from data
  # data <-
  #   data %>%
  #   filter(cellid %in% grid$cellid)

  return(data)

}

