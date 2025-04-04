test_that("prepare_spatial_data correctly assigns cell IDs", {
  # Create sample data
  data <- data.frame(
    xcoord = c(1, 2, 3, 4),
    ycoord = c(1, 2, 3, 4),
    species = c("A", "B", "C", "D")
  )

  # Create sample grid
  grid <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(0, 0, 0, 5, 5, 5, 5, 0, 0, 0),
            ncol = 2, byrow = TRUE
          )
        )
      ),
      sf::st_polygon(
        list(
          matrix(
            c(5, 5, 5, 10, 10, 10, 10, 5, 5, 5),
            ncol = 2, byrow = TRUE
          )
        )
      )
    ),
    cellid = 1:2
  )

  # Set grid CRS
  grid1 <- sf::st_set_crs(grid, 4326)

  # Test with same CRS
  cube_crs <- "EPSG:4326"
  output_crs <- "EPSG:4326"
  result <- prepare_spatial_data(data, grid1, cube_crs, output_crs)
  expect_true("cellid" %in% names(result))
  expect_equal(result$cellid, c(1, 1, 1, 1))

  # Test with different CRS
  cube_crs2 <- "EPSG:3857"
  output_crs2 <- "EPSG:3857"
  grid2 <- sf::st_set_crs(grid, 3857)
  result2 <- prepare_spatial_data(data, grid2, cube_crs2, output_crs2)
  expect_true("cellid" %in% names(result2))

})

test_that("prepare_spatial_data handles edge cases", {
  # Create sample data with realistic coordinates for EPSG:4326
  data <- data.frame(
    xcoord = c(1, 2, 3, 4), # Longitude
    ycoord = c(49, 50, 51, 52), # Latitude
    species = c("A", "B", "C", "D")
  )

  # Create sample grid
  grid <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(0, 48, 0, 53, 5, 53, 5, 48, 0, 48),
            ncol = 2, byrow = TRUE
          )
        )
      ),
      sf::st_polygon(
        list(
          matrix(
            c(5, 48, 5, 53, 10, 53, 10, 48, 5, 48),
            ncol = 2, byrow = TRUE
          )
        )
      )
    ),
    cellid = 1:2
  )

  # Set grid CRS
  grid <- sf::st_set_crs(grid, 3857)

  cube_crs <- "EPSG:3857"
  output_crs <- "EPSG:3857"
  result <- prepare_spatial_data(data, grid, cube_crs, output_crs)
  expect_true("cellid" %in% names(result))
  expect_equal(result$cellid, c(1, 1, 1, 1))

  # Test with data outside the grid
  data_outside <- data.frame(
    xcoord = c(100, 200, 300, 400),
    ycoord = c(100, 200, 300, 400),
    species = c("A", "B", "C", "D")
  )
  expect_error(
    prepare_spatial_data(data_outside, grid, cube_crs, output_crs),
    "No spatial intersection between occurrence data and grid."
  )

  # Test with an empty grid
  grid_empty <- sf::st_sf(geometry = sf::st_sfc(), cellid = integer(0))
  grid_empty <- sf::st_set_crs(grid_empty, 3857)
  expect_error(
    prepare_spatial_data(data, grid_empty, cube_crs, output_crs),
    "No spatial intersection between occurrence data and grid."
  )
})

test_that("prepare_spatial_data handles errors", {
  # Create sample data
  data <- data.frame(
    xcoord = c(1, 2, 3, 4),
    ycoord = c(1, 2, 3, 4),
    species = c("A", "B", "C", "D")
  )

  # Create sample grid
  grid <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(0, 0, 0, 5, 5, 5, 5, 0, 0, 0),
            ncol = 2, byrow = TRUE
          )
        )
      ),
      sf::st_polygon(
        list(
          matrix(
            c(5, 5, 5, 10, 10, 10, 10, 5, 5, 5),
            ncol = 2, byrow = TRUE
          )
        )
      )
    ),
    cellid = 1:2
  )

  cube_crs <- "EPSG:4326"
  output_crs <- "EPSG:4326"

  # Test with invalid data input
  expect_error(
    prepare_spatial_data(list(1, 2, 3), grid, cube_crs, output_crs)
  )

  # Test with invalid grid input
  expect_error(
    prepare_spatial_data(data, data.frame(a = 1:2, b=3:4), cube_crs, output_crs)
  )
})
