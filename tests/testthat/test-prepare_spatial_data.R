test_that("prepare_spatial_data correctly assigns cell IDs", {
  # Create sample data frame
  data_df <- data.frame(
    xcoord = c(1, 2, 3, 4), # Longitude
    ycoord = c(49, 50, 51, 52), # Latitude
    species = c("A", "B", "C", "D")
  )

  # Create sample grid (as before)
  grid <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(0, 48, 0, 53, 5, 53, 5, 48, 0, 48), # Adjusted for overlap in 3857
            ncol = 2, byrow = TRUE
          )
        )
      ),
      sf::st_polygon(
        list(
          matrix(
            c(5, 48, 5, 53, 10, 53, 10, 48, 5, 48), # Adjusted for overlap in 3857
            ncol = 2, byrow = TRUE
          )
        )
      )
    ),
    cellid = 1:2,
    crs = 4326 # Explicitly set CRS here
  )


  # Set grid CRS
  grid1 <- sf::st_set_crs(grid, 4326)

  # **Crucial Change:** Convert data_df to sf object *with* CRS
  data_sf1 <- sf::st_as_sf(data_df, coords = c("xcoord", "ycoord"), crs = 4326)

  # Test with same CRS
  cube_crs <- "EPSG:4326"
  output_crs <- "EPSG:4326"
  result <- prepare_spatial_data(data_df, data_sf1, grid1, cube_crs, output_crs) # Pass the sf object
  expect_true("cellid" %in% names(result))
  expect_equal(result$cellid, c(1, 1, 1, 1))

  # Test with different CRS
  cube_crs2 <- "EPSG:3857"
  output_crs2 <- "EPSG:3857"
  grid2 <- sf::st_transform(grid, 3857)
  # **Crucial Change:** Convert data_df to sf object *with* CRS and the correct CRS
  data_sf2 <- sf::st_as_sf(data_df, coords = c("xcoord", "ycoord"), crs = 4326) %>% #important to set the correct crs
    sf::st_transform(crs = 3857)
  result2 <- prepare_spatial_data(data_df, data_sf2, grid2, cube_crs2, output_crs2) # Pass the sf object
  expect_true("cellid" %in% names(result2))
})

test_that("prepare_spatial_data handles edge cases", {
  # Create sample data frame with realistic coordinates for EPSG:4326
  data_df <- data.frame(
    xcoord = c(1, 2, 3, 4), # Longitude
    ycoord = c(49, 50, 51, 52), # Latitude
    species = c("A", "B", "C", "D")
  )

  # Create sample grid *explicitly in EPSG:4326*
  grid_4326 <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(0, 48, 0, 53, 5, 53, 5, 48, 0, 48),  # Longitude/Latitude
            ncol = 2, byrow = TRUE
          )
        )
      ),
      sf::st_polygon(
        list(
          matrix(
            c(5, 48, 5, 53, 10, 53, 10, 48, 5, 48), # Longitude/Latitude
            ncol = 2, byrow = TRUE
          )
        )
      )
    ),
    cellid = 1:2,
    crs = 4326 # Explicitly set CRS here
  )

  # Set grid CRS
  grid <- sf::st_transform(grid_4326, crs = 3857) # Use st_transform

  cube_crs <- "EPSG:3857"
  output_crs <- "EPSG:3857"
  data_sf <- sf::st_as_sf(data_df, coords = c("xcoord", "ycoord"), crs = 4326) %>%
    sf::st_transform(crs = 3857)
  result <- prepare_spatial_data(data_df, data_sf, grid, cube_crs, output_crs)
  expect_true("cellid" %in% names(result))
  expect_equal(result$cellid, c(1, 1, 1, 1))

  # Test with data outside the grid
  data_outside_df <- data.frame(
    xcoord = c(100, 200, 300, 400),
    ycoord = c(100, 200, 300, 400),
    species = c("A", "B", "C", "D")
  )
  data_outside_sf <- sf::st_as_sf(data_outside_df, coords = c("xcoord", "ycoord"), crs = 3857)
  expect_error(
    prepare_spatial_data(data_outside_df, data_outside_sf, grid, cube_crs, output_crs),
    "No spatial intersection between occurrence data and grid."
  )

  # Test with an empty grid
  grid_empty <- sf::st_sf(geometry = sf::st_sfc(), cellid = integer(0), crs = 3857)
  data_sf_empty_grid <- sf::st_as_sf(data_df, coords = c("xcoord", "ycoord"), crs = 3857)
  expect_error(
    prepare_spatial_data(data_df, data_sf_empty_grid, grid_empty, cube_crs, output_crs),
    "No spatial intersection between occurrence data and grid."
  )
})

test_that("prepare_spatial_data handles errors", {
  # Create sample data frame
  data_df <- data.frame(
    xcoord = c(1, 2, 3, 4), # Longitude
    ycoord = c(49, 50, 51, 52), # Latitude
    species = c("A", "B", "C", "D")
  )

  # Create sample grid *explicitly in EPSG:4326*
  grid_4326 <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(
        list(
          matrix(
            c(0, 48, 0, 53, 5, 53, 5, 48, 0, 48),  # Longitude/Latitude
            ncol = 2, byrow = TRUE
          )
        )
      ),
      sf::st_polygon(
        list(
          matrix(
            c(5, 48, 5, 53, 10, 53, 10, 48, 5, 48), # Longitude/Latitude
            ncol = 2, byrow = TRUE
          )
        )
      )
    ),
    cellid = 1:2,
    crs = 4326 # Explicitly set CRS here
  )


  cube_crs <- "EPSG:4326"
  output_crs <- "EPSG:4326"

  # Test with invalid data input.  This test doesn't need to change, as the error
  # is thrown within prepare_spatial_data before the sf conversion.
  expect_error(
    prepare_spatial_data(list(1, 2, 3), grid, cube_crs, output_crs)
  )

  # Test with invalid grid input
  expect_error(
    prepare_spatial_data(data_df, data.frame(a = 1:2, b = 3:4), cube_crs, output_crs)
  )
})
