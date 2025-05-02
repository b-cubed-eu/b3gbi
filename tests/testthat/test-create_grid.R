test_that("create_grid creates a valid grid", {
  # Create a simple sf object for testing
  germany_map <- rnaturalearth::ne_countries(country = "Germany",
                                             scale = "medium",
                                             returnclass = "sf")
  germany_map <- sf::st_transform(germany_map, crs = "EPSG:3035")

  # Test with cell_size = 10 km
  grid_km <- create_grid(germany_map,
                         cell_size = 10000,
                         grid_units = "km",
                         make_valid = FALSE)
  expect_s3_class(grid_km, "sf")
  expect_true("cellid" %in% names(grid_km))
  expect_true("area" %in% names(grid_km))
  expect_equal(sf::st_crs(grid_km), sf::st_crs(germany_map))

  # Test with make_valid = TRUE
  grid_valid <- create_grid(germany_map,
                            cell_size = 10000,
                            grid_units = "km",
                            make_valid = TRUE)
  expect_s3_class(grid_valid, "sf")

  # Test with cell_size = 1 degree
  germany_map_deg <- sf::st_transform(germany_map, crs = "EPSG:4326")
  grid_deg <- create_grid(germany_map_deg,
                          cell_size = 1,
                          grid_units = "degrees",
                          make_valid = FALSE)
  expect_s3_class(grid_deg, "sf")
  expect_equal(sf::st_crs(grid_deg), sf::st_crs(germany_map_deg))

})

test_that("create_grid handles edge cases", {
  # Create a simple sf object for testing
  germany_map <- rnaturalearth::ne_countries(country = "Germany",
                                             scale = "medium",
                                             returnclass = "sf")
  germany_map <- sf::st_transform(germany_map, crs = "EPSG:3035")

  # Test with small cell_size in km
  grid_small_km <- create_grid(germany_map,
                               cell_size = 1000,
                               grid_units = "km",
                               make_valid = FALSE)
  expect_s3_class(grid_small_km, "sf")

  # Test with large cell_size in km
  grid_large_km <- create_grid(germany_map,
                               cell_size = 100000,
                               grid_units = "km",
                               make_valid = FALSE)
  expect_s3_class(grid_large_km, "sf")

  # Convert to degrees
  germany_map_deg <- sf::st_transform(germany_map, crs = "EPSG:4326")
  # Test with small cell_size in degrees
  grid_small_deg <- create_grid(germany_map_deg,
                                cell_size = 0.25,
                                grid_units = "degrees",
                                make_valid = FALSE)
  expect_s3_class(grid_small_deg, "sf")

  # Test with large cell_size in degrees
  grid_large_deg <- create_grid(germany_map_deg,
                                cell_size = 10,
                                grid_units = "degrees",
                                make_valid = FALSE)
  expect_s3_class(grid_large_deg, "sf")

})

test_that("create_grid handles errors", {
  # Test with invalid input data
  expect_error(create_grid(data.frame(x = 1:10, y = 1:10),
                           cell_size = 10000,
                           grid_units = "km",
                           make_valid = FALSE))

  # Test with invalid grid_units
  germany_map <- rnaturalearth::ne_countries(country = "Germany",
                                             scale = "medium",
                                             returnclass = "sf")
  germany_map <- sf::st_transform(germany_map, crs = "EPSG:3035")
  expect_error(create_grid(germany_map,
                           cell_size = 10000,
                           grid_units = "invalid",
                           make_valid = FALSE))

  # Test with too large cell_size in km
  expect_error(create_grid(germany_map,
                           cell_size = 1000000,
                           grid_units = "km",
                           make_valid = FALSE))

  # Convert to degrees
  germany_map_deg <- sf::st_transform(germany_map, crs = "EPSG:4326")

   # Test with too large cell_size in degrees
  expect_error(create_grid(germany_map,
                           cell_size = 100,
                           grid_units = "degrees",
                           make_valid = FALSE))

})
