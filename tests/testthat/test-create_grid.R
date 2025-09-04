test_that("create_grid creates a valid grid", {
  # Create a simple sf object for testing
  germany_map <- rnaturalearth::ne_countries(country = "Germany",
                                             scale = "medium",
                                             returnclass = "sf")
  germany_map <- sf::st_transform(germany_map, crs = "EPSG:3035")
  germany_bbox <- sf::st_bbox(germany_map)

  # Test with cell_size = 10 km
  grid_km <- create_grid(germany_bbox,
                         cell_size = 10000,
                         projected_crs = "EPSG:3035",
                         make_valid = FALSE)
  expect_s3_class(grid_km, "sf")
  expect_true("cellid" %in% names(grid_km))
  expect_true("area" %in% names(grid_km))
  expect_equal(sf::st_crs(grid_km), sf::st_crs(germany_map))

  # Test with make_valid = TRUE
  grid_valid <- create_grid(germany_bbox,
                            cell_size = 10000,
                            projected_crs = "EPSG:3035",
                            make_valid = TRUE)
  expect_s3_class(grid_valid, "sf")

})

test_that("create_grid handles edge cases", {
  # Create a simple sf object for testing
  denmark_map <- rnaturalearth::ne_countries(country = "Denmark",
                                             scale = "medium",
                                             returnclass = "sf")
  denmark_map <- sf::st_transform(denmark_map, crs = "EPSG:3035")
  denmark_bbox <- sf::st_bbox(denmark_map)

  # Test with small cell_size
  grid_small_km <- create_grid(denmark_bbox,
                               cell_size = 1000,
                               projected_crs = "EPSG:3035",
                               make_valid = FALSE)
  expect_s3_class(grid_small_km, "sf")

  # Test with large cell_size
  grid_large_km <- create_grid(denmark_bbox,
                               cell_size = 100000,
                               projected_crs = "EPSG:3035",
                               make_valid = FALSE)
  expect_s3_class(grid_large_km, "sf")

})

test_that("create_grid handles errors", {
  # Test with invalid input data
  expect_error(create_grid(data.frame(x = 1:10, y = 1:10),
                           cell_size = 10000,
                           projected_crs = "EPSG:3035",
                           make_valid = FALSE))

  # Test with invalid crs
  germany_map <- rnaturalearth::ne_countries(country = "Germany",
                                             scale = "medium",
                                             returnclass = "sf")
  germany_map <- sf::st_transform(germany_map, crs = "EPSG:3035")
  expect_error(create_grid(germany_map,
                           cell_size = 10000,
                           projected_crs = "invalid",
                           make_valid = FALSE))

  # Test with too large cell_size
  expect_error(create_grid(germany_map,
                           cell_size = 1000000,
                           projected_crs = "EPSG:3035",
                           make_valid = FALSE))

})
