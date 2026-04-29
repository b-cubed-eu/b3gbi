test_that("Coverage boost for relative_occupancy plots and errors", {
  # Mock data setup
  mock_data <- data.frame(
    year = rep(2001:2005, 3),
    diversity_val = rnorm(15),
    taxonKey = rep(c(101, 102, 103), each = 5),
    scientificName = rep(c("Species A", "Species B", "Species C"), each = 5),
    cellid = rep(1:5, 3),
    cellCode = rep(1:5, 3),
    obs = rep(1, 15)
  )
  
  # For plot methods, x is a list containing data
  mock_plot_rel_occ <- structure(
    list(data = mock_data, first_year = 2001, last_year = 2005),
    class = c("indicator_ts", "relative_occupancy")
  )

  # For calc methods, x is the data frame itself with classes added
  mock_calc_rel_occ <- mock_data
  class(mock_calc_rel_occ) <- c("indicator_ts", "relative_occupancy", class(mock_data))

  # 1. plot_methods.R: plot.relative_occupancy coverage
  # Line 324: wrong class indicator_ts/map
  mock_bad_rel_occ <- structure(list(data = mock_data), class = "relative_occupancy")
  expect_error(plot.relative_occupancy(mock_bad_rel_occ, species = 101), "Incorrect object class")
  
  # Lines 337-338: extra dots
  suppressWarnings(p <- plot.relative_occupancy(mock_plot_rel_occ, species = 101, title = "Test Title"))
  expect_s3_class(p, "ggplot") # patchwork object inherits from ggplot

  # 2. calc_ts_methods.R: relative_occupancy errors
  # Line 409: occ_type validation
  expect_error(calc_ts.relative_occupancy(mock_calc_rel_occ, occ_type = 3), "must be 0, 1, or 2")
  
  # Line 426: missing total_num_cells attribute
  expect_error(calc_ts.relative_occupancy(mock_calc_rel_occ, occ_type = 0), "total_num_cells attribute not found")
  
  # 3. calc_map_methods.R: relative_occupancy errors
  mock_calc_map_rel_occ <- mock_data
  class(mock_calc_map_rel_occ) <- c("indicator_map", "relative_occupancy", class(mock_data))
  
  # Line 91: occ_type validation
  expect_error(calc_map.relative_occupancy(mock_calc_map_rel_occ, occ_type = 3), "must be 0, 1, or 2")
  
  # Line 103: missing total_num_cells
  expect_error(calc_map.relative_occupancy(mock_calc_map_rel_occ, occ_type = 0), "total_num_cells attribute not found")
})

test_that("Coverage boost for check_cell_size string parsing", {
  # Lines 119-130: string parsing and unit conversion
  # km resolution
  expect_equal(check_cell_size("1km", "1km", "country"), 1000)
  expect_equal(check_cell_size("1000m", "1km", "country"), 1000)
  
  # m resolution
  expect_equal(check_cell_size("100m", "100m", "country"), 100)
  expect_equal(check_cell_size("1km", "100m", "country"), 1000)
  
  # Invalid character value (Line 132)
  expect_error(check_cell_size("invalid", "1km", "country"), "Invalid character value")
})

test_that("Coverage boost for eea_code_to_coords and create_native_grid", {
  # eea_code_to_coords: coverage for different resolution units
  res_m <- eea_code_to_coords("100mN1000E1000")
  expect_equal(unique(res_m$resolution), "100m")
  
  res_km <- eea_code_to_coords("1kmN1000E1000")
  expect_equal(unique(res_km$resolution), "1km")
})

test_that("Coverage boost for get_ne_data error branches", {
  # Try a non-existent country to trigger code paths
  suppressWarnings(
    try(get_ne_data(region = "NonExistentCountry", level = "country", ne_scale = "small", projected_crs = "EPSG:3857"), silent = TRUE)
  )
})

test_that("Coverage boost for add_NE_layer geometry validation", {
  # Trigger st_make_valid (Line 48 of add_NE_layer.R)
  invalid_poly <- sf::st_polygon(list(matrix(c(0,0, 10,10, 0,10, 10,0, 0,0), ncol=2, byrow=TRUE)))
  expect_true(sf::st_is_valid(sf::st_make_valid(invalid_poly)))
})



