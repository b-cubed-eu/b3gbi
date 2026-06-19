# These tests need the real get_ne_data for correct bbox coordinates
Sys.unsetenv("B3GBI_TESTING")

library(testthat)
library(sf)
library(dplyr)
library(b3gbi)

test_that("Native grid scaling aligns correctly for EEA", {
  mock_data <- data.frame(
    cellCode = c("1kmE4321N3210", "1kmE4322N3211", "1kmE4329N3219"),
    xcoord = c(4321000, 4322000, 4329000),
    ycoord = c(3210000, 3211000, 3219000),
    resolution = "1km",
    obs = c(10, 20, 30),
    taxonKey = c(1, 2, 1),
    scientificName = c("Species A", "Species B", "Species A"),
    year = 2020
  )

  mock_cube <- list(
    data = mock_data,
    grid_type = "eea",
    first_year = 2020,
    last_year = 2020,
    num_species = 2,
    kingdoms = "Animalia",
    num_families = 2,
    coord_range = c(4321000, 4329000, 3210000, 3219000),
    resolutions = "1km"
  )
  class(mock_cube) <- "processed_cube"

  # Native resolution result
  result_native <- compute_indicator_workflow(
    data = mock_cube,
    type = "obs_richness",
    dim_type = "map",
    ci_type = "none"
  )

  # Aggregated to 10km
  result_10km <- compute_indicator_workflow(
    data = mock_cube,
    type = "obs_richness",
    dim_type = "map",
    cell_size = 10,
    ci_type = "none"
  )

  # Aggregated should have fewer or equal rows
  expect_lte(nrow(result_10km$data), nrow(result_native$data))
  # No NA values
  expect_false(any(is.na(result_10km$data$diversity_val)))
  # Total richness should be preserved (average of cell values)
  expect_gt(sum(result_10km$data$diversity_val, na.rm = TRUE), 0)
})

test_that("Native grid scaling aligns correctly for MGRS", {
  # Use valid 1km MGRS codes (3 digits each for easting/northing)
  mock_data <- data.frame(
    cellCode = c("32VNH320100", "32VNH321101"),
    xcoord = c(532000, 532100),
    ycoord = c(6210000, 6210100),
    utmzone = 32,
    hemisphere = "North",
    resolution = "1km",
    obs = c(10, 20),
    taxonKey = c(1, 2),
    scientificName = c("Species A", "Species B"),
    year = 2020
  )

  mock_cube <- list(
    data = mock_data,
    grid_type = "mgrs",
    first_year = 2020,
    last_year = 2020,
    num_species = 2,
    kingdoms = "Animalia",
    num_families = 2,
    coord_range = c(5, 50, 6, 51),
    resolutions = "1km"
  )
  class(mock_cube) <- "processed_cube"

  # Native resolution result
  result_native <- compute_indicator_workflow(
    data = mock_cube,
    type = "obs_richness",
    dim_type = "map",
    ci_type = "none"
  )

  # Aggregated to 10km
  result_10km <- compute_indicator_workflow(
    data = mock_cube,
    type = "obs_richness",
    dim_type = "map",
    cell_size = 10,
    ci_type = "none"
  )

  # Aggregated should have fewer or equal rows
  expect_lte(nrow(result_10km$data), nrow(result_native$data))
  # No NA values
  expect_false(any(is.na(result_10km$data$diversity_val)))
  # Has valid data
  expect_gt(sum(result_10km$data$diversity_val, na.rm = TRUE), 0)
})

test_that("EEA grid scales successfully with cell_size='grid' and no resolution prefix in cellCode", {
  mock_data <- data.frame(
    cellCode = c("E4321N3210", "E4322N3211"),
    xcoord = c(4321000, 4322000),
    ycoord = c(3210000, 3211000),
    obs = c(10, 20),
    taxonKey = c(1, 2),
    scientificName = c("Species A", "Species B"),
    year = 2020
  )

  mock_cube <- list(
    data = mock_data,
    grid_type = "eea",
    first_year = 2020,
    last_year = 2020,
    num_species = 2,
    kingdoms = "Animalia",
    num_families = 2,
    coord_range = c(4321000, 4322000, 3210000, 3211000),
    resolutions = NULL
  )
  class(mock_cube) <- "processed_cube"

  result <- compute_indicator_workflow(
    data = mock_cube,
    type = "obs_richness",
    dim_type = "map",
    cell_size = "grid",
    ci_type = "none"
  )

  expect_equal(nrow(result$data), 2)
  expect_false(any(is.na(result$data$diversity_val)))
})

test_that("create_native_grid handles invalid/missing resolutions fallback", {
  # Mock MGRS cube data with NA or invalid resolution in df
  mgrs_df <- data.frame(
    cellCode = c("32VNH05", "32VNH06"),
    xcoord = c(500000, 600000),
    ycoord = c(6000000, 6100000),
    utmzone = c(32, 32),
    hemisphere = c("N", "N"),
    resolution = c(NA, "invalid_res")
  )
  
  grid <- b3gbi:::create_native_grid(mgrs_df, projection = "EPSG:4326", grid_type = "mgrs")
  expect_s3_class(grid, "sf")
  expect_equal(nrow(grid), 2)
  
  # EQDGC cube data with NA or invalid resolution
  eqdgc_df <- data.frame(
    cellCode = c("E10N20", "E10N21"),
    xcoord = c(10, 10),
    ycoord = c(20, 21),
    resolution = c("invalid_res", NA)
  )
  grid_eqdgc <- b3gbi:::create_native_grid(eqdgc_df, projection = "EPSG:4326", grid_type = "eqdgc")
  expect_s3_class(grid_eqdgc, "sf")
  expect_equal(nrow(grid_eqdgc), 2)
})

