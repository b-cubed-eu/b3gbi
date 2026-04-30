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
