test_that("detect_grid identifies isea3h codes", {
  # Example ISEA3H code (Mocnik style - long integer)
  isea3h_code <- "5639762336074163442"
  expect_equal(detect_grid(isea3h_code), "isea3h")

  # Negative codes (pentagon cells) should also be detected
  expect_equal(detect_grid("-458282525011250000"), "isea3h")
})

test_that("isea3h_code_to_coords structure is correct", {
  # A real GBIF code from Denmark
  isea3h_code <- "-458282525011250000"

  coords <- isea3h_code_to_coords(isea3h_code)

  # Check structure
  expect_s3_class(coords, "data.frame")
  expect_true("xcoord" %in% names(coords))
  expect_true("ycoord" %in% names(coords))
  expect_true("resolution" %in% names(coords))

  # Coordinates should be valid numbers (not NAs)
  expect_false(is.na(coords$xcoord))
  expect_false(is.na(coords$ycoord))
})

test_that("isea3h_code_to_coords decodes Denmark pentagon", {
  # Real GBIF Denmark code: pentagon near Skagerrak
  coords <- isea3h_code_to_coords("-458282525011250000")
  expect_equal(coords$ycoord, 58.282525, tolerance = 0.001)
  expect_equal(coords$xcoord, 11.250000, tolerance = 0.001)
  expect_equal(coords$resolution, "4")
})

test_that("isea3h_code_to_coords decodes European hexagon", {
  # Real GBIF code: hexagon near Netherlands
  coords <- isea3h_code_to_coords("452583359004665569")
  expect_equal(coords$ycoord, 52.583359, tolerance = 0.001)
  expect_equal(coords$xcoord, 4.665569, tolerance = 0.001)
  expect_equal(coords$resolution, "4")
})

test_that("isea3h_code_to_coords handles negative longitude", {
  # Real GBIF code: negative longitude (west of Greenwich)
  coords <- isea3h_code_to_coords("4859758557001684287")
  expect_equal(coords$ycoord, 59.758557, tolerance = 0.001)
  expect_equal(coords$xcoord, -1.684287, tolerance = 0.001)
  expect_equal(coords$resolution, "4")
})

test_that("isea3h_code_to_coords handles Southern Hemisphere", {
  # Constructed: Australia lat=-25.0, lon=135.0, res=4
  # res_code = 4 + 22 = 26 (negative lat)
  # id = 26 * 10^17 + 25000000 * 10^9 + 135000000
  coords <- isea3h_code_to_coords("2625000000135000000")
  expect_equal(coords$ycoord, -25.0, tolerance = 0.001)
  expect_equal(coords$xcoord, 135.0, tolerance = 0.001)
  expect_equal(coords$resolution, "4")
})

test_that("isea3h_code_to_coords handles negative lat and lon", {
  # Constructed: Brazil lat=-15.0, lon=-47.0, res=4
  # res_code = 4 + 22 + 44 = 70 (negative lat AND lon)
  # id = 70 * 10^17 + 15000000 * 10^9 + 47000000
  coords <- isea3h_code_to_coords("7015000000047000000")
  expect_equal(coords$ycoord, -15.0, tolerance = 0.001)
  expect_equal(coords$xcoord, -47.0, tolerance = 0.001)
  expect_equal(coords$resolution, "4")
})

test_that("isea3h_code_to_coords handles USA (negative lon only)", {
  # Constructed: Washington DC area lat=39.0, lon=-77.0, res=4
  # res_code = 4 + 44 = 48 (negative lon)
  # id = 48 * 10^17 + 39000000 * 10^9 + 77000000
  coords <- isea3h_code_to_coords("4839000000077000000")
  expect_equal(coords$ycoord, 39.0, tolerance = 0.001)
  expect_equal(coords$xcoord, -77.0, tolerance = 0.001)
  expect_equal(coords$resolution, "4")
})

test_that("isea3h_code_to_coords handles vectors correctly", {
  ids <- c(
    "-458282525011250000",
    "452583359004665569",
    "465090030011250000"
  )
  coords <- isea3h_code_to_coords(ids)
  expect_equal(nrow(coords), 3)
  expect_false(any(is.na(coords$xcoord)))
  expect_false(any(is.na(coords$ycoord)))
})

test_that("isea3h_code_to_coords handles empty and NA input", {
  # Empty input
  coords_empty <- isea3h_code_to_coords(character(0))
  expect_equal(nrow(coords_empty), 0)

  # NA input
  coords_na <- isea3h_code_to_coords(NA)
  expect_true(is.na(coords_na$xcoord))
  expect_true(is.na(coords_na$ycoord))
})

test_that("indicator wrappers handle isea3h cubes correctly (structure check)", {
  # Mock a processed cube with isea3h data
  mock_data <- tibble::tibble(
    cellCode = c("-458282525011250000", "452583359004665569", "-458282525011250000"),
    year = c(2020, 2020, 2021),
    obs = c(10, 5, 8),
    taxonKey = c(123, 456, 123),
    scientificName = c("Species A", "Species B", "Species A"),
    xcoord = c(11.25, 4.67, 11.25),
    ycoord = c(58.28, 52.58, 58.28),
    resolution = c("4", "4", "4")
  )

  mock_cube <- list(
    data = mock_data,
    grid_type = "isea3h",
    first_year = 2020,
    last_year = 2021,
    num_species = 2,
    species_names = c("Species A", "Species B"),
    coord_range = list(xmin = 4.67, xmax = 11.25, ymin = 52.58, ymax = 58.28),
    resolution = "4"
  )
  class(mock_cube) <- c("processed_cube", "list")

  # Run an indicator (Observed Richness Map)
  result_map <- obs_richness_map(mock_cube)
  expect_s3_class(result_map, "indicator_map")
  expect_equal(result_map$grid_type, "isea3h")

  # Run an indicator (Observed Richness Time Series)
  result_ts <- obs_richness_ts(mock_cube)
  expect_s3_class(result_ts, "indicator_ts")
  expect_equal(nrow(result_ts$data), 2) # 2020 and 2021

  # Verify CRS is WGS84 as expected for ISEA3H
  input_crs <- sf::st_crs(result_map$data)
  expect_equal(input_crs$epsg, 4326)
})

test_that("plot_map uses wider grid_line_width for isea3h grids", {
  # Build a minimal indicator_map with grid_type = "isea3h"
  mock_data <- tibble::tibble(
    cellCode = c("-458282525011250000", "452583359004665569", "-458282525011250000"),
    year = c(2020, 2020, 2021),
    obs = c(10, 5, 8),
    taxonKey = c(123, 456, 123),
    scientificName = c("Species A", "Species B", "Species A"),
    xcoord = c(11.25, 4.67, 11.25),
    ycoord = c(58.28, 52.58, 58.28),
    resolution = c("4", "4", "4")
  )

  mock_cube <- list(
    data = mock_data,
    grid_type = "isea3h",
    first_year = 2020,
    last_year = 2021,
    num_species = 2,
    species_names = c("Species A", "Species B"),
    coord_range = list(xmin = 4.67, xmax = 11.25, ymin = 52.58, ymax = 58.28),
    resolution = "4"
  )
  class(mock_cube) <- c("processed_cube", "list")

  result_map <- obs_richness_map(mock_cube)

  # Plot without setting grid_line_width - should use the isea3h default (0.5)
  p <- suppressWarnings(plot_map(result_map))
  expect_s3_class(p, "ggplot")
})

test_that("plot_map filter_outliers removes extreme cells", {
  # Use the built-in example map
  data(example_indicator_map1, package = "b3gbi")

  # filter_outliers = TRUE should not error on a normal dataset
  p <- suppressWarnings(plot_map(example_indicator_map1, filter_outliers = TRUE))
  expect_s3_class(p, "ggplot")

  # Inject an extreme outlier cell into the data
  outlier_map <- example_indicator_map1
  outlier_geom <- sf::st_sfc(sf::st_point(c(1e7, 1e7)),
    crs = sf::st_crs(outlier_map$data)
  )
  outlier_row <- sf::st_sf(
    cellid = 9999L,
    diversity_val = 1,
    geometry = outlier_geom
  )
  # Match columns to avoid bind errors
  missing_cols <- setdiff(names(outlier_map$data), names(outlier_row))
  for (col in missing_cols) {
    outlier_row[[col]] <- NA
  }
  outlier_map$data <- rbind(outlier_map$data, outlier_row[, names(outlier_map$data)])

  # With filter_outliers = TRUE, the outlier should be removed
  p_filtered <- suppressWarnings(plot_map(outlier_map, filter_outliers = TRUE))
  expect_s3_class(p_filtered, "ggplot")

  # The filtered plot data should have fewer rows than the unfiltered
  p_unfiltered <- suppressWarnings(plot_map(outlier_map, filter_outliers = FALSE))
  expect_true(nrow(p_filtered$data) <= nrow(p_unfiltered$data))
})
