test_that("detect_grid identifies isea3h codes", {
  # Example ISEA3H code (Mocnik style - long integer)
  isea3h_code <- "5639762336074163442"
  expect_equal(detect_grid(isea3h_code), "isea3h")
})

test_that("isea3h_code_to_coords works with dggridR", {
  skip_if_not_installed("dggridR")
  
  # Example code from GBIF
  isea3h_code <- "5639762336074163442"
  
  # Attempt conversion
  coords <- isea3h_code_to_coords(isea3h_code)
  
  # Check structure
  expect_s3_class(coords, "data.frame")
  expect_true("xcoord" %in% names(coords))
  expect_true("ycoord" %in% names(coords))
  expect_true("resolution" %in% names(coords))
  
  # We don't verify exact coordinates without knowing the Mocnik mapping details,
  # but we check if they are within valid lat/lon ranges.
  expect_true(all(coords$xcoord >= -180 & coords$xcoord <= 180))
  expect_true(all(coords$ycoord >= -90 & coords$ycoord <= 90))
})

test_that("indicator wrappers handle isea3h cubes correctly", {
  skip_if_not_installed("dggridR")
  
  # Mock a processed cube with isea3h data
  # We use the coords from the previous test or known valid ones
  # Assuming isea3h code conversion is working (or mocked)
  
  # Mocking isea3h_code_to_coords if strictly necessary, but better to test integration if possible.
  # For this test, we construct a data frame that looks like it came from process_cube(grid_type="isea3h")
  
  mock_data <- tibble::tibble(
    cellCode = c("5639762336074163442", "5639762336074163443"), # Dummy codes
    year = c(2020, 2020),
    obs = c(10, 5),
    taxonKey = c(123, 456), # Using taxonKey as per process_cube standard
    scientificName = c("Species A", "Species B"),
    # Valid Lat/Lon for testing 
    xcoord = c(10.0, 10.1),
    ycoord = c(50.0, 50.1),
    resolution = c("res13", "res13")
  )
  
  mock_cube <- list(
    data = mock_data,
    grid_type = "isea3h",
    coord_range = list(10.0, 10.1, 50.0, 50.1), # xmin, xmax, ymin, ymax
    resolution = "res13"
  )
  class(mock_cube) <- c("processed_cube", "list")
  
  # Run an indicator (Observed Richness)
  # We expect this to run through compute_indicator_workflow -> create_grid (adapted) -> result
  result <- obs_richness_map(mock_cube)
  
  # Checks
  expect_s3_class(result, "sf")
  expect_s3_class(result, "diversity_grid")
  expect_equal(nrow(result), 2) # Should match input rows
  expect_true("richness" %in% names(result))
  
  # Verify CRS is WGS84 as expected for ISEA3H
  input_crs <- sf::st_crs(result)
  expect_equal(input_crs$epsg, 4326)
})
