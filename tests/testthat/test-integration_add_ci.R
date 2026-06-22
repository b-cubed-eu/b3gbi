library(testthat)
library(b3gbi)

test_that("Integration: total_occ_ts followed by add_ci works", {
  # Load example data
  data(example_cube_1)

  # Calculate time series of total occurrences
  # This should return an indicator_ts object without CIs
  res <- total_occ_ts(example_cube_1)

  expect_s3_class(res, "indicator_ts")
  expect_false("ll" %in% names(res$data))
  expect_false("ul" %in% names(res$data))

  # Add confidence intervals
  # Using num_bootstrap = 10 for speed in tests
  res_ci <- suppressWarnings(add_ci(res, num_bootstrap = 10, bootstrap_level = "indicator"))

  expect_s3_class(res_ci, "indicator_ts")
  expect_true("ll" %in% names(res_ci$data))
  expect_true("ul" %in% names(res_ci$data))
  expect_true("est_boot" %in% names(res_ci$data))
})

test_that("Integration: pielou_evenness_ts followed by add_ci works (cube level)", {
  skip_if_not_installed("dubicube")

  # Load example data
  data(example_cube_1)

  # Calculate time series of Pielou evenness
  res <- pielou_evenness_ts(example_cube_1)

  expect_s3_class(res, "indicator_ts")

  # Add confidence intervals (cube level)
  # Using num_bootstrap = 20 for stability
  res_ci <- suppressWarnings(add_ci(res, num_bootstrap = 20, bootstrap_level = "cube"))

  expect_s3_class(res_ci, "indicator_ts")
  expect_true("ll" %in% names(res_ci$data))
  expect_true("ul" %in% names(res_ci$data))
})

test_that("Integration: add_ci works with string-based taxonomic keys", {
  cube_df <- tibble::tibble(
    year = c(2020, 2020, 2021, 2021),
    cellCode = c("1kmE32N20", "1kmE32N20", "1kmE32N20", "1kmE32N20"),
    occurrences = c(5, 10, 15, 20),
    scientificName = c("Species A", "Species B", "Species A", "Species B"),
    speciesKey = c("A1", "B2", "A1", "B2"),
    kingdomKey = c("N", "N", "N", "N"),
    familyKey = c("F1", "F1", "F1", "F1")
  )
  # Process cube
  cube <- suppressWarnings(process_cube(cube_df, grid_type = "eea"))
  
  # Calculate indicator
  res <- spec_occ_ts(cube)
  
  # Add CI
  res_ci <- suppressWarnings(add_ci(res, num_bootstrap = 20, bootstrap_level = "indicator"))
  
  expect_s3_class(res_ci, "indicator_ts")
  expect_true("ll" %in% names(res_ci$data))
  expect_true("ul" %in% names(res_ci$data))
  expect_equal(res_ci$data$taxonKey[1], "A1")
})
