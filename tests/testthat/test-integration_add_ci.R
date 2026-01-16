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
  res_ci <- add_ci(res, num_bootstrap = 10, bootstrap_level = "indicator")
  
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
  # Using num_bootstrap = 2 for speed
  res_ci <- add_ci(res, num_bootstrap = 2, bootstrap_level = "cube")
  
  expect_s3_class(res_ci, "indicator_ts")
  expect_true("ll" %in% names(res_ci$data))
  expect_true("ul" %in% names(res_ci$data))
})
