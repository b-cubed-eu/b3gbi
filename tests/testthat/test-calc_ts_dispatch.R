# Test S3 dispatch for calc_ts during bootstrapping

library(testthat)
library(b3gbi)

# Skip if dubicube not available
skip_if_not_installed("dubicube")

# Load test data
data(example_cube_1)

# Test that calc_ts dispatch works correctly
test_that("calc_ts S3 dispatch works for bootstrapping", {
  # Create indicator
  res <- total_occ_ts(example_cube_1)
  
  # Get prepared params
  params <- prepare_indicator_bootstrap(
    indicator = res,
    num_bootstrap = 2,
    ci_type = "norm"
  )
  
  # Check that data has correct class
  expect_true(
    "total_occ" %in% class(params$bootstrap_params$data_cube),
    info = "data_cube should have total_occ class"
  )
  
  # Check that total_occ is first class
  expect_equal(
    class(params$bootstrap_params$data_cube)[1],
    "total_occ",
    info = "total_occ should be first class for S3 dispatch"
  )
  
  # Test that calc_ts dispatches correctly
  test_result <- calc_ts(params$bootstrap_params$data_cube)
  
  # Should return a data frame with diversity_val column
  expect_s3_class(test_result, "data.frame")
  expect_true(
    "diversity_val" %in% names(test_result),
    info = "calc_ts should return diversity_val column"
  )
  expect_true(
    "year" %in% names(test_result),
    info = "calc_ts should return year column"
  )
})
