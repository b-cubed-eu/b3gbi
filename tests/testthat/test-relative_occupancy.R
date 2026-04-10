# Tests for relative_occupancy functions

library(b3gbi)
library(testthat)

test_that("relative_occupancy_map calculates correctly", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")
  
  result <- relative_occupancy_map(example_cube_1, ci_type = "none")
  
  expect_s3_class(result, "relative_occupancy")
  expect_true("diversity_val" %in% names(result$data))
  expect_true(all(result$data$diversity_val >= 0 & result$data$diversity_val <= 1))
})

test_that("relative_occupancy_ts calculates correctly", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")
  
  result <- relative_occupancy_ts(example_cube_1, ci_type = "none")
  
  expect_s3_class(result, "relative_occupancy")
  expect_true("diversity_val" %in% names(result$data))
  expect_true("year" %in% names(result$data))
  expect_true(all(result$data$diversity_val >= 0 & result$data$diversity_val <= 1, na.rm = TRUE))
})
