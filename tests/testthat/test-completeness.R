
test_that("completeness_map works with example data", {
  skip_on_cran()
  
  # Use built-in example data
  data(example_cube_1, package = "b3gbi")
  
  # Run properly
  result <- completeness_map(example_cube_1, cutoff_length = 0)
  
  expect_s3_class(result, "indicator_map")
  expect_s3_class(result, "completeness")
  expect_true("diversity_val" %in% names(result$data))
  expect_true("cellid" %in% names(result$data))
  
  # Check values are between 0 and 1
  expect_true(all(result$data$diversity_val >= 0 & result$data$diversity_val <= 1, na.rm = TRUE))
})

test_that("completeness_ts works with example data", {
  skip_on_cran()
  
  data(example_cube_1, package = "b3gbi")
  
  result <- completeness_ts(example_cube_1, cutoff_length = 0)
  
  expect_s3_class(result, "indicator_ts")
  expect_s3_class(result, "completeness")
  expect_true("diversity_val" %in% names(result$data))
  expect_true("year" %in% names(result$data))
  
  # Check years are present
  expect_true(length(unique(result$data$year)) > 0)
})
