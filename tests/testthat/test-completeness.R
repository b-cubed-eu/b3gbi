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

test_that("completeness_map works with abundance data", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  result <- completeness_map(example_cube_1, data_type = "abundance", cutoff_length = 0)

  expect_s3_class(result, "indicator_map")
  expect_true(all(result$data$diversity_val >= 0 & result$data$diversity_val <= 1, na.rm = TRUE))
})

test_that("completeness_map honors cutoff_length", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  # Default cutoff_length is 5.
  # If we set it high, all cells should be removed, triggering an error.
  expect_error(
    completeness_map(example_cube_1, cutoff_length = 200),
    "There are no grid cells left to process after filtering."
  )
})

test_that("completeness_map works with assume_freq = TRUE", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  result <- completeness_map(example_cube_1, data_type = "incidence", assume_freq = TRUE, cutoff_length = 0)

  expect_s3_class(result, "indicator_map")
  expect_true(all(result$data$diversity_val >= 0 & result$data$diversity_val <= 1, na.rm = TRUE))
})

test_that("completeness handles empty data gracefully", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")

  # Create an empty cube
  empty_cube <- example_cube_1
  empty_cube$data <- empty_cube$data[0, ]

  expect_error(completeness_map(empty_cube), "No data found in the cube")
  expect_error(completeness_ts(empty_cube), "No data found in the cube")
})
