# Tests for sum_by_taxon function

test_that("sum_by_taxon works with a valid rank column", {
  result <- sum_by_taxon(example_cube_1, "family")

  expect_s3_class(result, "tbl_df")
  expect_true("total_observations" %in% names(result))
  expect_true("family" %in% names(result))
  expect_gt(nrow(result), 0)

  # Sum of total_observations should equal sum of obs in original data
  expected_total <- sum(example_cube_1$data$obs, na.rm = TRUE)
  expect_equal(sum(result$total_observations), expected_total)
})

test_that("sum_by_taxon works with kingdom rank", {
  result <- sum_by_taxon(example_cube_1, "kingdom")

  expect_s3_class(result, "tbl_df")
  expect_true("kingdom" %in% names(result))
  expect_gt(nrow(result), 0)
})

test_that("sum_by_taxon works with scientificName rank", {
  result <- sum_by_taxon(example_cube_1, "scientificName")

  expect_s3_class(result, "tbl_df")
  expect_true("scientificName" %in% names(result))
  # Number of groups should match number of unique species
  n_species <- length(unique(example_cube_1$data$scientificName))
  expect_equal(nrow(result), n_species)
})

test_that("sum_by_taxon errors on non-processed_cube input", {
  expect_error(
    sum_by_taxon(data.frame(a = 1), "family"),
    "object must be of class 'processed_cube'"
  )
})

test_that("sum_by_taxon errors when rank is not a character", {
  expect_error(
    sum_by_taxon(example_cube_1, 123),
    "rank must be a single character string"
  )
})

test_that("sum_by_taxon errors when rank is a vector", {
  expect_error(
    sum_by_taxon(example_cube_1, c("family", "kingdom")),
    "rank must be a single character string"
  )
})

test_that("sum_by_taxon errors when rank column does not exist", {
  expect_error(
    sum_by_taxon(example_cube_1, "nonexistent_column"),
    "rank 'nonexistent_column' not found"
  )
test_that("sum_by_taxon works correctly", {
  # Mock data
  df <- data.frame(
    family = c("A", "A", "B"),
    obs = c(10, 20, 5),
    stringsAsFactors = FALSE
  )

  # Mock processed_cube object
  mock_cube <- structure(list(data = df), class = "processed_cube")

  # Test successful summation
  res <- sum_by_taxon(mock_cube, "family")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 2)
  # Check if both columns exist
  expect_true("total_observations" %in% names(res))
  expect_true("family" %in% names(res))

  # Verify values
  expect_equal(res$total_observations[res$family == "A"], 30)
  expect_equal(res$total_observations[res$family == "B"], 5)
})

test_that("sum_by_taxon errors on invalid input", {
  df <- data.frame(family = "A", obs = 1, stringsAsFactors = FALSE)
  mock_cube <- structure(list(data = df), class = "processed_cube")

  # Wrong class
  expect_error(sum_by_taxon(list(data = df), "family"), "must be of class 'processed_cube'")

  # Missing rank in data
  expect_error(sum_by_taxon(mock_cube, "genus"), "rank 'genus' not found in object\\$data")

  # Invalid rank parameter (multiple)
  expect_error(sum_by_taxon(mock_cube, c("family", "genus")), "must be a single character string")

  # Invalid rank parameter (non-character)
  expect_error(sum_by_taxon(mock_cube, 123), "must be a single character string")
})
