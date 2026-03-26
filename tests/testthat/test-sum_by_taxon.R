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
