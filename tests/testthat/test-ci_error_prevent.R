test_that("ci_error_prevent handles all NA values", {
  mock_boot <- list(t = matrix(rep(NA, 10)), t0 = NA)
  result <- ci_error_prevent(mock_boot)
  expect_equal(result$t, matrix(rep(1.2, 10)))
  expect_equal(result$t0, 1.2)
})

test_that("ci_error_prevent handles some NA values", {
  mock_boot <- list(t = matrix(c(1, 2, NA, 4, NA)), t0 = 3)
  result <- ci_error_prevent(mock_boot)
  expect_equal(result$t, matrix(c(1, 2, 7/3, 4, 7/3)))
})

test_that("ci_error_prevent handles no NA values", {
  mock_boot <- list(t = matrix(1:10), t0 = 5)
  result <- ci_error_prevent(mock_boot)
  expect_equal(result, mock_boot)
})

test_that("ci_error_prevent handles edge cases", {
  mock_boot <- list(t = matrix(NA), t0 = NA)
  result <- ci_error_prevent(mock_boot)
  expect_equal(result$t, matrix(1.2))
  expect_equal(result$t0, 1.2)

  mock_boot <- list(t = matrix(numeric(0)), t0 = NA)
  result <- ci_error_prevent(mock_boot)
  expect_equal(result$t, matrix(numeric(0)))
  expect_equal(result$t0, 1.2)
})
