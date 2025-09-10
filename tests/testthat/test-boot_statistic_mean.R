test_that("boot_statistic_mean calculates the correct mean", {
  data <- 1:5
  indices <- c(1, 3, 5)
  result <- boot_statistic_mean(data, indices)
  expect_equal(result, mean(data[indices]))
})

test_that("boot_statistic_mean handles empty indices", {
  data <- 1:5
  indices <- integer(0)
  result <- boot_statistic_mean(data, indices)
  expect_true(is.nan(result)) # mean of empty vector is NaN
})

test_that("boot_statistic_mean handles duplicate indices", {
  data <- 1:5
  indices <- c(1, 1, 3, 3, 5)
  result <- boot_statistic_mean(data, indices)
  expect_equal(result, mean(data[indices]))
})

test_that("boot_statistic_mean handles edge cases", {
  data <- numeric(0)
  indices <- numeric(0)
  result <- boot_statistic_mean(data, indices)
  expect_true(is.nan(result))

  data <- 5
  indices <- 1
  result <- boot_statistic_mean(data, indices)
  expect_equal(result, 5)
})

test_that("boot_statistic_mean handles NA values", {
  data <- c(1, 2, NA, 4, 5)
  indices <- 1:5
  result <- boot_statistic_mean(data, indices)
  expect_equal(result, mean(data, na.rm = TRUE))
})

test_that("boot_statistic_mean handles out-of-bounds indices", {
  data <- 1:5
  indices <- c(1, 2, 6)
  expect_error(boot_statistic_mean(data, indices),
               "Indices contain out-of-bounds values.")

  indices <- c(0, 1, 2)
  expect_error(boot_statistic_mean(data, indices),
               "Indices contain out-of-bounds values.")
})
