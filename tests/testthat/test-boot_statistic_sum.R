test_that("boot_statistic_sum calculates the correct sum", {
  data <- 1:5
  indices <- c(1, 3, 5)
  result <- boot_statistic_sum(data, indices)
  expect_equal(result, sum(data[indices]))

  data <- c(1.1, 2.2, 3.3, 4.4, 5.5)
  indices <- 1:5
  result <- boot_statistic_sum(data, indices)
  expect_equal(result, sum(data[indices]))
})

test_that("boot_statistic_sum handles empty indices", {
  data <- 1:5
  indices <- integer(0)
  result <- boot_statistic_sum(data, indices)
  expect_equal(result, 0)
})

test_that("boot_statistic_sum handles duplicate indices", {
  data <- 1:5
  indices <- c(1, 1, 3, 3, 5)
  result <- boot_statistic_sum(data, indices)
  expect_equal(result, sum(data[indices]))
})

test_that("boot_statistic_sum handles edge cases", {
  data <- numeric(0)
  indices <- numeric(0)
  result <- boot_statistic_sum(data, indices)
  expect_equal(result, 0)

  data <- 5.6
  indices <- 1
  result <- boot_statistic_sum(data, indices)
  expect_equal(result, 5.6)
})

test_that("boot_statistic_sum handles NA values", {
  data <- c(1, 2, NA, 4, 5)
  indices <- 1:5
  result <- boot_statistic_sum(data, indices)
  expect_equal(result, sum(data, na.rm = TRUE))

  data <- c(NA, NA, NA)
  indices <- 1:3
  result <- boot_statistic_sum(data, indices)
  expect_equal(result, 0)
})

test_that("boot_statistic_sum handles out of bounds indices", {
  data <- 1:5
  indices <- c(1, 2, 6)
  expect_error(boot_statistic_sum(data, indices))

  indices <- c(0, 1, 2)
  expect_error(boot_statistic_sum(data, indices))
})
