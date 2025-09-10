test_that("boot_statistic_newness calculates the correct rounded mean", {
  data <- 1:5
  indices <- c(1, 3, 5)
  result <- boot_statistic_newness(data, indices)
  expect_equal(result, round(mean(data[indices])))

  data <- c(1.1, 2.2, 3.3, 4.4, 5.5)
  indices <- 1:5
  result <- boot_statistic_newness(data, indices)
  expect_equal(result, round(mean(data[indices])))
})

test_that("boot_statistic_newness handles empty indices", {
  data <- 1:5
  indices <- integer(0)
  result <- boot_statistic_newness(data, indices)
  expect_true(is.nan(result))
})

test_that("boot_statistic_newness handles duplicate indices", {
  data <- 1:5
  indices <- c(1, 1, 3, 3, 5)
  result <- boot_statistic_newness(data, indices)
  expect_equal(result, round(mean(data[indices])))
})

test_that("boot_statistic_newness handles edge cases", {
  data <- numeric(0)
  indices <- numeric(0)
  result <- boot_statistic_newness(data, indices)
  expect_true(is.nan(result))

  data <- 5.6
  indices <- 1
  result <- boot_statistic_newness(data, indices)
  expect_equal(result, 6)

  data <- 5.1
  indices <- 1
  result <- boot_statistic_newness(data, indices)
  expect_equal(result, 5)
})

test_that("boot_statistic_newness handles out-of-bounds indices", {
  data <- 1:5
  indices <- c(1, 2, 6)
  expect_error(boot_statistic_newness(data, indices))

  indices <- c(0, 1, 2)
  expect_error(boot_statistic_newness(data, indices))
})

test_that("boot_statistic_newness handles NA values", {
  data <- c(1, 2, NA, 4, 5)
  indices <- 1:5
  result <- boot_statistic_newness(data, indices)
  expect_equal(result, round(mean(data, na.rm = TRUE)))
})
