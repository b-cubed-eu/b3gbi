# Mock compute_evenness_formula
mock_compute_evenness_formula <- function(data, type) {
  return(sum(data)) # For simplicity, just sum the data
}

test_that("boot_statistic_evenness subsets data correctly", {
  data <- 1:5
  indices <- c(1, 3, 5)
  type <- "simpson"

  with_mocked_bindings(
    `compute_evenness_formula` = mock_compute_evenness_formula,
    {
      result <- boot_statistic_evenness(data, indices, type)
      expect_equal(result, sum(data[indices]))
    }
  )
})

test_that("boot_statistic_evenness handles empty indices", {
  data <- 1:5
  indices <- integer(0)
  type <- "simpson"

  with_mocked_bindings(
    `compute_evenness_formula` = mock_compute_evenness_formula,
    {
      result <- boot_statistic_evenness(data, indices, type)
      expect_equal(result, 0)
    }
  )
})

test_that("boot_statistic_evenness handles duplicate indices", {
  data <- 1:5
  indices <- c(1, 1, 3, 3, 5)
  type <- "simpson"

  with_mocked_bindings(
    `compute_evenness_formula` = mock_compute_evenness_formula,
    {
      result <- boot_statistic_evenness(data, indices, type)
      expect_equal(result, sum(data[indices]))
    }
  )
})

test_that("boot_statistic_evenness handles out-of-bounds indices", {
  data <- 1:5
  indices <- c(1:5, 100)
  type <- "simpson"

  with_mocked_bindings(
    `compute_evenness_formula` = mock_compute_evenness_formula,
    {
      expect_error(boot_statistic_evenness(data, indices, type),
                   "Indices contain out-of-bounds values.")
    }
  )
})
