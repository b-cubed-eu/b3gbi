
# Mock compute_tax_distinct_formula
mock_compute_tax_distinct_formula <- function(df, tax_hier) {
  return(nrow(df)) # Simple mock: return the number of rows in df
}

mock_readRDS <- function(filename) {
  # Create a mock taxonomic_hierarchy.RDS for testing
  mock_tax_hier <- data.frame(
    scientificName = c("A", "B", "C"),
    family = c("F1", "F2", "F1")
  )
  return(mock_tax_hier)
}

test_that("boot_statistic_td calculates taxonomic distinctness correctly", {
  data <- c("A", "B", "C", "A")
  indices <- c(1, 2, 3)

  mockr::with_mock(
    `compute_tax_distinct_formula` = mock_compute_tax_distinct_formula,
    `my_readRDS` = mock_readRDS,
    {
      result <- boot_statistic_td(data, indices)
      expect_equal(result, 3)
    }
  )

  indices <- c(1, 1, 3)
  mockr::with_mock(
    `compute_tax_distinct_formula` = mock_compute_tax_distinct_formula,
    `my_readRDS` = mock_readRDS,
    {
      result <- boot_statistic_td(data, indices)
      expect_equal(result, 3)
    }
  )
})

test_that("boot_statistic_td handles empty indices", {
  data <- c("A", "B", "C")
  indices <- integer(0)

  mockr::with_mock(
    `compute_tax_distinct_formula` = mock_compute_tax_distinct_formula,
    `my_readRDS` = mock_readRDS,
    {
      result <- boot_statistic_td(data, indices)
      expect_equal(result, 0)
    }
  )
})

test_that("boot_statistic_td handles edge cases", {
  data <- character(0)
  indices <- integer(0)

  mockr::with_mock(
    `compute_tax_distinct_formula` = mock_compute_tax_distinct_formula,
    `my_readRDS` = mock_readRDS,
    {
      result <- boot_statistic_td(data, indices)
      expect_equal(result, 0)
    }
  )
})

test_that("boot_statistic_td handles out-of-bounds indices", {
  data <- c("A", "B", "C")
  indices <- c(1, 2, 4)
  expect_error(boot_statistic_td(data, indices))

  indices <- c(0, 1, 2)
  expect_error(boot_statistic_td(data, indices))
})

test_that("boot_statistic_td handles NA values", {
  data <- c("A", "B", NA, "C")
  indices <- 1:4

  mockr::with_mock(
    `compute_tax_distinct_formula` = mock_compute_tax_distinct_formula,
    `my_readRDS` = mock_readRDS,
    {
      result <- boot_statistic_td(data, indices)
      expect_equal(result, 4)
    }
  )
})
