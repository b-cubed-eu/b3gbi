# Tests for replace_na function

library(b3gbi)
library(testthat)

test_that("replace_na replaces NA values with zeros", {
  mock_map <- list(
    div_name = "Test Indicator",
    map_level = "country",
    map_region = "Test Country",
    projection = "EPSG:3857",
    coord_range = c(1, 2, 3, 4),
    cell_size = "10km",
    num_cells = 4,
    first_year = 2000,
    last_year = 2010,
    num_years = 11,
    num_species = 20,
    num_families = c(10, 11),
    kingdoms = c("Animalia", "Plantae"),
    data = data.frame(
      cellid = 1:4,
      cellCode = c("A", "B", "C", "D"),
      diversity_val = c(1, NA, 3, NA)
    )
  )
  class(mock_map) <- c("indicator_map", "test_map")
  
  result <- replace_na(mock_map)
  
  expect_s3_class(result, "indicator_map")
  expect_equal(result$data$diversity_val, c(1, 0, 3, 0))
  expect_false(any(is.na(result$data$diversity_val)))
})

test_that("replace_na errors when no NA values present", {
  mock_map <- list(
    div_name = "Test Indicator",
    map_level = "country",
    map_region = "Test Country",
    projection = "EPSG:3857",
    coord_range = c(1, 2, 3, 4),
    cell_size = "10km",
    num_cells = 3,
    first_year = 2000,
    last_year = 2010,
    num_years = 11,
    num_species = 20,
    num_families = c(10, 11),
    kingdoms = c("Animalia", "Plantae"),
    data = data.frame(
      cellid = 1:3,
      cellCode = c("A", "B", "C"),
      diversity_val = c(1, 2, 3)
    )
  )
  class(mock_map) <- c("indicator_map", "test_map")
  
  expect_error(replace_na(mock_map), "No NA values present")
})

test_that("replace_na errors on wrong class input", {
  expect_error(
    replace_na(data.frame(a = 1)),
    "Incorrect object class"
  )
})

test_that("replace_na works with indicator_ts objects", {
  mock_ts <- list(
    div_name = "Test Indicator TS",
    first_year = 2000,
    last_year = 2010,
    map_region = "Test Region",
    coord_range = c(1, 2, 3, 4),
    num_species = 10,
    num_families = c(5, 6),
    kingdoms = "Animalia",
    data = data.frame(
      year = 2000:2003,
      cellCode = "CODE1",
      diversity_val = c(1, NA, 3, NA)
    )
  )
  class(mock_ts) <- c("indicator_ts", "test_ts")
  
  result <- replace_na(mock_ts)
  
  expect_s3_class(result, "indicator_ts")
  expect_equal(result$data$diversity_val, c(1, 0, 3, 0))
  expect_false(any(is.na(result$data$diversity_val)))
})
