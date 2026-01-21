library(testthat)
library(dplyr)

# ------------------------------------------------------------------------------
# Mock data for testing
# ------------------------------------------------------------------------------

# Mock data for map calculation
mock_map_data <- data.frame(
  cellid = c(1, 1, 2, 2, 2),
  cellCode = c("A1", "A1", "A2", "A2", "A2"),
  taxonKey = c(101, 102, 101, 103, 104),
  area = c(10, 10, 20, 20, 20)
)
mock_map_srd <- structure(mock_map_data, 
                          class = c("spec_richness_density", "data.frame"))

# Mock data for time series calculation
mock_ts_data <- data.frame(
  year = c(2001, 2001, 2002, 2002, 2002),
  taxonKey = c(101, 102, 101, 103, 104)
)
attr(mock_ts_data, "total_area_sqkm") <- 100
mock_ts_srd <- structure(mock_ts_data, 
                         class = c("spec_richness_density", "data.frame"))

# ------------------------------------------------------------------------------
# Tests for calc_map.spec_richness_density
# ------------------------------------------------------------------------------

test_that("calc_map.spec_richness_density calculates correctly", {
  result <- calc_map.spec_richness_density(mock_map_srd)
  
  # Cell 1: 2 unique species / Area 10 = 0.2
  # Cell 2: 3 unique species / Area 20 = 0.15
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(0.2, 0.15)
  )
  
  expect_equal(result, expected_result)
  expect_true("cellCode" %in% names(result))
})

test_that("calc_map.spec_richness_density handles empty data", {
  empty_input <- mock_map_srd[0, ]
  result <- calc_map.spec_richness_density(empty_input)
  
  expect_equal(nrow(result), 0)
  expect_true(is.numeric(result$diversity_val))
})

# ------------------------------------------------------------------------------
# Tests for calc_ts.spec_richness_density
# ------------------------------------------------------------------------------

test_that("calc_ts.spec_richness_density calculates correctly", {
  result <- calc_ts.spec_richness_density(mock_ts_srd)
  
  # Year 2001: 2 unique species / Total Area 100 = 0.02
  # Year 2002: 3 unique species / Total Area 100 = 0.03
  expected_result <- data.frame(
    year = c(2001, 2002),
    diversity_val = c(0.02, 0.03)
  )
  
  expect_equal(result, expected_result)
})

test_that("calc_ts.spec_richness_density throws error on missing total_area_sqkm", {
  invalid_input <- mock_ts_srd
  attr(invalid_input, "total_area_sqkm") <- NULL
  
  expect_error(calc_ts.spec_richness_density(invalid_input), "missing or invalid")
})

test_that("calc_ts.spec_richness_density handles empty input gracefully", {
  empty_input <- mock_ts_srd[0, ]
  attr(empty_input, "total_area_sqkm") <- 100
  result <- calc_ts.spec_richness_density(empty_input)
  
  expect_equal(nrow(result), 0)
})

# ------------------------------------------------------------------------------
# Tests for wrapper functions (mocked)
# ------------------------------------------------------------------------------

test_that("spec_richness_density wrappers call workflow correctly", {
  mock_cube <- structure(list(data = data.frame()), class = "processed_cube")
  
  with_mocked_bindings(
    compute_indicator_workflow = function(data, type, dim_type, ...) {
      list(data = data, type = type, dim_type = dim_type)
    },
    {
      res_map <- spec_richness_density_map(mock_cube)
      expect_equal(res_map$type, "spec_richness_density")
      expect_equal(res_map$dim_type, "map")
      
      res_ts <- spec_richness_density_ts(mock_cube)
      expect_equal(res_ts$type, "spec_richness_density")
      expect_equal(res_ts$dim_type, "ts")
    }
  )
})

# ------------------------------------------------------------------------------
# Tests for plot and bootstrap
# ------------------------------------------------------------------------------

test_that("plot.spec_richness_density handles valid input", {
  mock_indicator <- structure(list(data = data.frame()), 
                             class = c("indicator_ts", "spec_richness_density"))
  
  with_mocked_bindings(
    call_plot = function(...) TRUE,
    {
      expect_silent(plot.spec_richness_density(mock_indicator))
    }
  )
})

test_that("boot_statistic_richness_density calculates correctly", {
  d <- data.frame(
    taxonKey = c(1, 1, 2, 3),
    total_area = c(10, 10, 10, 10)
  )
  
  # 3 unique species / 10 area = 0.3
  res <- boot_statistic_richness_density(d, 1:4)
  expect_equal(res, 0.3)
  
  # Resample only first two rows (both are taxonKey 1)
  # 1 unique species / 10 area = 0.1
  res_resample <- boot_statistic_richness_density(d, c(1, 2))
  expect_equal(res_resample, 0.1)
})
