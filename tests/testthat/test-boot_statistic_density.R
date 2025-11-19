# --- Mock Data ---
# Dataset representing observations in cells for one year.
# total_area is constant (100) across all rows.
mock_data <- data.frame(
  cell_id = 1:5,
  obs = c(10, 5, 20, 15, 30), # Total sum: 80
  total_area = c(100, 100, 100, 100, 100)
)
# Expected density for all data: 80 / 100 = 0.8

# Test Block 1: Successful density calculation with full data (simulating initial call)
test_that("boot_statistic_density calculates correct density on full data", {

  indices <- 1:nrow(mock_data)
  result <- boot_statistic_density(mock_data, indices)

  # Expected result: sum(obs) / total_area = 80 / 100 = 0.8
  expect_equal(result, 0.8)
})

# Test Block 2: Successful density calculation with resampled data (simulating bootstrap)
test_that("boot_statistic_density calculates correct density on resampled data", {

  # Indices: Row 1 (10 obs), Row 3 (20 obs), Row 1 (10 obs, resampling row 1)
  indices <- c(1, 3, 1)

  result <- boot_statistic_density(mock_data, indices)

  # Resampled sum: 10 + 20 + 10 = 40
  # Constant area: 100
  # Expected result: 40 / 100 = 0.4
  expect_equal(result, 0.4)
})

# Test Block 3: Validation failure for indices > nrow(data)
test_that("boot_statistic_density stops on out-of-bounds indices (too large)", {

  # Index 6 is out of bounds (nrow is 5)
  indices <- c(1, 6)

  expect_error(
    boot_statistic_density(mock_data, indices),
    regexp = "Indices contain out-of-bounds values\\."
  )
})

# Test Block 4: Validation failure for indices < 1
test_that("boot_statistic_density stops on out-of-bounds indices (too small)", {

  # Index 0 is out of bounds
  indices <- c(1, 0)

  expect_error(
    boot_statistic_density(mock_data, indices),
    regexp = "Indices contain out-of-bounds values\\."
  )
})

# Test Block 5: Handles zero observations correctly
test_that("boot_statistic_density handles zero observations", {

  # Indices only sample rows with 0 obs
  mock_data_zero <- mock_data
  mock_data_zero$obs <- 0

  indices <- 1:3
  result <- boot_statistic_density(mock_data_zero, indices)

  # Expected result: 0 / 100 = 0
  expect_equal(result, 0)
})
