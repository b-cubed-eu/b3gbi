library(testthat)
library(b3gbi)

test_that("add_ci handles input validation", {
  # Invalid data class
  expect_error(
    add_ci(indicator = "invalid"),
    "indicator must be an indicator_ts object."
  )
})

test_that("add_ci returns original object with warning for excluded indicators", {
  # Mock an indicator_ts object for obs_richness
  mock_ts <- list(
    data = data.frame(year = 2000, diversity_val = 10),
    div_type = "obs_richness"
  )
  class(mock_ts) <- c("indicator_ts", "obs_richness")

  expect_warning(
    result <- add_ci(mock_ts),
    "Cannot calculate sensible confidence intervals for obs_richness"
  )
  expect_equal(result, mock_ts)

  # Mock for Hill numbers
  mock_hill <- list(
    data = data.frame(year = 2000, diversity_val = 10),
    div_type = "hill0"
  )
  class(mock_hill) <- c("indicator_ts", "hill0")

  expect_warning(
    result_hill <- add_ci(mock_hill),
    "Confidence intervals cannot calculated for hill0 as they are handled by the iNext package"
  )
  expect_equal(result_hill, mock_hill)
})

test_that("add_ci calls dubicube for cube-level bootstrapping", {
  skip_if_not_installed("dubicube")

  # Mock raw data
  mock_raw_data <- data.frame(
    year = c(2000, 2000),
    scientificName = c("Sp A", "Sp B"),
    obs = c(1, 1),
    cellCode = c("C1", "C2")
  )

  # Mock indicator_ts object
  mock_ts <- list(
    data = data.frame(year = 2000, diversity_val = 2),
    raw_data = mock_raw_data,
    div_type = "total_occ"
  )
  class(mock_ts) <- c("indicator_ts", "total_occ")

  # Mock dubicube functions
  mock_bootstrap_cube <- function(...) {
    data.frame(year = 2000, sample_id = 1, diversity_val = 2)
  }

  mock_calculate_bootstrap_ci <- function(...) {
    data.frame(year = 2000,
               ll = 1,
               ul = 3,
               est_boot = 2,
               se_boot = 0.1,
               bias_boot = 0,
               int_type = "norm",
               conf = 0.95,
               est_original = 2)
  }

  testthat::with_mocked_bindings(
    bootstrap_cube = mock_bootstrap_cube,
    calculate_bootstrap_ci = mock_calculate_bootstrap_ci,
    .package = "dubicube",
    {
      result <- add_ci(mock_ts, bootstrap_level = "cube")

      expect_true("ll" %in% names(result$data))
      expect_true("ul" %in% names(result$data))
      expect_equal(result$data$ll, 1)
      expect_equal(result$data$ul, 3)
    }
  )
})

test_that("add_ci determines boot_method correctly for pielou_evenness", {
  skip_if_not_installed("dubicube")

  # 1. Setup Mock Data
  mock_raw_data <- data.frame(
    year = c(2000, 2000),
    scientificName = c("Sp A", "Sp B"),
    obs = c(1, 1),
    cellCode = c("C1", "C2")
  )

  # Mock indicator_ts object for pielou_evenness
  # Note: Pielou evenness is a 'whole_cube' indicator in the rule book
  mock_ts <- list(
    data = data.frame(year = 2000, diversity_val = 0.8),
    raw_data = mock_raw_data,
    div_type = "pielou_evenness"
  )
  class(mock_ts) <- c("indicator_ts", "pielou_evenness")

  # 2. Define Mock Functions
  # Capture and inspect arguments passed from b3gbi to dubicube
  mock_bootstrap_cube <- function(...) {
    args <- list(...)

    # VERIFY: 'method' should now be present and correctly set to 'boot_whole_cube'
    # based on the logic in prepare_indicator_bootstrap
    expect_true("method" %in% names(args),
                info = "The 'method' argument should be passed to bootstrap_cube")

    expect_equal(args$method, "boot_whole_cube",
                 info = "Pielou evenness should use 'boot_whole_cube' method")

    # VERIFY: 'seed' is passed correctly (if provided in add_ci)
    expect_true("seed" %in% names(args),
                info = "The 'seed' argument should be passed to bootstrap_cube")

    # Return a dummy bootstrap result
    data.frame(year = 2000, sample_id = 1, diversity_val = 0.8)
  }

  mock_calculate_bootstrap_ci <- function(...) {
    # Return a dummy CI result
    data.frame(year = 2000,
               ll = 0.7,
               ul = 0.9,
               est_boot = 0.8,
               se_boot = 0.05,
               bias_boot = 0,
               int_type = "norm",
               conf = 0.95,
               est_original = 0.8)
  }

  # 3. Execute Test with Mocked Bindings
  testthat::with_mocked_bindings(
    bootstrap_cube = mock_bootstrap_cube,
    calculate_bootstrap_ci = mock_calculate_bootstrap_ci,
    .package = "dubicube",
    {
      # We provide a seed here to test the new functionality
      add_ci(mock_ts, bootstrap_level = "cube", seed = 123)
    }
  )
})

test_that("add_ci respects boot_args and ci_args", {
  skip_if_not_installed("dubicube")

  # Mock raw data
  mock_raw_data <- data.frame(
    year = c(2000, 2000),
    scientificName = c("Sp A", "Sp B"),
    obs = c(1, 1),
    cellCode = c("C1", "C2")
  )

  # Mock indicator_ts object
  mock_ts <- list(
    data = data.frame(year = 2000, diversity_val = 2),
    raw_data = mock_raw_data,
    div_type = "total_occ"
  )
  class(mock_ts) <- c("indicator_ts", "total_occ")

  captured_seed <- NULL
  mock_bootstrap_cube <- function(..., seed) {
    captured_seed <<- seed
    data.frame(year = 2000, sample_id = 1, diversity_val = 2)
  }

  captured_type <- NULL
  mock_calculate_bootstrap_ci <- function(..., type) {
    captured_type <<- type
    data.frame(year = 2000,
               ll = 1,
               ul = 3,
               est_boot = 2,
               se_boot = 0.1,
               bias_boot = 0,
               int_type = "norm",
               conf = 0.95,
               est_original = 2)
  }

  testthat::with_mocked_bindings(
    bootstrap_cube = mock_bootstrap_cube,
    calculate_bootstrap_ci = mock_calculate_bootstrap_ci,
    .package = "dubicube",
    {
      add_ci(mock_ts,
             boot_args = list(seed = 456),
             ci_args = list(type = "perc"))

      expect_equal(captured_seed, 456)
      expect_equal(captured_type, "perc")
    }
  )
})
