library(testthat)
library(b3gbi)

test_that("calc_ci hill methods are covered", {
  # Mock data
  mock_data <- data.frame(
    year = c(2000, 2001),
    scientificName = c("Sp A", "Sp B"),
    obs = c(1, 1),
    cellCode = c("C1", "C2")
  )

  # Indicator data
  indicator_df <- data.frame(year = c(2000, 2001), diversity_val = c(1, 1))

  # Mock calc_ts_hill_core
  mock_hill_results <- data.frame(
    year = c(2000, 2001),
    ll = c(0.8, 0.8),
    ul = c(1.2, 1.2)
  )

  testthat::with_mocked_bindings(
    calc_ts_hill_core = function(...) mock_hill_results,
    .package = "b3gbi",
    {
      # Test hill0
      x0 <- mock_data
      class(x0) <- c("hill0", class(mock_data))
      res0 <- calc_ci(x0, indicator = indicator_df, num_bootstrap = 10)
      expect_true("ll" %in% names(res0))
      expect_equal(res0$ll, c(0.8, 0.8))

      # Test hill1
      x1 <- mock_data
      class(x1) <- c("hill1", class(mock_data))
      res1 <- calc_ci(x1, indicator = indicator_df, num_bootstrap = 10)
      expect_true("ll" %in% names(res1))

      # Test hill2
      x2 <- mock_data
      class(x2) <- c("hill2", class(mock_data))
      res2 <- calc_ci(x2, indicator = indicator_df, num_bootstrap = 10)
      expect_true("ll" %in% names(res2))
    }
  )
})

test_that("add_ci and prepare_indicator_bootstrap errors are covered", {
  # Mock indicator
  mock_indicator <- list(
    data = data.frame(year = 2000, diversity_val = 1),
    raw_data = data.frame(year = 2000, obs = 1),
    div_type = "total_occ"
  )
  class(mock_indicator) <- "indicator_ts"

  # Invalid bootstrap_level in add_ci
  expect_error(
    add_ci(mock_indicator, bootstrap_level = "invalid")
  )

  # Unknown div_type in prepare_indicator_bootstrap
  bad_indicator <- mock_indicator
  bad_indicator$div_type <- "unknown"
  expect_error(
    prepare_indicator_bootstrap(bad_indicator, num_bootstrap = 10, ci_type = "perc"),
    "Unknown indicator"
  )

  # Missing grouping columns in prepare_indicator_bootstrap
  spec_indicator <- mock_indicator
  spec_indicator$div_type <- "spec_occ"
  # raw_data missing taxonKey
  expect_error(
    prepare_indicator_bootstrap(spec_indicator, num_bootstrap = 10, ci_type = "perc"),
    "Missing required grouping columns"
  )
})

test_that("calc_ci methods stopifnot_error for wrong data class", {
  indicator_df <- data.frame(year = 2000, diversity_val = 1)

  expect_error(calc_ci.total_occ(data.frame(), indicator_df), "Wrong data class")
  expect_error(calc_ci.occ_density(data.frame(), indicator_df), "Wrong data class")
  expect_error(calc_ci.spec_richness_density(data.frame(), indicator_df), "Wrong data class")

  x_newness <- data.frame(year=2000)
  expect_error(calc_ci.newness(x_newness, indicator_df), "Wrong data class")

  expect_error(calc_ci.williams_evenness(data.frame()), "Wrong data class")
  expect_error(calc_ci.pielou_evenness(data.frame()), "Wrong data class")
  expect_error(calc_ci.ab_rarity(data.frame(), indicator_df), "Wrong data class")
  expect_error(calc_ci.area_rarity(data.frame(), indicator_df), "Wrong data class")
  expect_error(calc_ci.spec_occ(data.frame(), indicator_df), "Wrong data class")
  expect_error(calc_ci.spec_range(data.frame(), indicator_df), "Wrong data class")
  expect_error(calc_ci.hill0(data.frame(), indicator_df), "Wrong data class")
})

test_that("add_ci edge cases and warnings", {
  # 1. Insufficient data warning
  mock_indicator <- list(
    data = data.frame(year = 2000, diversity_val = 1),
    raw_data = data.frame(year = 2000, obs = 1),
    div_type = "total_occ"
  )
  class(mock_indicator) <- "indicator_ts"

  testthat::with_mocked_bindings(
    bootstrap_cube = function(...) data.frame(),
    calculate_bootstrap_ci = function(...) data.frame(),
    .package = "dubicube",
    {
      expect_warning(
        add_ci(mock_indicator, bootstrap_level = "cube"),
        "Unable to calculate confidence intervals"
      )
    }
  )

  # 2. Already contains CIs and overwrite = FALSE
  ci_indicator <- mock_indicator
  ci_indicator$data$ll <- 0.9
  ci_indicator$data$ul <- 1.1
  expect_warning(
    add_ci(ci_indicator, overwrite = FALSE),
    "already contains confidence intervals"
  )

  # 3. Non-hill indicator in noci_list
  obs_indicator <- list(
    data = data.frame(year = 2000, diversity_val = 1),
    div_type = "obs_richness"
  )
  class(obs_indicator) <- "indicator_ts"
  expect_warning(
    add_ci(obs_indicator),
    "Cannot calculate sensible confidence intervals"
  )
})

test_that("calc_ci density methods area validation", {
  indicator_df <- data.frame(year = 2000, diversity_val = 1)

  # occ_density missing area attribute
  x_occ <- data.frame(year = 2000, obs = 1)
  class(x_occ) <- c("occ_density", class(x_occ))
  expect_error(calc_ci.occ_density(x_occ, indicator_df), "missing or invalid in the data attributes")

  # spec_richness_density missing area attribute
  x_spec <- data.frame(year = 2000, taxonKey = 1)
  class(x_spec) <- c("spec_richness_density", class(x_spec))
  expect_error(calc_ci.spec_richness_density(x_spec, indicator_df), "missing or invalid in the data attributes")
})

test_that("calc_ci.default warning", {
  expect_warning(calc_ci("invalid", data.frame()), "calc_ci does not know how to handle object of class")
})

test_that("calc_ci.spec_occ and spec_range empty results", {
  indicator_df <- data.frame(year = 2000, taxonKey = 1, scientificName = "Sp A", diversity_val = 1)
  mock_data <- data.frame(year = 2000, taxonKey = 1, scientificName = "Sp A", obs = 1, cellCode = "C1")
  class(mock_data) <- c("spec_occ", class(mock_data))

  # Mock get_bootstrap_ci to return a data frame with negative ll
  testthat::with_mocked_bindings(
    get_bootstrap_ci = function(...) data.frame(year=2000, ll=-1, ul=1.1),
    .package = "b3gbi",
    {
      # Test spec_occ
      res_occ <- calc_ci.spec_occ(mock_data, indicator_df, num_bootstrap = 10)
      expect_equal(nrow(res_occ), 1)
      expect_true("ll" %in% names(res_occ))
      expect_equal(res_occ$ll, 0) # Converted from -1

      # Test spec_range
      mock_range <- mock_data
      class(mock_range) <- c("spec_range", class(mock_range))
      res_range <- calc_ci.spec_range(mock_range, indicator_df, num_bootstrap = 10)
      expect_equal(nrow(res_range), 1)
    }
  )
})

test_that("calc_ci.occ_density and spec_richness_density valid area", {
  indicator_df <- data.frame(year = 2000, diversity_val = 1)

  # occ_density
  x_occ <- data.frame(year = 2000, obs = 1)
  attr(x_occ, "total_area_sqkm") <- 100
  class(x_occ) <- c("occ_density", class(x_occ))

  testthat::with_mocked_bindings(
    boot = function(...) list(t0 = 1, t = matrix(1, ncol=1)),
    .package = "boot",
    {
       # We also need to mock get_bootstrap_ci which is in b3gbi
       testthat::with_mocked_bindings(
         get_bootstrap_ci = function(...) data.frame(year=2000, ll=0.8, ul=1.2),
         .package = "b3gbi",
         {
           res <- calc_ci.occ_density(x_occ, indicator_df, num_bootstrap = 10)
           expect_true("ll" %in% names(res))
         }
       )
    }
  )

  # spec_richness_density
  x_spec <- data.frame(year = 2000, taxonKey = 1)
  attr(x_spec, "total_area_sqkm") <- 100
  class(x_spec) <- c("spec_richness_density", class(x_spec))

  testthat::with_mocked_bindings(
    boot = function(...) list(t0 = 1, t = matrix(1, ncol=1)),
    .package = "boot",
    {
      testthat::with_mocked_bindings(
        get_bootstrap_ci = function(...) data.frame(year=2000, ll=0.8, ul=1.2),
        .package = "b3gbi",
        {
          res <- calc_ci.spec_richness_density(x_spec, indicator_df, num_bootstrap = 10)
          expect_true("ll" %in% names(res))
        }
      )
    }
  )
})

test_that("calc_ci evenness methods trigger core", {
  x <- data.frame(year = 2000, obs = 1)
  class(x) <- c("williams_evenness", class(x))

  testthat::with_mocked_bindings(
    calc_ci_evenness_core = function(...) data.frame(year=2000, diversity_val=1, ll=0.8, ul=1.2),
    .package = "b3gbi",
    {
      res_w <- calc_ci.williams_evenness(x)
      expect_true("ll" %in% names(res_w))

      class(x) <- c("pielou_evenness", "data.frame")
      res_p <- calc_ci.pielou_evenness(x)
      expect_true("ll" %in% names(res_p))
    }
  )
})

test_that("calc_ci.ab_rarity and area_rarity coverage", {
  indicator_df <- data.frame(year = 2000, diversity_val = 1)
  x <- data.frame(year = 2000, taxonKey = 1, obs = 1, cellid = 1)

  testthat::with_mocked_bindings(
    boot = function(...) list(t0 = 1, t = matrix(1, ncol=1)),
    .package = "boot",
    {
      testthat::with_mocked_bindings(
        get_bootstrap_ci = function(...) data.frame(year=2000, ll=0.8, ul=1.2),
        .package = "b3gbi",
        {
          # ab_rarity
          x_ab <- x
          class(x_ab) <- c("ab_rarity", class(x))
          res_ab <- calc_ci.ab_rarity(x_ab, indicator_df)
          expect_true("ll" %in% names(res_ab))

          # area_rarity
          x_area <- x
          class(x_area) <- c("area_rarity", class(x))
          res_area <- calc_ci.area_rarity(x_area, indicator_df)
          expect_true("ll" %in% names(res_area))
        }
      )
    }
  )
})

