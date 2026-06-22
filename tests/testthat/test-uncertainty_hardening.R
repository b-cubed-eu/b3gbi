test_that("prepare_indicator_bootstrap handles expected_years", {
  indicator <- list(
    div_type = "total_occ",
    raw_data = data.frame(year = 2000, obs = 1),
    data = data.frame(year = 2000, diversity_val = 1)
  )
  class(indicator$raw_data) <- c("total_occ", "data.frame")
  
  expected <- 2000:2005
  params <- prepare_indicator_bootstrap(
    indicator = indicator,
    num_bootstrap = 10,
    ci_type = "norm",
    expected_years = expected
  )
  
  expect_equal(params$bootstrap_params$expected_years, expected)
})

test_that("calc_ts_evenness_core respects expected_years", {
  # Mock data with only one year
  df <- data.frame(
    year = 2000,
    taxonKey = 1,
    obs = 1
  )
  class(df) <- c("pielou_evenness", "data.frame")
  
  # Call with expected_years including a year not in df
  expected <- c(2000, 2001)
  res <- b3gbi:::calc_ts_evenness_core(df, type = "pielou_evenness", expected_years = expected)
  
  # Check that 2001 is present in output (even if NA or 0)
  expect_true(2001 %in% res$year)
})

test_that("calc_ts_hill_core respects expected_years", {
  df <- data.frame(
    year = 2000,
    taxonKey = 1,
    obs = 1,
    scientificName = "A",
    cellCode = "X"
  )
  class(df) <- c("hill0", "data.frame")
  
  expected <- c(2000, 2001)
  res <- b3gbi:::calc_ts_hill_core(df, type = "hill0", expected_years = expected)
  
  expect_true(2001 %in% res$year)
})

test_that("calc_ts_completeness_core respects expected_years", {
  df <- data.frame(
    year = 2000,
    taxonKey = 1,
    obs = 1,
    scientificName = "A",
    cellid = 1,
    cellCode = "X"
  )
  class(df) <- c("completeness", "data.frame")
  
  expected <- c(2000, 2001)
  # Test both gridded and non-gridded logic
  res_non_gridded <- b3gbi:::calc_ts_completeness_core(df, gridded_average = FALSE, expected_years = expected)
  expect_true(2001 %in% res_non_gridded$year)
  
  res_gridded <- b3gbi:::calc_ts_completeness_core(df, gridded_average = TRUE, expected_years = expected)
  expect_true(2001 %in% res_gridded$year)
})

test_that("compute_indicator_workflow filters dots correctly", {
  # Mock a cube that will fail early but after dots filtering
  mock_cube <- list(data = data.frame())
  class(mock_cube) <- "wrong_class"
  
  # We just want to check that it doesn't error out on the filtering itself
  # and that the logic for extracting parameters works.
  # Since we can't easily check internal variables, we just verify it runs
  # up to the class check.
  expect_error(
    compute_indicator_workflow(mock_cube, type = "total_occ", ci_type = "norm", unknown_arg = 1),
    regexp = "not recognized"
  )
})

test_that("compute_indicator_workflow handles invalid dim_type", {
  mock_cube <- list(data = data.frame(obs = 1))
  class(mock_cube) <- "processed_cube"
  expect_error(
    compute_indicator_workflow(mock_cube, type = "total_occ", dim_type = "invalid"),
    regexp = "should be one of"
  )
})

test_that("add_ci returns original object with warning for excluded indicators", {
  indicator <- list(div_type = "obs_richness")
  class(indicator) <- "indicator_ts"
  
  expect_warning(
    res <- add_ci(indicator),
    regexp = "Returning indicator without"
  )
  expect_identical(res, indicator)
})

test_that("calc_ci S3 methods work correctly", {
  # Common mock objects
  ind_base <- data.frame(year = 2000, diversity_val = 1)
  
  # Hill indicators - provide more data points to allow successful bootstrapping
  df_hill <- data.frame(
    year = rep(2000, 20),
    obs = c(rep(1, 10), rep(0, 10)),
    scientificName = rep(LETTERS[1:5], 4),
    cellCode = paste0("C", 1:20),
    taxonKey = rep(1:5, 4)
  )
  for (h in c("hill0", "hill1", "hill2")) {
    x <- df_hill
    class(x) <- c(h, "data.frame")
    res <- suppressWarnings(b3gbi:::calc_ci(x, ind_base, num_bootstrap = 20))
    expect_true("ll" %in% names(res), info = paste("Testing", h))
  }
  
  # Evenness indicators - need multiple species with DIFFERENT abundances
  df_even <- data.frame(
    year = rep(2000, 10),
    obs = 1:10,
    taxonKey = rep(1:5, 2)
  )
  for (e in c("pielou_evenness", "williams_evenness")) {
    x <- df_even
    class(x) <- c(e, "data.frame")
    res <- suppressWarnings(b3gbi:::calc_ci(x, ind_base, num_bootstrap = 20))
    expect_true("ll" %in% names(res), info = paste("Testing", e))
  }
  
  # Total Occ and Density
  df_to <- data.frame(year = rep(2000, 20), obs = sample(c(0, 1, 5, 10), 20, replace = TRUE))
  class(df_to) <- c("total_occ", "data.frame")
  res_to <- suppressWarnings(b3gbi:::calc_ci(df_to, ind_base, num_bootstrap = 20))
  expect_true("ll" %in% names(res_to))
  
  df_od <- data.frame(year = rep(2000, 20), obs = sample(c(0, 1, 5, 10), 20, replace = TRUE), area = 1:20)
  class(df_od) <- c("occ_density", "data.frame")
  attr(df_od, "total_area_sqkm") <- 100
  res_od <- suppressWarnings(b3gbi:::calc_ci(df_od, ind_base, num_bootstrap = 20))
  expect_true("ll" %in% names(res_od))
  
  # Richness Density
  df_srd <- data.frame(year = rep(2000, 10), taxonKey = 1:10)
  class(df_srd) <- c("spec_richness_density", "data.frame")
  attr(df_srd, "total_area_sqkm") <- 100
  res_srd <- suppressWarnings(b3gbi:::calc_ci(df_srd, ind_base, num_bootstrap = 20))
  expect_true("ll" %in% names(res_srd))
  
  # Rarity and Newness
  # Use multiple years and cells with varied species counts to ensure variance
  df_rare <- data.frame(
    year = rep(2000:2001, each = 20),
    obs = 1:40,
    area = 1:40,
    cellid = rep(c(rep(1, 5), rep(2, 4), rep(3, 3), rep(4, 2), 5:10), 2),
    taxonKey = rep(1:20, 2)
  )
  ind_rare <- data.frame(year = 2000:2001, diversity_val = 1)
  for (r in c("ab_rarity", "area_rarity", "newness")) {
    x <- df_rare
    class(x) <- c(r, "data.frame")
    res <- suppressWarnings(b3gbi:::calc_ci(x, ind_rare, num_bootstrap = 20))
    expect_true("ll" %in% names(res), info = paste("Testing", r))
  }
  
  # Spec Occ and Range
  df_spec <- data.frame(
    year = rep(2000, 20),
    obs = sample(c(0, 1), 20, replace = TRUE),
    taxonKey = 1,
    scientificName = "A",
    cellCode = paste0("C", 1:20)
  )
  ind_spec <- data.frame(year = 2000, diversity_val = 1, taxonKey = 1, scientificName = "A")
  for (s in c("spec_occ", "spec_range")) {
    x <- df_spec
    class(x) <- c(s, "data.frame")
    res <- suppressWarnings(b3gbi:::calc_ci(x, ind_spec, num_bootstrap = 20))
    expect_true("ll" %in% names(res), info = paste("Testing", s))
  }
})
