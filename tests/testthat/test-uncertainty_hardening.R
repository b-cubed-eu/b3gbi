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
