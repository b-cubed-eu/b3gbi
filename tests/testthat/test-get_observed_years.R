# --- Mock Data ---

# Case 1: indicator_map
# Data only contains observations for 2002 and 2004. Range should be 2000-2005.
mock_map <- structure(
  list(
    years_with_obs = c(2002, 2004),
    # The function uses min/max from this vector, so we include the range definition for robustness
    some_other_data = NULL
  ),
  class = "indicator_map"
)

# Case 2: indicator_ts
# Data contains observations for 2001 and 2003. Range should be 2000-2005.
mock_ts <- structure(
  list(
    data = data.frame(
      year = c(2001, 2003, 2003),
      value = c(10, 20, 30)
    )
  ),
  class = "indicator_ts"
)

# Case 3: processed_cube (Logic is the same as indicator_ts)
mock_cube <- structure(
  list(
    data = data.frame(
      year = c(1999, 2001, 2001),
      value = c(1, 2, 3)
    )
  ),
  class = "processed_cube"
)


# Test Block 1: indicator_map class
test_that("get_observed_years correctly handles indicator_map class", {

  # Inject known years_with_obs and ensure the range is correctly derived
  mock_map_wide_range <- structure(
    list(
      years_with_obs = c(2000, 2002, 2005),
      dummy_data = NULL
    ),
    class = "indicator_map"
  )

  result <- get_observed_years(mock_map_wide_range)

  # Expected result: years 2000 to 2005
  expected_years <- 2000:2005
  # Expected occurrences: TRUE for 2000, 2002, 2005; FALSE otherwise
  expected_occurrences <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)

  expected_df <- data.frame(years = expected_years, occurrences = expected_occurrences)

  expect_equal(result, expected_df)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("years", "occurrences"))
})

# Test Block 2: indicator_ts class
test_that("get_observed_years correctly handles indicator_ts class", {

  result <- get_observed_years(mock_ts) # Years 2001, 2003

  # Expected result: years 2001 to 2003
  expected_years <- 2001:2003
  # Expected occurrences: TRUE for 2001, FALSE for 2002, TRUE for 2003
  expected_occurrences <- c(TRUE, FALSE, TRUE)

  expected_df <- data.frame(years = expected_years, occurrences = expected_occurrences)

  expect_equal(result, expected_df)
})

# Test Block 3: processed_cube class
test_that("get_observed_years correctly handles processed_cube class", {

  result <- get_observed_years(mock_cube) # Years 1999, 2001

  # Expected result: years 1999 to 2001
  expected_years <- 1999:2001
  # Expected occurrences: TRUE for 1999, FALSE for 2000, TRUE for 2001
  expected_occurrences <- c(TRUE, FALSE, TRUE)

  expected_df <- data.frame(years = expected_years, occurrences = expected_occurrences)

  expect_equal(result, expected_df)
})

# Test Block 4: Edge Cases (Single Year, Continuous Range)
test_that("get_observed_years handles single year and continuous range", {

  # Single Year (1995)
  mock_single_year <- structure(
    list(years_with_obs = 1995),
    class = "indicator_map"
  )
  result_single <- get_observed_years(mock_single_year)
  expect_equal(result_single$years, 1995)
  expect_true(result_single$occurrences)

  # Continuous Range (2010, 2011, 2012)
  mock_continuous <- structure(
    list(years_with_obs = 2010:2012),
    class = "indicator_map"
  )
  result_continuous <- get_observed_years(mock_continuous)
  expect_equal(result_continuous$years, 2010:2012)
  expect_true(all(result_continuous$occurrences))
})
