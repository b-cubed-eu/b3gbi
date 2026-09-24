# Mock data for testing
mock_species_abundance <- c(10, 5, 3, 2, 1)

# Tests for compute_evenness_formula
test_that("compute_evenness_formula calculates Pielou's evenness correctly", {
  evenness_pielou <- compute_evenness_formula(
    mock_species_abundance,
    type = "pielou_evenness"
    )
  expect_equal(round(evenness_pielou, 3), 0.834)
})

test_that("compute_evenness_formula calculates Williams' evenness correctly", {
  evenness_williams <- compute_evenness_formula(
    mock_species_abundance,
    type = "williams_evenness"
    )
  expect_equal(round(evenness_williams, 3), 0.621)
})

test_that("compute_evenness_formula handles NaN values correctly", {
  abundance_nan <- c(0, 0, 0, 0, 0)
  evenness_nan <- compute_evenness_formula(
    abundance_nan,
    type = "pielou_evenness"
    )
  expect_true(is.na(evenness_nan))
  evenness_nan <- compute_evenness_formula(
    abundance_nan,
    type = "williams_evenness"
  )
  expect_true(is.na(evenness_nan))
})

# Tests for compute_tax_distinct_formula are in test-tax_distinct.R

