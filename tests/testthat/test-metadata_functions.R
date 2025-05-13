# Mock the 'available_indicators' object for testing get_indicator_name and get_legend_title
available_indicators_mock <- list(
  obs_richness = list(indicator_name = "Observed Species Richness", legend_label = "Observed Richness"),
  hill1 = list(indicator_name = "Hill Number of Order 1", legend_label = "Hill 1"),
  cum_richness = list(indicator_name = "Cumulative Species Richness", legend_label = "Cumulative Richness")
)

# Override the environment to use the mock object
test_that("get_indicator_name retrieves the correct name", {
  expect_equal(get_indicator_name("obs_richness"), "Observed Species Richness")
  expect_equal(get_indicator_name("hill1"), "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)")
  expect_equal(get_indicator_name("cum_richness"), "Cumulative Species Richness")
  expect_error(get_indicator_name("invalid_indicator"), "Indicator class is not registered. Check that you typed it correctly")
})

test_that("get_legend_title retrieves the correct legend label", {
  expect_equal(get_legend_title("obs_richness"), "Observed Richness")
  expect_equal(get_legend_title("hill1"), "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)")
  expect_equal(get_legend_title("cum_richness"), "Richness")
  expect_error(get_legend_title("another_invalid"), "Indicator class is not registered. Check that you typed it correctly.")
})

# Create mock indicator_map, indicator_ts, and processed_cube objects for
# testing get_observed_years
mock_indicator_map <- structure(list(years_with_obs = 2000:2005),
                                class = "indicator_map")
mock_indicator_ts <- structure(list(
  data = data.frame(year = c(2001, 2003, 2005))
  ), class = "indicator_ts")
mock_processed_cube <- structure(list(
  data = data.frame(year = c(1990, 1992, 1994, 1992))
  ), class = "processed_cube")
mock_wrong_class <- list(data = data.frame(year = c(2001, 2003, 2005)))

test_that("get_observed_years works correctly for indicator_map", {
  result <- get_observed_years(mock_indicator_map)
  expected_df <- data.frame(years = 2000:2005, occurrences = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(result, expected_df)
})

test_that("get_observed_years works correctly for indicator_ts", {
  result <- get_observed_years(mock_indicator_ts)
  expected_df <- data.frame(years = 2001:2005,
                            occurrences = c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(result, expected_df)
})

test_that("get_observed_years works correctly for processed_cube", {
  result <- get_observed_years(mock_processed_cube)
  expected_df <- data.frame(years = 1990:1994,
                            occurrences = c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(result, expected_df)
})

test_that("get_observed_years errors for objects of wrong class", {
  expect_error(get_observed_years(mock_wrong_class))
})

# Create mock data objects for testing list_species
mock_object_with_species_names <- list(species_names = c("Species A",
                                                         "Species B"))
mock_object_with_data_frame <- list(
  data = data.frame(taxonKey = c(1, 2, 1),
                    scientificName = c("Species C", "Species D", "Species C"))
)
mock_object_with_empty_species_names <- list(
  species_names = character(0),
  data = data.frame(taxonKey = 3, scientificName = "Species E")
)
mock_object_with_no_species_info <- list(data = data.frame(other_col = 1:2))

test_that("list_species returns species_names if available", {
  expect_equal(list_species(mock_object_with_species_names), c("Species A",
                                                               "Species B"))
})

test_that(
  "list_species returns distinct taxonKey and scientificName data frame", {
  result <- list_species(mock_object_with_data_frame)
  expected_df <- data.frame(taxonKey = c(1, 2), scientificName = c("Species C",
                                                                   "Species D"))
  expect_equal(result, expected_df)
})

test_that(
  paste0(
    "list_species returns distinct taxonKey and scientificName data ",
    "frame when species_names is empty"
  ), {
    result <- list_species(mock_object_with_empty_species_names)
    expected_df <- data.frame(taxonKey = 3, scientificName = "Species E")
    expect_equal(result, expected_df)
  })

test_that("list_species errors for objects with wrong class", {
  expect_error(list_species(mock_object_with_no_species_info))
})
