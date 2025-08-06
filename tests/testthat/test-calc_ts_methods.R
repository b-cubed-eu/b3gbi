# Mock input data for testing
mock_data <- data.frame(
  year = c(2001, 2001, 2002, 2002, 2002, 2003),
  taxonKey = c(101, 102, 101, 103, 104, 105)
)

# Assign the correct class for the input
class(mock_data) <- append(c("obs_richness"), class(mock_data))

# Tests
test_that("calc_ts.obs_richness calculates correctly", {
  result <- calc_ts.obs_richness(mock_data)

  expected_result <- data.frame(
    year = c(2001, 2002, 2003),
    diversity_val = c(2, 3, 1)
  )

  # Check that the result matches the expected calculation
  expect_equal(result, expected_result)
})

test_that("calc_ts.obs_richness throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002, 2003),
    taxonKey = c(101, 102, 103)
  )

  # Ensure error is thrown if the input class is incorrect
  expect_error(
    calc_ts.obs_richness(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.obs_richness handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      taxonKey = integer()),
                           class = c("obs_richness", "data.frame"))

  expected_empty_result <- data.frame(
    year = integer(),
    diversity_val = integer()
  )

  # Check that the function returns an appropriately empty result
  result <- calc_ts.obs_richness(empty_input)
  expect_equal(result, expected_empty_result)
})


# Mock input data for testing
mock_cum_rich_data <- data.frame(
  year = c(2001, 2001, 2002, 2003, 2003),
  taxonKey = c(101, 102, 103, 101, 104)
)

# Assign the correct class for the input
mock_cum_richness <- structure(mock_cum_rich_data, class = c("cum_richness",
                                                             "data.frame"))

# Tests
test_that("calc_ts.cum_richness calculates cumulative richness correctly", {
  result <- calc_ts.cum_richness(mock_cum_richness)

  expected_result <- data.frame(
    year = c(2001, 2002, 2003),
    diversity_val = c(2, 3, 4)
  )

  expect_equal(result, expected_result)
})

test_that("calc_ts.cum_richness throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002, 2003),
    taxonKey = c(101, 102, 103)
  )

  expect_error(
    calc_ts.cum_richness(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.cum_richness handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      taxonKey = integer()),
                           class = c("cum_richness", "data.frame"))

  expected_empty_result <- data.frame(
    year = integer(),
    diversity_val = integer()
  )

  result <- calc_ts.cum_richness(empty_input)
  expect_equal(result, expected_empty_result)
})


# Mock input data for testing total occurrences
mock_total_occ_data <- data.frame(
  year = c(2001, 2001, 2002, 2002, 2003),
  obs = c(10, 20, 5, 15, 25)
)

mock_total_occ <- structure(mock_total_occ_data, class = c("total_occ",
                                                           "data.frame"))

test_that("calc_ts.total_occ calculates total occurrences correctly", {
  result <- calc_ts.total_occ(mock_total_occ)

  expected_result <- data.frame(
    year = c(2001, 2002, 2003),
    diversity_val = c(30, 20, 25)
  )

  expect_equal(result, expected_result)
})

test_that("calc_ts.total_occ throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002, 2003),
    obs = c(10, 20, 30)
  )

  expect_error(
    calc_ts.total_occ(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.total_occ handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      obs = integer()),
                           class = c("total_occ", "data.frame"))

  expected_empty_result <- data.frame(
    year = integer(),
    diversity_val = integer()
  )

  result <- calc_ts.total_occ(empty_input)
  expect_equal(result, expected_empty_result)
})


# Mock input data for testing occurrence density
mock_occ_density_data <- data.frame(
  year = c(2001, 2002, 2003),
  cellid = c(1, 1, 1),
  obs = c(100, 200, 300),
  area = c(10, 10, 10)
)

mock_occ_density <- structure(mock_occ_density_data, class = c("occ_density",
                                                               "data.frame"))
mock_occ_density$resolution <- "1km"  # Mock the resolution attribute

test_that("calc_ts.occ_density calculates occurrence density correctly", {
  result <- calc_ts.occ_density(mock_occ_density)

  expected_result <- data.frame(
    year = c(2001, 2002, 2003),
    diversity_val = c(10, 20, 30)
  )

  expect_equal(result, expected_result)
})

test_that("calc_ts.occ_density throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002, 2003),
    cellid = c(1, 1, 1),
    obs = c(10, 20, 30),
    area = c(1, 1, 1)
  )

  expect_error(
    calc_ts.occ_density(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.occ_density handles non-km resolutions correctly", {
  # Creating a mock input with resolution in degrees will trigger the error
  mock_invalid_resolution <- mock_occ_density
  mock_invalid_resolution$resolution <- "1degree"

  expect_error(
    calc_ts.occ_density(mock_invalid_resolution),
    "choose a projected CRS that uses meters or kilometers"
  )
})

test_that("calc_ts.occ_density handles empty input gracefully", {
  # Create an empty data frame with the structure needed
  empty_input <- structure(
    data.frame(
      year = integer(),
      cellid = integer(),
      obs = integer(),
      area = integer()
    ),
    class = c("occ_density", "data.frame")
  )

  # Set attribute separately for an empty data frame
  attr(empty_input, "resolution") <- "1km"

  expected_empty_result <- data.frame(
    year = integer(),
    diversity_val = integer()
  )

  result <- calc_ts.occ_density(empty_input)
  expect_equal(result, expected_empty_result)
})


# Mock input data for newness calculation
mock_newness_data <- data.frame(
  year = c(2001, 2001, 2002, 2002, 2003),
  values = c(1, 2, 3, 4, 5)  # Dummy values
)

mock_newness <- structure(mock_newness_data, class = c("newness",
                                                       "data.frame"))

test_that("calc_ts.newness calculates newness correctly", {
  result <- calc_ts.newness(mock_newness)

  expected_result <- data.frame(
    year = c(2001, 2002, 2003),
    diversity_val = c(2001, 2002, 2002)
  )

  expect_equal(result, expected_result)
})

test_that("calc_ts.newness throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002, 2003),
    values = c(1, 2, 3)
  )

  expect_error(
    calc_ts.newness(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.newness handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      values = integer()),
                           class = c("newness", "data.frame"))

  expected_empty_result <- data.frame(
    year = integer(),
    diversity_val = logical()
  )

  result <- calc_ts.newness(empty_input)
  expect_equal(result, expected_empty_result)
})


# Mock input data for testing abundance-based rarity
mock_ab_rarity_data <- data.frame(
  year = c(2001, 2001, 2002, 2002),
  cellid = c(1, 1, 2, 2),
  taxonKey = c(101, 102, 101, 103),
  obs = c(10, 20, 5, 15)
)

mock_ab_rarity <- structure(mock_ab_rarity_data, class = c("ab_rarity",
                                                           "data.frame"))

test_that("calc_ts.ab_rarity calculates rarity correctly", {
  result <- calc_ts.ab_rarity(mock_ab_rarity)

  # Expected output should reflect aggregated results
  expected_result <- data.frame(
    year = c(2001, 2002),
    diversity_val = c(5.833333, 6.666667)
  )

  # Use tolerance to allow for minor floating point differences
  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("calc_ts.ab_rarity throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002),
    obs = c(10, 20)
  )

  expect_error(
    calc_ts.ab_rarity(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.ab_rarity handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      cellid = integer(),
                                      taxonKey = integer(),
                                      obs = integer()),
                           class = c("ab_rarity", "data.frame"))

  expected_empty_result <- data.frame(
    year = integer(),
    diversity_val = integer()
  )

  result <- calc_ts.ab_rarity(empty_input)
  expect_equal(result, expected_empty_result)
})


# Mock input data for testing area-based rarity
mock_area_rarity_data <- data.frame(
  year = c(2001, 2001, 2002, 2002),
  cellid = c(1, 1, 2, 2),
  taxonKey = c(101, 102, 101, 103)
)

mock_area_rarity <- structure(mock_area_rarity_data, class = c("area_rarity",
                                                               "data.frame"))

test_that("calc_ts.area_rarity calculates area-based rarity correctly", {
  result <- calc_ts.area_rarity(mock_area_rarity)

  # Expected output should reflect aggregated results
  expected_result <- data.frame(
    year = c(2001, 2002),
    diversity_val = c(3, 3)
  )

  expect_equal(result, expected_result)
})

test_that("calc_ts.area_rarity throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002),
    cellid = c(1, 2)
  )

  expect_error(
    calc_ts.area_rarity(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.area_rarity handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      cellid = integer(),
                                      taxonKey = integer()),
                           class = c("area_rarity", "data.frame"))

  expected_empty_result <- data.frame(
    year = integer(),
    diversity_val = integer()
  )

  result <- calc_ts.area_rarity(empty_input)
  expect_equal(result, expected_empty_result)
})


# Mock input data for testing species occurrence
mock_spec_occ_data <- data.frame(
  year = c(2001, 2001, 2002, 2002),
  taxonKey = c(101, 101, 102, 103),
  scientificName = c("Species A", "Species A", "Species B", "Species C"),
  obs = c(10, 10, 15, 5)
)

mock_spec_occ <- structure(mock_spec_occ_data, class = c("spec_occ",
                                                         "data.frame"))

test_that("calc_ts.spec_occ calculates species occurrences correctly", {
  result <- calc_ts.spec_occ(mock_spec_occ)

  expected_result <- structure(
    data.frame(
      year = c(2001, 2002, 2002),
      taxonKey = c(101, 102, 103),
      scientificName = c("Species A", "Species B", "Species C"),
      diversity_val = c(20, 15, 5)  # Expected based on summing obs
    ), class = c("spec_occ", "data.frame"))

  expect_equal(result, expected_result)
})

test_that("calc_ts.spec_occ throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002),
    taxonKey = c(101, 102),
    scientificName = c("Species A", "Species B")
  )

  expect_error(
    calc_ts.spec_occ(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.spec_occ handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      taxonKey = integer(),
                                      scientificName = character(),
                                      obs = integer()),
                           class = c("spec_occ", "data.frame"))

  expected_empty_result <- structure(
    data.frame(
      year = integer(),
      taxonKey = integer(),
      scientificName = character(),
      diversity_val = integer()
    ), class = c("spec_occ", "data.frame"))

  result <- calc_ts.spec_occ(empty_input)
  expect_equal(result, expected_empty_result)
})


# Mock input data for testing species range
mock_spec_range_data <- data.frame(
  year = c(2001, 2001, 2002, 2002),
  taxonKey = c(101, 102, 101, 102),
  cellCode = c(1, 1, 2, 2),
  obs = c(1, 3, 1, 1),
  scientificName = c("Species A", "Species B", "Species A", "Species B")
)

mock_spec_range <- structure(mock_spec_range_data, class = c("spec_range",
                                                             "data.frame"))

test_that("calc_ts.spec_range calculates species range correctly", {
  result <- calc_ts.spec_range(mock_spec_range)

  expected_result <- structure(
    data.frame(
      year = c(2001, 2002, 2001, 2002),
      taxonKey = c(101, 101, 102, 102),
      scientificName = c("Species A", "Species A", "Species B", "Species B"),
      diversity_val = c(1, 1, 1, 1)  # Sum based evaluation
    ), class = c("spec_range", "data.frame"))

  expect_equal(result, expected_result)
})

test_that("calc_ts.spec_range throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002),
    taxonKey = c(101, 102),
    scientificName = c("Species A", "Species B"),
    cellCode = c(1, 2)
  )

  expect_error(
    calc_ts.spec_range(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.spec_range handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      taxonKey = integer(),
                                      scientificName = character(),
                                      obs = integer(),
                                      cellCode = integer()),
                           class = c("spec_range", "data.frame"))

  expected_empty_result <- structure(data.frame(
    year = integer(),
    taxonKey = integer(),
    scientificName = character(),
    diversity_val = integer()
  ), class = c("spec_range", "data.frame"))

  result <- calc_ts.spec_range(empty_input)
  expect_equal(result, expected_empty_result)
})


# Define a mock data frame for testing
mock_tax_distinct_data <- data.frame(
  year = c(2001, 2001, 2002, 2003),
  scientificName = c("Species A", "Species B", "Species C", "Species D")
)

# Add class for the input
mock_tax_distinct <- structure(mock_tax_distinct_data, class = c("tax_distinct",
                                                                 "data.frame"))

# Mock return value for the taxize classification
mock_tax_hier <- list(
  `Species A` = list(rank = "species", name = "A"),
  `Species B` = list(rank = "species", name = "B"),
  `Species C` = list(rank = "species", name = "C"),
  `Species D` = list(rank = "species", name = "D")
)

test_that("calc_ts.tax_distinct calculates correctly", {
  # Mock the call to taxize::classification
  mockr::with_mock(
    `my_classification` = function(...) {
      message("taxize::classification called") # DEBUG
      mock_tax_hier
    },
    # Mock a return value for compute_tax_distinct_formula
    `compute_tax_distinct_formula` = function(.x, tax_hier) {
      message("compute_tax_distinct_formula called")  # DEBUG
      # Simply return a fixed value for simplicity
      return(1)
    },
    {
      result <- calc_ts.tax_distinct(mock_tax_distinct)
    }
  )

  expected_result <- tibble::tibble(
    year = c(2001, 2002, 2003),
    diversity_val = c(1, 1, 1)
  )
  expect_equal(result, expected_result)
})

test_that("calc_ts.tax_distinct handles missing taxize package", {
  with_mocked_bindings(
    my_classification = function(...) stop(
      "Please install the taxize package to use this function."
      ),

    {
      expect_error(calc_ts.tax_distinct(mock_tax_distinct),
                   "Please install the taxize package")
    }
  )
})

test_that("calc_ts.tax_distinct throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002, 2003),
    scientificName = c("Species A", "Species B", "Species C")
  )

  expect_error(
    calc_ts.tax_distinct(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.tax_distinct handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      scientificName = character()),
                           class = c("tax_distinct", "data.frame"))

  expected_empty_result <- tibble::tibble(
    year = integer(),
    diversity_val = numeric()
  )

  result <- calc_ts.tax_distinct(empty_input)
  expect_equal(result, expected_empty_result)
})


# Define mock input data for occurrence turnover
mock_occ_turnover_data <- data.frame(
  year = c(2001, 2002, 2002, 2003),
  taxonKey = c(101, 102, 103, 104)
)

# Adding the required class
mock_occ_turnover <- structure(mock_occ_turnover_data, class = c("occ_turnover", "data.frame"))

# Mock list_org_by_year's functionality
list_org_by_year <- function(data, var) {
  return(
    list(
      `2001` = c(101),
      `2002` = c(102, 103),
      `2003` = c(104)
    )
  )
}

test_that("calc_ts.occ_turnover calculates turnover correctly", {
  result <- calc_ts.occ_turnover(mock_occ_turnover)

  # Given mock data outcome: in/out/manipulated
  expected_result <- tibble::tibble(
    year = c(2001, 2002, 2003),
    diversity_val = c(NA, 1, 1)  # Example based on singular additions/losses
  )

  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("calc_ts.occ_turnover throws error on wrong class", {
  mock_invalid_input <- data.frame(
    year = c(2001, 2002),
    taxonKey = c(101, 103)
  )

  expect_error(
    calc_ts.occ_turnover(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.occ_turnover handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      taxonKey = integer()),
                           class = c("occ_turnover", "data.frame"))

  expected_empty_result <- tibble::tibble(
    year = integer(),
    diversity_val = numeric()
  )

  result <- calc_ts.occ_turnover(empty_input)
  expect_equal(result, expected_empty_result)
})



# Mock input data
mock_evenness_data <- data.frame(
  year = c(2001, 2002, 2003, 2001, 2002, 2003),
  taxonKey = c(1, 1, 1, 2, 2, 2),
  obs = c(10, 15, 10, 20, 10, 15)
)

# Create mock input with the appropriate class
mock_williams_evenness <- structure(mock_evenness_data, class = c("williams_evenness", "data.frame"))
mock_pielou_evenness <- structure(mock_evenness_data, class = c("pielou_evenness", "data.frame"))

test_that("calc_ts.williams_evenness calculates correctly", {
  # Mock the function to return a fixed value
  mockr::with_mock(
    `compute_evenness_formula` = function(...) {
      return(0.5)  # Mocked return value
    },
    # Call the function with the mock data
    {
      result <- calc_ts.williams_evenness(mock_williams_evenness)
    }
  )
  # Expected output should reflect the mocked return value
  expected_result <- data.frame(
    year = c(2001, 2002, 2003),
    diversity_val = c(0.5, 0.5, 0.5)  # All years should have the identical mocked evenness value
  )

  expect_equal(result, expected_result)
})

test_that("calc_ts.williams_evenness throws error on wrong class", {
  mock_invalid_input <- data.frame(year = c(2001, 2002), taxonKey = c(1, 2), obs = c(10, 20))

  expect_error(
    calc_ts.williams_evenness(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts.williams_evenness handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      taxonKey = integer(),
                                      obs = integer()),
                           class = c("williams_evenness", "data.frame"))

  expected_empty_result <- tibble::tibble(
    year = integer(),
    diversity_val = numeric()
  )

  result <- calc_ts.williams_evenness(empty_input)
  expect_equal(result, expected_empty_result)
})

test_that("calc_ts.pielou_evenness calculates correctly", {
  # Mock the function to return a fixed value
  mockr::with_mock(
    `compute_evenness_formula` = function(...) {
      return(0.5)  # Mocked return value
    },
    # Call the function with the mock data
    {
      result <- calc_ts.pielou_evenness(mock_pielou_evenness)
    }
  )
  # Expected output should reflect the mocked return value
  expected_result <- data.frame(
    year = c(2001, 2002, 2003),
    diversity_val = c(0.5, 0.5, 0.5)  # Consistent mocked return value
  )

  expect_equal(result, expected_result)
})

test_that("calc_ts.pielou_evenness handles empty input gracefully", {
  empty_input <- structure(data.frame(year = integer(),
                                      taxonKey = integer(),
                                      obs = integer()),
                           class = c("pielou_evenness", "data.frame"))

  expected_empty_result <- tibble::tibble(
    year = integer(),
    diversity_val = numeric()
  )

  result <- calc_ts.pielou_evenness(empty_input)
  expect_equal(result, expected_empty_result)
})

test_that("calc_ts.pielou_evenness throws error on wrong class", {
  mock_invalid_input <- data.frame(year = c(2001, 2002), taxonKey = c(1, 2), obs = c(10, 20))

  expect_error(
    calc_ts.pielou_evenness(mock_invalid_input),
    "Wrong data class."
  )
})

test_that("calc_ts_evenness_core handles empty input", {
  empty_input <- structure(data.frame(year = integer(), taxonKey = integer(), obs = integer()), class = c("data.frame", "sf"))

  expected_result <- tibble::tibble(
    year = integer(),
    diversity_val = double()  # Consistent with the empty sequence
  )

  result <- calc_ts_evenness_core(empty_input, type = "williams_evenness")
  expect_equal(result, expected_result)
})


# Mock input data
mock_hill_data <- data.frame(
  year = c(2001, 2002, 2001, 2002),
  scientificName = c("Species A", "Species B", "Species A", "Species C"),
  obs = c(10, 15, 5, 10),
  cellCode = c(1, 1, 2, 2)
)

# Create mock input with appropriate classes
mock_hill0 <- structure(mock_hill_data, class = c("hill0", "data.frame"))
mock_hill1 <- structure(mock_hill_data, class = c("hill1", "data.frame"))
mock_hill2 <- structure(mock_hill_data, class = c("hill2", "data.frame"))

# Mock return for iNEXT::estimateD
mock_estimateD <- tibble::tibble(
  Assemblage = c(2001, 2002),
  qD = c(2, 3), # Changed for Hill0 (species richness)
  t = c(2, 3),
  SC = c(0.8, 0.9),
  Order.q = c(0, 0),
  qD.LCL = c(1.5, 2.5),
  qD.UCL = c(2.5, 3.5)
)

test_that("calc_ts.hill0 calculates correctly", {
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      if (datatype == "incidence_raw" && all(q == 0)) {
        # Calculate species richness for each matrix in the list
        richness_by_year <- purrr::map2_df(
          data, names(data),
          function(matrix, year) {
            tibble::tibble(
              Assemblage = as.numeric(year),
              qD = nrow(matrix), # Number of rows is the number of species
              Order.q = 0
            )
          }
        ) %>%
          dplyr::mutate(t = qD,
                        SC = 1,
                        qD.LCL = qD,
                        qD.UCL = qD) # Add other required columns

        return(richness_by_year)
      } else {
        # Placeholder for other q values or datatypes
        return(tibble::tibble(Assemblage = numeric(),
                              qD = numeric(),
                              t = numeric(),
                              SC = numeric(),
                              Order.q = numeric(),
                              qD.LCL = numeric(),
                              qD.UCL = numeric()))
      }
    },
    {
      result <- calc_ts.hill0(
        mock_hill0,
        cutoff_length = 1, coverage = 0.8
      )
    }
  )

  # Compare result with expected data.
  expected_result <- tibble::tibble(
    year = c(2001, 2002),
    diversity_val = c(1, 2),
    samp_size_est = c(1, 2),
    coverage = c(1, 1),
    diversity_type = c(0, 0),
    ll = c(1, 2),
    ul = c(1, 2)
  )

  expect_equal(result %>% dplyr::select(year,
                                        diversity_val,
                                        samp_size_est,
                                        coverage,
                                        diversity_type,
                                        ll,
                                        ul),
               expected_result, tolerance = 1e-6)
})

test_that("calc_ts.hill1 calculates correctly", {
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      if (datatype == "incidence_raw" && all(q == 1)) {
        # Calculate Hill-1 (Shannon) for each matrix
        hill1_by_year <- purrr::map2_df(
          data, names(data),
          function(matrix, year) {
            abundances <- rowSums(matrix)
            proportions <- abundances[abundances > 0] / sum(abundances)
            shannon <- -sum(proportions * log(proportions))
            hill1 <- exp(shannon)
            tibble::tibble(
              Assemblage = as.numeric(year),
              qD = hill1,
              Order.q = 1
            )
          }
        ) %>%
          dplyr::mutate(t = qD, SC = 1,
                        qD.LCL = qD,
                        qD.UCL = qD) # Add other required columns

        return(hill1_by_year)
      } else {
        # Placeholder for other q values or datatypes
        return(tibble::tibble(Assemblage = numeric(),
                              qD = numeric(),
                              t = numeric(),
                              SC = numeric(),
                              Order.q = numeric(),
                              qD.LCL = numeric(),
                              qD.UCL = numeric()))
      }
    },
    {
      result <- calc_ts.hill1(
        mock_hill1,
        cutoff_length = 1, coverage = 0.8
      )
    }
  )

  # Expected Hill-1 values (calculate these based on your mock_hill_data)
  expected_hill1 <- tibble::tibble(
    year = c(2001, 2002),
    diversity_val = c(1.000, 2.000), # Replace with actual Hill-1 values
    samp_size_est = c(1.000, 2.000),
    coverage = c(1, 1),
    diversity_type = c(1, 1),
    ll = c(1.000, 2.000),
    ul = c(1.000, 2.000)
  )

  expect_equal(result %>% dplyr::select(year,
                                        diversity_val,
                                        samp_size_est,
                                        coverage,
                                        diversity_type,
                                        ll,
                                        ul),
               expected_hill1, tolerance = 1e-6)
})

test_that("calc_ts.hill2 calculates correctly", {
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      if (datatype == "incidence_raw" && all(q == 2)) {
        # Calculate Hill-2 (Inverse Simpson) for each matrix
        hill2_by_year <- purrr::map2_df(
          data, names(data),
          function(matrix, year) {
            abundances <- rowSums(matrix)
            proportions <- abundances[abundances > 0] / sum(abundances)
            simpson <- sum(proportions^2)
            hill2 <- 1 / simpson
            tibble::tibble(
              Assemblage = as.numeric(year),
              qD = hill2,
              Order.q = 2
            )
          }
        ) %>%
          dplyr::mutate(t = qD, SC = 1,
                        qD.LCL = qD,
                        qD.UCL = qD) # Add other required columns

        return(hill2_by_year)
      } else {
        # Placeholder for other q values or datatypes
        return(tibble::tibble(Assemblage = numeric(),
                              qD = numeric(),
                              t = numeric(),
                              SC = numeric(),
                              Order.q = numeric(),
                              qD.LCL = numeric(),
                              qD.UCL = numeric()))
      }
    },
    {
      result <- calc_ts.hill2(
        mock_hill2,
        cutoff_length = 1, coverage = 0.8
      )
    }
  )

  # Expected Hill-2 values (calculate these based on your mock_hill_data)
  expected_hill2 <- tibble::tibble(
    year = c(2001, 2002),
    diversity_val = c(1.000, 2.000), # Replace with actual Hill-2 values
    samp_size_est = c(1.000, 2.000),
    coverage = c(1, 1),
    diversity_type = c(2, 2),
    ll = c(1.000, 2.000),
    ul = c(1.000, 2.000)
  )

  expect_equal(result %>% dplyr::select(year,
                                        diversity_val,
                                        samp_size_est,
                                        coverage,
                                        diversity_type,
                                        ll,
                                        ul),
               expected_hill2, tolerance = 1e-6)
})
