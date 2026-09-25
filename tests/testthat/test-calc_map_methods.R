
# Mock input data for map functions (reused)
mock_map_data <- data.frame(
  cellid = c(1, 1, 2, 2),
  cellCode = c("A1", "A1", "A2", "A2"),
  scientificName = c("Species A", "Species B", "Species A", "Species C"),
  obs = c(1, 1, 1, 1),
  taxonKey = c("spA", "spB", "spA", "spC"),
  year = c(1992, 1992, 1993, 1993)
)

# Create mock input with appropriate classes (reused)
mock_map_hill0 <- structure(mock_map_data, class = c("hill0", "data.frame"))
mock_map_hill1 <- structure(mock_map_data, class = c("hill1", "data.frame"))
mock_map_hill2 <- structure(mock_map_data, class = c("hill2", "data.frame"))

test_that("calc_map.hill0 calculates correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      if (datatype == "incidence_freq" && all(q == 0)) {
        expected_data <- list(
          `1` = c(1, 1, 1),
          `2` = c(1, 1, 1)
        )
        expect_equal(data, expected_data)
        return(tibble::tibble(
          Assemblage = c("1", "2"),
          qD = c(2, 2),
          m = c(2, 2),
          SC = c(level, level),
          Order.q = 0,
          Method = c("Extrapolation", "Extrapolation")
        ))
      } else {
        return(tibble::tibble(Assemblage = character(),
                              qD = numeric(),
                              Order.q = numeric(),
                              m = numeric(),
                              SC = numeric(),
                              Order.q = numeric(),
                              Method = character()))
      }
    },
    {
      result <- calc_map.hill0(
        mock_map_hill0,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_result <- tibble::tibble(
    cellid = c(1, 2),
    cellCode = c("A1","A2"),
    diversity_val = c(2, 2),
    samp_size_est = c(2, 2),
    coverage = c(0.8, 0.8),
    diversity_type = c(0, 0),
    method = c("Extrapolation", "Extrapolation")
  )

  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("calc_map.hill1 calculates correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      if (datatype == "incidence_freq" && all(q == 1)) {
        expected_data <- list(
          `1` = c(1,1,1),
          `2` = c(1,1,1)
        )
        expect_equal(data, expected_data)
        return(tibble::tibble(
          Assemblage = c("1", "2"),
          qD = c(2, 2),
          Order.q = 1,
          m = c(2, 2),
          SC = c(level, level),
          Method = c("Extrapolation", "Extrapolation")
        ))
      } else {
        return(tibble::tibble(Assemblage = character(),
                              qD = numeric(),
                              m = numeric(),
                              SC = numeric(),
                              Order.q = numeric(),
                              Method = character()))
      }
    },
    {
      result <- calc_map.hill1(
        mock_map_hill1,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill1 <- tibble::tibble(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(2, 2),
    samp_size_est = c(2, 2),
    coverage = c(0.8, 0.8),
    diversity_type = c(1, 1),
    method = c("Extrapolation", "Extrapolation")
  )

  expect_equal(result, expected_hill1, tolerance = 1e-6)
})

test_that("calc_map.hill2 calculates correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      if (datatype == "incidence_freq" && all(q == 2)) {
        expected_data <- list(
          `1` = c(1, 1, 1),
          `2` = c(1, 1, 1)
        )
        expect_equal(data, expected_data)
        return(tibble::tibble(
          Assemblage = c("1", "2"),
          qD = c(2, 2),
          Order.q = 2,
          m = c(2, 2),
          SC = c(level, level),
          Method = c("Extrapolation", "Extrapolation")
        ))
      } else {
        return(tibble::tibble(Assemblage = character(),
                              qD = numeric(),
                              m = numeric(),
                              SC = numeric(),
                              Order.q = numeric()))
      }
    },
    {
      result <- calc_map.hill2(
        mock_map_hill2,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill2 <- tibble::tibble(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(2, 2),
    samp_size_est = c(2, 2),
    coverage = c(0.8, 0.8),
    diversity_type = c(2, 2),
    method = c("Extrapolation", "Extrapolation")
  )

  expect_equal(result, expected_hill2, tolerance = 1e-6)
})

test_that("calc_map.hill0 handles empty data correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill0(
        mock_map_hill0,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_result <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_result)
})

test_that("calc_map.hill1 handles empty data correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill1(
        mock_map_hill1,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill1 <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_hill1)
})

test_that("calc_map.hill2 handles empty data correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill2(
        mock_map_hill2,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill2 <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_hill2)
})

test_that("calc_map.hill0 handles NA values correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill0(
        mock_map_hill0,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_result <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_result)
})

test_that("calc_map.hill1 handles NA values correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill1(
        mock_map_hill1,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill1 <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_hill1)
})

test_that("calc_map.hill2 handles NA values correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill2(
        mock_map_hill2,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill2 <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_hill2)
})

test_that("calc_map.hill0 handles non-numeric data correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill0(
        mock_map_hill0,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_result <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_result)
})

test_that("calc_map.hill1 handles non-numeric data correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill1(
        mock_map_hill1,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill1 <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_hill1)
})

test_that("calc_map.hill2 handles non-numeric data correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill2(
        mock_map_hill2,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill2 <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_hill2)
})

test_that("calc_map.hill0 handles non-standard data types correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill0(
        mock_map_hill0,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_result <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_result)
})

test_that("calc_map.hill1 handles non-standard data types correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill1(
        mock_map_hill1,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill1 <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_hill1)
})

test_that("calc_map.hill2 handles non-standard data types correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `my_estimateD` = function(data, q, datatype, base, level, ...) {
      return(tibble::tibble(Assemblage = character(),
                            qD = numeric(),
                            m = numeric(),
                            SC = numeric(),
                            Order.q = numeric(),
                            Method = character()))
    },
    {
      result <- calc_map.hill2(
        mock_map_hill2,
        cutoff_length = 1,
        coverage = 0.8,
        data_type = "incidence",
        assume_freq = FALSE
      )
    }
  )

  expected_hill2 <- tibble::tibble(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric(),
    samp_size_est = numeric(),
    coverage = numeric(),
    diversity_type = numeric(),
    method = character()
  )

  expect_equal(result, expected_hill2)
})


# Mock input data for other calc_map functions
mock_map_data_other <- data.frame(
  cellid = c(1, 1, 2, 2, 1, 2),
  cellCode = c("A1", "A1", "A2", "A2", "A1", "A2"),
  scientificName = c("Species A",
                     "Species B",
                     "Species A",
                     "Species C",
                     "Species A",
                     "Species C"),
  obs = c(1, 1, 1, 1, 1, 1),
  taxonKey = c("spA", "spB", "spA", "spC", "spA", "spC"),
  year = c(2000, 2010, 2005, 2015, 2002, 2018),
  area = c(100, 100, 200, 200, 100, 200),
  resolution = c("10km", "10km", "10km", "10km", "10km", "10km")
)

mock_obs_richness <-
  structure(mock_map_data_other, class = c("obs_richness", "data.frame"))
mock_total_occ <-
  structure(mock_map_data_other, class = c("total_occ", "data.frame"))
mock_newness <-
  structure(mock_map_data_other, class = c("newness", "data.frame"))
mock_occ_density <-
  structure(mock_map_data_other, class = c("occ_density", "data.frame"))

test_that("calc_map.obs_richness calculates correctly", {
  result <- calc_map.obs_richness(mock_obs_richness)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(2, 2)
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.obs_richness handles NA values correctly", {
  mock_obs_richness_with_na <- mock_obs_richness
  mock_obs_richness_with_na$obs[1] <- NA
  result <- calc_map.obs_richness(mock_obs_richness_with_na)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(2, 2)
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.obs_richness handles empty data correctly", {
  mock_obs_richness_empty <- mock_obs_richness[0, ]
  result <- calc_map.obs_richness(mock_obs_richness_empty)
  expected_result <- data.frame(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric()
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.total_occ calculates correctly", {
  result <- calc_map.total_occ(mock_total_occ)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(3, 3)
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.total_occ handles NA values correctly", {
  mock_total_occ_with_na <- mock_total_occ
  mock_total_occ_with_na$obs[1] <- NA
  result <- calc_map.total_occ(mock_total_occ_with_na)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(2, 3)
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.total_occ handles empty data correctly", {
  mock_total_occ_empty <- mock_total_occ[0, ]
  result <- calc_map.total_occ(mock_total_occ_empty)
  expected_result <- data.frame(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric()
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.newness calculates correctly without min year", {
  result <- calc_map.newness(mock_newness)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(round(mean(c(2000, 2010, 2002))),
                      round(mean(c(2005, 2015, 2018))))
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.newness calculates correctly with min year", {
  result <- calc_map.newness(mock_newness, newness_min_year = 2006)
  # The initial calculation of the mean year should still be the same
  initial_mean_year_cell1 <- round(mean(c(2000, 2010, 2002))) # 2004
  initial_mean_year_cell2 <- round(mean(c(2005, 2015, 2018))) # 2013

  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(ifelse(initial_mean_year_cell1 > 2006,
                             initial_mean_year_cell1, NA),
                      ifelse(initial_mean_year_cell2 > 2006,
                             initial_mean_year_cell2, NA))
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.newness handles NA values correctly", {
  mock_newness_with_na <- mock_newness
  mock_newness_with_na$year[1] <- NA
  result <- calc_map.newness(mock_newness_with_na)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(round(mean(c(2010, 2002))),
                      round(mean(c(2005, 2015, 2018))))
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.newness handles empty data correctly", {
  mock_newness_empty <- mock_newness[0, ]
  result <- calc_map.newness(mock_newness_empty)
  expected_result <- data.frame(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric()
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.occ_density calculates correctly", {
  result <- calc_map.occ_density(mock_occ_density)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(3 / 100, 3 / 200)
  )
  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("calc_map.occ_density handles NA values correctly", {
  mock_occ_density_with_na <- mock_occ_density
  mock_occ_density_with_na$obs[1] <- NA
  result <- calc_map.occ_density(mock_occ_density_with_na)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(2 / 100, 3 / 200)
  )
  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("calc_map.occ_density handles empty data correctly", {
  mock_occ_density_empty <- mock_occ_density[0, ]
  result <- calc_map.occ_density(mock_occ_density_empty)
  expected_result <- data.frame(
    cellid = numeric(),
    cellCode = character(),
    diversity_val = numeric()
  )
  expect_equal(result, expected_result)
})



# Mock input data for evenness calculations
mock_evenness_data <- data.frame(
  cellid = c(1, 1, 1, 2, 2, 2),
  cellCode = c("A1", "A1", "A1", "A2", "A2", "A2"),
  taxonKey = c("spA", "spB", "spA", "spA", "spB", "spC"),
  obs = c(2, 1, 1, 1, 1, 1)
)

mock_williams_evenness <-
  structure(mock_evenness_data, class = c("williams_evenness", "data.frame"))
mock_pielou_evenness <-
  structure(mock_evenness_data, class = c("pielou_evenness", "data.frame"))

# Mock the compute_evenness_formula function
mock_compute_evenness_formula <- function(x, type) {
  if (type == "williams_evenness") {
    # Simplified mock for Williams' evenness (should return a single value)
    # Assuming Williams' evenness is sensitive to number of species and
    # total abundance
    num_species <- length(x[x > 0])
    total_abundance <- sum(x)
    if (num_species > 0) {
      return(total_abundance / num_species) # Just a placeholder calculation
    } else {
      return(NA)
    }
  } else if (type == "pielou_evenness") {
    # Simplified mock for Pielou's evenness (should return a value
    # between 0 and 1)
    # Assuming Pielou's evenness relates observed diversity to maximum
    # possible diversity
    num_species <- length(x[x > 0])
    total_abundance <- sum(x)
    if (num_species > 0 && total_abundance > 0) {
      return(num_species / log(total_abundance)) # Placeholder
    } else {
      return(NA)
    }
  }
  return(NA)
}

test_that("calc_map.williams_evenness calculates correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `compute_evenness_formula` = mock_compute_evenness_formula,
    {
      result <- calc_map.williams_evenness(mock_williams_evenness)
      expected_result <- data.frame(
        cellid = c(1, 2),
        cellCode = c("A1", "A2"),
        diversity_val = c((2 + 1 + 1) / 2, (1 + 1 + 1) / 3)
      )
      expect_equal(result, expected_result, tolerance = 1e-6)
    }
  )
})

test_that("calc_map.pielou_evenness calculates correctly", {
  testthat::skip_if_not_installed("mockr")
  mockr::with_mock(
    `compute_evenness_formula` = mock_compute_evenness_formula,
    {
      result <- calc_map.pielou_evenness(mock_pielou_evenness)
      expected_result <- data.frame(
        cellid = c(1, 2),
        cellCode = c("A1", "A2"),
        diversity_val = c(2 / log(2 + 1 + 1), 3 / log(1 + 1 + 1))
      )
      expect_equal(result, expected_result, tolerance = 1e-6)
    }
  )
})

test_that("calc_map_evenness_core handles cases with no occurrences", {
  testthat::skip_if_not_installed("mockr")
  mock_evenness_empty <- data.frame(
    cellid = integer(0),
    cellCode = character(0),
    taxonKey = character(0),
    obs = integer(0)
  )
  mock_williams_evenness_empty <-
    structure(mock_evenness_empty, class = c("williams_evenness", "data.frame"))
  mock_pielou_evenness_empty <-
    structure(mock_evenness_empty, class = c("pielou_evenness", "data.frame"))

  mockr::with_mock(
    `compute_evenness_formula` = mock_compute_evenness_formula,
    {
      result_williams <-
        calc_map.williams_evenness(mock_williams_evenness_empty)
      expected_result_empty <- tibble::tibble(
        cellid = integer(0),
        cellCode = character(0),
        diversity_val = numeric(0)
      )
      expect_equal(result_williams, expected_result_empty)

      result_pielou <- calc_map.pielou_evenness(mock_pielou_evenness_empty)
      expect_equal(result_pielou, expected_result_empty)
    }
  )
})

test_that("calc_map_evenness_core handles NA values in obs correctly", {
  testthat::skip_if_not_installed("mockr")
  mock_evenness_core_na <- data.frame(
    cellid = c(1, 1, 2, 2),
    cellCode = c("A1", "A1", "A2", "A2"),
    taxonKey = c("spA", "spB", "spA", "spC"),
    obs = c(2, NA, 1, 1)
  )

  mockr::with_mock(
    `compute_evenness_formula` = mock_compute_evenness_formula,
    {
      # Williams' Evenness Calculation (based on species across cells)
      # Cell 1: Species A (2), Species B (0). Total abundance = 2,
      #         Num Species = 1. Evenness = 2 / 1 = 2
      # Cell 2: Species A (1), Species B (0), Species C (1).
      #         Total abundance = 2, Num Species = 2. Evenness = 2 / 2 = 1
      result_williams <- calc_map_evenness_core(mock_evenness_core_na,
                                                type = "williams_evenness")
      expected_result_williams <- data.frame(
        cellid = c(1, 2),
        cellCode = c("A1", "A2"),
        diversity_val = c(2, 1)
      )
      expect_equal(result_williams, expected_result_williams, tolerance = 1e-6)

      # Pielou's Evenness Calculation (based on species across cells)
      # Cell 1: Num Species = 1, Total Abundance = 2.
      #         Evenness = 1 / log(2) ≈ 1.44
      # Cell 2: Num Species = 2, Total Abundance = 2.
      #         Evenness = 2 / log(2) ≈ 2.89
      result_pielou <- calc_map_evenness_core(mock_evenness_core_na,
                                              type = "pielou_evenness")
      expected_result_pielou <- data.frame(
        cellid = c(1, 2),
        cellCode = c("A1", "A2"),
        diversity_val = c(1 / log(2), 2 / log(2))
      )
      expect_equal(result_pielou, expected_result_pielou, tolerance = 1e-6)
    }
  )
})


# Mock input data for abundance rarity
mock_ab_rarity_data <- data.frame(
  cellid = c(1, 1, 2, 2, 1),
  cellCode = c("A1", "A1", "A2", "A2", "A1"),
  taxonKey = c("spA", "spB", "spA", "spC", "spA"),
  obs = c(2, 1, 1, 1, 1)
)
mock_ab_rarity <- structure(mock_ab_rarity_data,
                            class = c("ab_rarity", "data.frame"))

test_that("calc_map.ab_rarity calculates correctly", {
  result <- calc_map.ab_rarity(mock_ab_rarity)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(
      (1 / (3 / 4)) + (1 / (1 / 4)), # spA rarity + spB rarity in cell 1
      (1 / (1 / 2)) + (1 / (1 / 2))  # spA rarity + spC rarity in cell 2
    )
  )
  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("calc_map.ab_rarity handles NA values in obs correctly", {
  mock_ab_rarity_na <- data.frame(
    cellid = c(1, 1, 2, 2, 1),
    cellCode = c("A1", "A1", "A2", "A2", "A1"),
    taxonKey = c("spA", "spB", "spA", "spC", "spA"),
    obs = c(2, NA, 1, 1, 1)
  )
  mock_ab_rarity_na_classed <- structure(mock_ab_rarity_na,
                                         class = c("ab_rarity", "data.frame"))
  result <- calc_map.ab_rarity(mock_ab_rarity_na_classed)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(
      (1 / (2 / 2)), # spA rarity in cell 1 (spB is removed due to NA in obs)
      (1 / (1 / 2)) + (1 / (1 / 2))  # spA rarity + spC rarity in cell 2
    )
  )
  expect_equal(result$diversity_val,
               expected_result$diversity_val,
               tolerance = 1e-6)
})

test_that("calc_map.ab_rarity handles empty input", {
  mock_ab_rarity_empty <- data.frame(cellid = integer(0),
                                     cellCode = character(0),
                                     taxonKey = character(0),
                                     obs = numeric(0))
  mock_ab_rarity_empty_classed <- structure(mock_ab_rarity_empty,
                                            class = c("ab_rarity",
                                                      "data.frame"))
  result <- calc_map.ab_rarity(mock_ab_rarity_empty_classed)
  expected_result <- data.frame(cellid = integer(0),
                                cellCode = character(0),
                                diversity_val = numeric(0))
  expect_equal(result, expected_result)
})

# Mock input data for area rarity
mock_area_rarity_data <- data.frame(
  cellid = c(1, 1, 2, 2, 1),
  cellCode = c("A1", "A1", "A2", "A2", "A1"),
  taxonKey = c("spA", "spB", "spA", "spC", "spA")
)
mock_area_rarity <- structure(mock_area_rarity_data,
                              class = c("area_rarity",
                                        "data.frame"))

test_that("calc_map.area_rarity calculates correctly", {
  result <- calc_map.area_rarity(mock_area_rarity)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(
      (1 / (2 / 2)) + (1 / (1 / 2)), # spA rarity + spB rarity in cell 1
      (1 / (2 / 2)) + (1 / (1 / 2))  # spA rarity + spC rarity in cell 2
    )
  )
  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("calc_map.area_rarity handles NA values in cellid or taxonKey", {
  mock_area_rarity_na <- data.frame(
    cellid = c(1, NA, 2, 2, 1),
    cellCode = c("A1", "A1", "A2", "A2", "A1"),
    taxonKey = c("spA", "spB", NA, "spC", "spA")
  )
  mock_area_rarity_na_classed <- structure(mock_area_rarity_na,
                                           class = c("area_rarity",
                                                     "data.frame"))
  result <- calc_map.area_rarity(mock_area_rarity_na_classed)
  expected_result <- data.frame(
    cellid = c(1, 2),
    cellCode = c("A1", "A2"),
    diversity_val = c(
      (1 / (1 / 2)), # spA rarity (spB removed because cellid is NA)
      (1 / (1 / 2)) # spC rarity (NA taxonKey removed)
    )
  )
  expect_equal(result, expected_result, tolerance = 1e-6)
})

test_that("calc_map.area_rarity handles empty input", {
  mock_area_rarity_empty <- data.frame(cellid = integer(0),
                                       cellCode = character(0),
                                       taxonKey = character(0))
  mock_area_rarity_empty_classed <- structure(mock_area_rarity_empty,
                                              class = c("area_rarity",
                                                        "data.frame"))
  result <- calc_map.area_rarity(mock_area_rarity_empty_classed)
  expected_result <- data.frame(cellid = integer(0),
                                cellCode = character(0),
                                diversity_val = numeric(0))
  expect_equal(result, expected_result)
})



# Mock data for testing
mock_spec_occ_data <- tibble::tibble(
  cellid = c(1, 1, 2, 3, 3, 4),
  cellCode = c("A1", "A1", "A2", "B1", "B1", "B2"),
  taxonKey = c(1, 1, 2, 3, 3, 4),
  scientificName = c("Species A",
                     "Species A",
                     "Species B",
                     "Species C",
                     "Species C",
                     "Species D"),
  obs = c(5, 3, 2, 7, 2, 1)
)
mock_spec_occ_data <- structure(mock_spec_occ_data,
                                class = c("spec_occ", "data.frame"))

mock_spec_range_data <- tibble::tibble(
  cellid = c(1, 1, 2, 3, 3, 4),
  cellCode = c("A1", "A1", "A2", "B1", "B1", "B2"),
  taxonKey = c(1, 1, 2, 3, 3, 4),
  scientificName = c("Species A",
                     "Species A",
                     "Species B",
                     "Species C",
                     "Species C",
                     "Species D")
)
mock_spec_range_data <- structure(mock_spec_range_data,
                                  class = c("spec_range", "data.frame"))

# Tests for calc_map.spec_occ
test_that("calc_map.spec_occ returns correct output structure", {
  result <- calc_map.spec_occ(mock_spec_occ_data)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("cellid",
                         "cellCode",
                         "taxonKey",
                         "scientificName",
                         "diversity_val"))
})

test_that("calc_map.spec_occ calculates diversity_val correctly", {
  result <- calc_map.spec_occ(mock_spec_occ_data)
  expected_result <- structure(
    data.frame(
      cellid = c(1, 2, 3, 4),
      cellCode = c("A1", "A2", "B1", "B2"),
      taxonKey = c(1, 2, 3, 4),
      scientificName = c("Species A",
                         "Species B",
                         "Species C",
                         "Species D"),
      diversity_val = c(8, 2, 9, 1)
    ), class = c("spec_occ", "data.frame")
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.spec_occ handles empty input", {
  empty_data <- tibble::tibble(
    cellid = integer(),
    cellCode = character(),
    taxonKey = integer(),
    scientificName = character(),
    obs = numeric()
  )
  class(empty_data) <- c("spec_occ", "data.frame")
  result <- calc_map.spec_occ(empty_data)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result,
               c("cellid",
                 "cellCode",
                 "taxonKey",
                 "scientificName",
                 "diversity_val"))
})

test_that("calc_map.spec_occ handles single row input", {
  single_row_data <- structure(
    tibble::tibble(
      cellid = 5,
      cellCode = "C1",
      taxonKey = 5,
      scientificName = "Species E",
      obs = 10
    ), class = c("spec_occ", "data.frame")
  )
  result <- calc_map.spec_occ(single_row_data)
  expected_result <- structure(
    tibble::tibble(
      cellid = 5,
      cellCode = "C1",
      taxonKey = 5,
      scientificName = "Species E",
      diversity_val = 10
    ), class = c("spec_occ", "data.frame")
  )
  expect_equal(result, expected_result)
})

# Tests for calc_map.spec_range
test_that("calc_map.spec_range returns correct output structure", {
  result <- calc_map.spec_range(mock_spec_range_data)
  expect_s3_class(result, "data.frame")
  expect_named(
    result,
    c("cellid", "cellCode", "taxonKey", "scientificName", "diversity_val")
  )
})

test_that("calc_map.spec_range calculates diversity_val correctly (always 1)", {
  result <- calc_map.spec_range(mock_spec_range_data)
  expected_result <- structure(
    tibble::tibble(
      cellid = c(1, 2, 3, 4),
      cellCode = c("A1", "A2", "B1", "B2"),
      taxonKey = c(1, 2, 3, 4),
      scientificName = c("Species A", "Species B", "Species C", "Species D"),
      diversity_val = c(1, 1, 1, 1)
    ), class = c("spec_range", "data.frame")
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.spec_range handles empty input", {
  empty_data <- structure(
    tibble::tibble(
      cellid = integer(),
      cellCode = character(),
      taxonKey = integer(),
      scientificName = character()
    ), class = c("spec_range", "data.frame")
  )
  result <- calc_map.spec_range(empty_data)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(
    result,
    c("cellid", "cellCode", "taxonKey", "scientificName", "diversity_val")
  )
})

test_that("calc_map.spec_range handles single row input", {
  single_row_data <- structure(
    tibble::tibble(
      cellid = 5,
      cellCode = "C1",
      taxonKey = 5,
      scientificName = "Species E"
    ), class = c("spec_range", "data.frame")
  )
  result <- calc_map.spec_range(single_row_data)
  expected_result <- structure(
    tibble::tibble(
      cellid = 5,
      cellCode = "C1",
      taxonKey = 5,
      scientificName = "Species E",
      diversity_val = 1
    ), class = c("spec_range", "data.frame")
  )
  expect_equal(result, expected_result)
})

test_that("calc_map.spec_range handles duplicate species in the same cell", {
  duplicate_data <- structure(
    tibble::tibble(
      cellid = c(1, 1, 2),
      cellCode = c("A1", "A1", "B2"),
      taxonKey = c(1, 1, 2),
      scientificName = c("Species A", "Species A", "Species B")
    ), class = c("spec_range", "data.frame")
  )
  result <- calc_map.spec_range(duplicate_data)
  expected_result <- structure(
    tibble::tibble(
      cellid = c(1, 2),
      cellCode = c("A1", "B2"),
      taxonKey = c(1, 2),
      scientificName = c("Species A", "Species B"),
      diversity_val = c(1, 1)
    ), class = c("spec_range", "data.frame")
  )
  expect_equal(result, expected_result)
})

# Tests for calc_map.tax_distinct are in test-tax_distinct.R
