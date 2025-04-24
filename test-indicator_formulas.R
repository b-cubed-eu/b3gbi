# Mock data for testing
mock_species_abundance <- c(10, 5, 3, 2, 1)
mock_species_hierarchy <- list(
  "Species A" = c("Family A", "Order A", "Class A", "Phylum A", "Kingdom A"),
  "Species B" = c("Family B", "Order B", "Class A", "Phylum A", "Kingdom A"),
  "Species C" = c("Family C", "Order C", "Class B", "Phylum B", "Kingdom B"),
  "Species D" = c("Family D", "Order D", "Class B", "Phylum B", "Kingdom B"),
  "Species E" = c("Family E", "Order E", "Class C", "Phylum C", "Kingdom C")
)

mock_species_data <- data.frame(scientificName = names(mock_species_hierarchy))

# Tests for compute_evenness_formula
test_that("compute_evenness_formula calculates Pielou's evenness correctly", {
  evenness_pielou <- compute_evenness_formula(
    mock_species_abundance,
    type = "pielou_evenness"
    )
  expect_equal(round(evenness_pielou, 3), 0.723)
})

test_that("compute_evenness_formula calculates Williams' evenness correctly", {
  evenness_williams <- compute_evenness_formula(
    mock_species_abundance,
    type = "williams_evenness"
    )
  expect_equal(round(evenness_williams, 3), 0.589)
})

test_that("compute_evenness_formula handles NaN values correctly", {
  abundance_nan <- c(0, 0, 0, 0, 0)
  evenness_nan <- compute_evenness_formula(
    abundance_nan,
    type = "pielou_evenness"
    )
  expect_true(is.na(evenness_nan))
})

# Tests for compute_tax_distinct_formula
test_that(
  "compute_tax_distinct_formula calculates taxonomic distinctness correctly", {
  skip_if_not_installed("taxize") # Skip if taxize is not installed
  tax_distinct <- compute_tax_distinct_formula(
    mock_species_data,
    mock_species_hierarchy
    )
  expect_equal(round(tax_distinct, 3), 2.2)
})

test_that("compute_tax_distinct_formula returns NA for less than 3 species", {
  tax_distinct_short <- compute_tax_distinct_formula(
    mock_species_data[1:2, ],
    mock_species_hierarchy
    )
  expect_true(is.na(tax_distinct_short))
})

test_that(
  "compute_tax_distinct_formula throws error if taxize is not installed", {
    with_mocked_bindings(
      "requireNamespace" = function(package, quietly) FALSE,
      {
        expect_error(
          compute_tax_distinct_formula(
            mock_species_data, mock_species_hierarchy
          ),
          "The taxize package is required to calculate taxonomic distinctness."
        )
      }
    )
  })
