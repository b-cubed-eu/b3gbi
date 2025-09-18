# Mock data for testing
mock_species_abundance <- c(10, 5, 3, 2, 1)

# Part 1: The mock species hierarchy (a list of data frames)
create_mock_hierarchy <- function(species_list) {
  # Get all unique ranks in the desired order
  ranks <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")

  # Create a function to build the data frame for a single species
  build_df <- function(name, hierarchy) {
    full_hierarchy <- c(rev(hierarchy), name)

    # Create the data frame
    df <- data.frame(
      name = full_hierarchy,
      rank = ranks[1:length(full_hierarchy)],
      id = sample(1000000:9999999, length(full_hierarchy)) # Dummy IDs
    )
    # Reorder the columns to match the desired output
    df <- df[, c("name", "rank", "id")]
    return(df)
  }

  # Use lapply to apply the function to each species in the list
  mock_hierarchy <- lapply(
    seq_along(species_list),
    function(i) {
      build_df(names(species_list)[i], species_list[[i]])
    }
  )

  # Set the names of the resulting list
  names(mock_hierarchy) <- names(species_list)

  return(mock_hierarchy)
}

# The raw hierarchy data
mock_species_hierarchy_raw <- list(
  "Species A" = c("Family A", "Order A", "Class A", "Phylum A", "Kingdom A"),
  "Species B" = c("Family B", "Order B", "Class A", "Phylum A", "Kingdom A"),
  "Species C" = c("Family C", "Order C", "Class B", "Phylum B", "Kingdom B"),
  "Species D" = c("Family D", "Order D", "Class B", "Phylum B", "Kingdom B"),
  "Species E" = c("Family E", "Order E", "Class C", "Phylum C", "Kingdom C")
)

# Generate the mock hierarchy object
mock_species_hierarchy <- create_mock_hierarchy(mock_species_hierarchy_raw)

# Part 2: The mock species data (a data frame)
mock_species_data <- data.frame(scientificName = names(mock_species_hierarchy))

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

# Tests for compute_tax_distinct_formula
test_that(
  "compute_tax_distinct_formula calculates taxonomic distinctness correctly", {
  skip_if_not_installed("taxize") # Skip if taxize is not installed
  tax_distinct <- compute_tax_distinct_formula(
    mock_species_data,
    mock_species_hierarchy
    )
  expect_equal(round(tax_distinct, 3), 0.9)
})

test_that("compute_tax_distinct_formula returns NA for less than 3 species", {
  tax_distinct_short <- compute_tax_distinct_formula(
    mock_species_data[1:2, , drop = FALSE],
    mock_species_hierarchy
    )
  expect_true(is.na(tax_distinct_short))
})

