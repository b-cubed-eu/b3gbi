# --- Mock Data ---

# Case 1: Object with pre-calculated species_names vector
mock_object_vector <- list(
  species_names = c("Vulpes vulpes", "Sciurus vulgaris"),
  data = data.frame(a = 1) # irrelevant data
)

# Case 2: Object without species_names, requiring processing of object$data
mock_object_dataframe <- list(
  species_names = character(0), # Empty vector to trigger the 'else' block
  data = data.frame(
    taxonKey = c(101, 102, 101, 103), # 101 is duplicated
    scientificName = c("Felis catus", "Canis familiaris", "Felis catus", "Equus caballus"),
    other_col = 99
  )
)

# Expected final data frame output
expected_species_df <- data.frame(
  taxonKey = c(101, 102, 103),
  scientificName = c("Felis catus", "Canis familiaris", "Equus caballus")
)

# Mock 1: dplyr::select - Must return only the two columns needed by distinct()
mock_select_species <- function(data, ...) {
  # In a real test, we verify the arguments used, but here we just return the needed data
  # We manually simulate the selection based on the input data structure
  return(data.frame(
    taxonKey = data$taxonKey,
    scientificName = data$scientificName
  ))
}

# Mock 2: dplyr::distinct - Must return the unique rows
mock_distinct_species <- function(data, taxonKey, scientificName) {
  # Since the mock object has duplicates, we manually simulate the distinct operation
  # and verify that it received the correct selected columns.

  # Ensure the data frame passed only contains the expected two columns
  expect_named(data, c("taxonKey", "scientificName"))

  # Return the expected result after deduplication
  return(expected_species_df)
}


# Test Block 1: Return species names vector directly
test_that("list_species returns species_names vector when available", {

  result <- list_species(mock_object_vector)

  # The result should be the character vector
  expect_type(result, "character")
  expect_equal(result, c("Vulpes vulpes", "Sciurus vulgaris"))
})

# Test Block 2: Process object$data to extract unique species (Mocking used here)
test_that("list_species processes object$data when species_names is empty", {

  # We use local_mocked_bindings to ensure we control the dplyr operations
  result <- with_mocked_bindings(
    list_species(mock_object_dataframe),

    # Mock the functions that are called in the pipe
    select = mock_select_species,
    distinct = mock_distinct_species
  )

  # The result should be the deduplicated data frame
  expect_s3_class(result, "data.frame")
  expect_named(result, c("taxonKey", "scientificName"))
  expect_equal(result, expected_species_df)
})

# Test Block 3: Edge Case - object$species_names is NULL (should fall through to else block)
test_that("list_species handles NULL species_names gracefully by processing data", {

  mock_object_null <- list(
    species_names = NULL, # NULL is NOT handled by length(), but the function logic implies
    # it should check for the length of the vector, which NULL doesn't have.
    # Assuming a check like: if (!is.null(object$species_names) && length(object$species_names) > 0)
    data = mock_object_dataframe$data
  )

  # We test the actual logic: if (length(NULL) > 0) is FALSE, so it enters the else block.
  # If the function is loaded correctly, we rely on the internal functions.

  result <- with_mocked_bindings(
    list_species(mock_object_null),

    select = mock_select_species,
    distinct = mock_distinct_species
  )

  # Should still return the processed data frame
  expect_equal(result, expected_species_df)
})

