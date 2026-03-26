test_that("list_species returns a sorted tibble with only scientificName for processed_cube", {
  # Mock processed_cube
  cube_df <- data.frame(
    taxonKey = 1:2,
    scientificName = c("Vulpes vulpes", "Sciurus vulgaris"), # Unsorted input
    obs = 1,
    stringsAsFactors = FALSE
  )
  mock_cube <- structure(list(data = cube_df), class = "processed_cube")

  res <- list_species(mock_cube)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2)
  expect_equal(names(res), "scientificName")
  # Verify sorted order: Sciurus comes before Vulpes
  expect_equal(res$scientificName, c("Sciurus vulgaris", "Vulpes vulpes"))
})

test_that("list_species returns a sorted tibble for indicator with species_names vector", {
  # Mock object with species_names vector (traditional indicator style)
  # Include duplicates and unsorted names
  mock_ind <- list(species_names = c("Felis catus", "Canis familiaris", "Felis catus"))

  res <- list_species(mock_ind)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2) # Should be unique
  expect_equal(as.character(res$scientificName), c("Canis familiaris", "Felis catus")) # Sorted
})

test_that("list_species handles data frames directly and sorts them", {
  df <- data.frame(
    scientificName = c("Sp B", "Sp A", "Sp B"),
    taxonKey = 1:3,
    stringsAsFactors = FALSE
  )

  res <- list_species(df)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2) # Should be distinct
  expect_equal(names(res), "scientificName")
  expect_equal(res$scientificName, c("Sp A", "Sp B")) # Sorted
})

test_that("list_species warns and returns empty tibble for invalid input", {
  expect_warning(res <- list_species(list(none = 1)), "Could not find species list")
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 0)
  expect_equal(names(res), "scientificName")
})
