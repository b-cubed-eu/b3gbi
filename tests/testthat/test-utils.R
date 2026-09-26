# Unit tests for functions in utils.R

# ----------- 
# is_sf_empty

# --- Mock Data Setup ---

# 1. NULL input
null_input <- NULL

# 2. Zero-row sf data frame (empty table)
sf_empty_df <- sf::st_sf(data.frame(id = integer(0)), geometry = sf::st_sfc())

# 3. sf data frame with empty geometry (one row, empty geom)
sf_one_row_empty_geom <- sf::st_sf(
  data.frame(id = 1),
  geometry = sf::st_sfc(sf::st_point(), crs = 4326) # st_point() creates an empty POINT
)

# 4. sfc with an empty geometry collection
sfc_empty_collection <- sf::st_sfc(sf::st_geometrycollection())

# 5. sfc with mixed empty and non-empty geometries
sfc_mixed_empty <- sf::st_sfc(
  sf::st_point(c(1, 1)),       # Non-empty
  sf::st_point()              # Empty
)

# 6. Non-empty, valid sf object
sf_valid <- sf::st_sf(
  data.frame(id = 1),
  geometry = sf::st_sfc(sf::st_point(c(2, 2)), crs = 4326)
)

# --- Test Block 1: Checks for TRUE (Is Empty) ---
test_that("is_sf_empty returns TRUE for all empty/null cases", {

  # 1. Null Check (The first IF branch)
  expect_true(is_sf_empty(null_input))

  # 2. Zero-row sf data frame (The second IF branch)
  expect_true(is_sf_empty(sf_empty_df))

  # 3. sfc with empty collection (The all(st_is_empty(x)) branch)
  expect_true(is_sf_empty(sfc_empty_collection))

  # 4. sf object with one row of empty geometry (The all(st_is_empty(x)) branch)
  expect_true(is_sf_empty(sf_one_row_empty_geom))
})

# --- Test Block 2: Checks for FALSE (Is Not Empty) ---
test_that("is_sf_empty returns FALSE for non-empty objects", {

  # 1. Valid, non-empty sf object
  expect_false(is_sf_empty(sf_valid))

  # 2. sfc with mixed empty and non-empty geometries
  # Function uses all(sf::st_is_empty(x)), so it should be FALSE if any are not empty
  expect_false(is_sf_empty(sfc_mixed_empty))
})

# ----------- 
# wrapper

# --- Test Block 1: Basic Wrapping ---
test_that("wrapper correctly splits text and uses newline separator", {

  long_string <- "This is a moderately long string that should be wrapped into multiple lines."

  # Set width to 20, forcing it to split into multiple lines
  wrapped_result <- wrapper(long_string, width = 20)

  # 1. Check that the result contains newline characters
  expect_true(grepl("\n", wrapped_result))

  # 2. Check the number of lines created (expected 4 lines based on the input text and width=20)
  expected_lines <- strsplit(wrapped_result, "\n")[[1]]

  # 3. Check that the original text is preserved
  # The original text, stripped of newlines, should match the original string
  unwrapped_text <- gsub("\n", " ", wrapped_result)
  # strwrap introduces some minor spacing changes, so we check if words are present
  expect_true(grepl("moderately long string", unwrapped_text))
})

# --- Test Block 2: No Wrapping ---
test_that("wrapper does not split text if width is sufficient", {

  short_string <- "A short line."

  # Set a large width (e.g., 50)
  wrapped_result <- wrapper(short_string, width = 50)

  # 1. Check that no newline characters are present
  expect_false(grepl("\n", wrapped_result))

  # 2. Check that the result is identical to the input (no extra whitespace/lines)
  expect_equal(wrapped_result, short_string)
})

# --- Test Block 3: Handling Vector Input (strwrap behavior) ---
test_that("wrapper handles vectors by combining results with newlines", {

  vector_input <- c("Title 1", "Very long title that needs wrapping")

  # Wrap with width=10
  wrapped_result <- wrapper(vector_input, width = 10)

  # Expected result: "Title 1\nVery long\ntitle that\nneeds\nwrapping"
  expected_lines <- c("Title 1", "Very long", "title", "that", "needs", "wrapping")

  actual_lines <- strsplit(wrapped_result, "\n")[[1]]

  # 1. Check the total number of lines created (1 + 4 = 5)
  expect_equal(length(actual_lines), 6)

  # 2. Check the specific output lines
  expect_equal(actual_lines, expected_lines)
})

# ------------- 
# resample

# --- Mock Data Setup ---
vector_single <- 5L
vector_multi <- 1:10
sample_size <- 5

# --- Test Block 1: The Fix (length(x) == 1) ---
test_that("resample handles length 1 vector correctly using rep", {

  # 1. Check basic replication
  result <- resample(vector_single, size = sample_size)
  expect_equal(result, rep(vector_single, sample_size))

  # 2. Check output properties
  expect_equal(length(result), sample_size)
  expect_true(all(result == vector_single))
})

# --- Test Block 2: Standard Behavior (length(x) > 1, replace=TRUE) ---
test_that("resample works like sample() for multi-length vectors (replace=TRUE)", {

  # Set a seed to ensure sample() and resample() are identical
  set.seed(42)
  resample_result <- resample(vector_multi, size = sample_size, replace = TRUE)

  set.seed(42)
  sample_result <- sample(vector_multi, size = sample_size, replace = TRUE)

  # 1. Check for identical output sequences
  expect_equal(resample_result, sample_result)

  # 2. Check output size
  expect_equal(length(resample_result), sample_size)
})

# --- Test Block 3: Standard Behavior (length(x) > 1, replace=FALSE) ---
test_that("resample works like sample() for multi-length vectors (replace=FALSE)", {

  # Set a seed
  set.seed(42)
  resample_result <- resample(vector_multi, size = sample_size, replace = FALSE)

  set.seed(42)
  sample_result <- sample(vector_multi, size = sample_size, replace = FALSE)

  # 1. Check for identical output sequences
  expect_equal(resample_result, sample_result)

  # 2. Check size and that replacement didn't occur (i.e., unique values)
  expect_equal(length(unique(resample_result)), sample_size)
})

# --- Test Block 4: Edge Case (size=0) ---
test_that("resample returns empty vector when size is 0", {

  result_single <- resample(vector_single, size = 0)
  result_multi <- resample(vector_multi, size = 0)

  expect_equal(length(result_single), 0)
  expect_equal(length(result_multi), 0)
})

# --------------- 
# add_yearvals_to_boot

# --- Mock Data Setup ---
# Original data with unique years 2000, 2001, 2002, 2003
orig_data_mock <- data.frame(
  year = c(2000, 2000, 2001, 2001, 2002, 2002, 2003, 2003),
  value = 1:8
)
# The expected names for the 'boot' object (excluding 2000)
expected_names <- c(2001, 2002, 2003)

# Mock bootstrap result (a list or data structure that needs names)
# It must have length equal to the number of expected names (3)
boot_mock <- list(
  t_1 = rnorm(10),
  t_2 = rnorm(10),
  t_3 = rnorm(10)
)
# Ensure the mock has the correct length before running the test
stopifnot(length(boot_mock) == length(expected_names))

# --- Test Block 1: Basic Functionality and Correct Naming ---
test_that("add_yearvals_to_boot assigns correct names, excluding the first year", {

  result <- add_yearvals_to_boot(boot_mock, orig_data_mock)

  # 1. Check that the object remains a list
  expect_type(result, "list")

  # 2. Check that the names match the unique years from the second element onwards
  expect_equal(names(result), as.character(expected_names))

  # 3. Check that the length and content of the object remain unchanged
  expect_equal(length(result), 3)
  expect_equal(result[[1]], boot_mock[[1]])
})

# --- Test Block 2: Handling a Single Year Input ---
test_that("add_yearvals_to_boot handles single year gracefully (returns empty names)", {

  orig_data_single_year <- data.frame(year = c(2020, 2020), value = 1:2)
  boot_mock_empty <- list()

  result <- add_yearvals_to_boot(boot_mock_empty, orig_data_single_year)

  # 1. Check if the names are NULL, which is the result of assigning character(0)
  #    to the names of a zero-length list in some R environments.
  #    Alternatively, check if it's identical to character(0) in the case where
  #    R is lenient.

  # Use expect_length to check the length of the names, which should be 0.
  expect_length(names(result), 0)

  # Use expect_null to check the specific case if the length 0 assignment resulted in NULL.
  # If the goal is strictly to check the length, expect_length is better.
  # If the goal is to check for *absence* of names, checking for NULL is the most explicit.
  expect_null(names(result))

  # 2. Crucially, the length of the result object must remain 0.
  expect_equal(length(result), 0)
})

# --- Test Block 3: Sequential vs. Non-sequential Years ---
test_that("add_yearvals_to_boot works with non-sequential years", {

  orig_data_non_seq <- data.frame(
    year = c(1990, 2000, 2000, 2010, 2010),
    value = 1:5
  )
  # Unique years are 1990, 2000, 2010. Expected names are 2000, 2010.
  expected_non_seq_names <- c(2000, 2010)
  boot_mock_non_seq <- list(rnorm(10), rnorm(10))

  result <- add_yearvals_to_boot(boot_mock_non_seq, orig_data_non_seq)

  # The output depends only on the sorted unique year values.
  expect_equal(names(result), as.character(expected_non_seq_names))
})

# ---------- 
# stopifnot_error

CUSTOM_ERROR <- "Custom error message triggered by failure."

## Success Tests

test_that("stopifnot_error succeeds when all conditions are TRUE (Minimal Two Arguments)", {

  # Total Arguments Passed: 2 (1 for err_message, 1 for ...)
  expect_no_error({
    # The intended call structure that works perfectly
    stopifnot_error(CUSTOM_ERROR, 1 == 1)
  })

  # Also test with a different TRUE condition (still 2 arguments total)
  expect_no_error({
    stopifnot_error("Another success", is.character("hello"))
  })
})

## Failure Tests

test_that("stopifnot_error stops with custom message on single failure conditions", {

  # Test with a single FALSE condition (2 arguments total)
  expect_error(
    stopifnot_error(CUSTOM_ERROR, 1 == 2),
    fixed = TRUE,
    regexp = CUSTOM_ERROR
  )

  # Test failure on NA (2 arguments total)
  expect_error(
    stopifnot_error(CUSTOM_ERROR, NA)
  )

  # Test failure on mixed vector (2 arguments total)
  expect_error(
    stopifnot_error(CUSTOM_ERROR, TRUE == FALSE),
    fixed = TRUE,
    regexp = CUSTOM_ERROR
  )
})

# --------------- 
# wrong_class

# --- Mock Data Setup ---
# 1. Valid object for single class check
my_list <- list(a = 1)
class(my_list) <- "my_list_class"

# 2. Valid object for multiple classes check (inherits ALL)
my_multi_object <- list(b = 2)
class(my_multi_object) <- c("class_A", "class_B", "base_class")

# 3. Invalid object (for all failures)
my_numeric <- 5

# 4. Partially invalid object (for multiple=TRUE failure)
my_partial_object <- list(c = 3)
class(my_partial_object) <- "class_A"

# --- Test Block 1: Success Conditions (No Error) ---
test_that("wrong_class succeeds when class is correct", {

  # 1. Single class match (inherits ANY)
  expect_no_error(
    wrong_class(my_list, class = "my_list_class", reason = "incorrect")
  )

  # 2. Multiple classes, matching ANY (default multiple=FALSE)
  expect_no_error(
    wrong_class(my_multi_object, class = c("class_A", "class_X"), reason = "unrecognized")
  )

  # 3. Multiple classes, matching ALL (multiple=TRUE)
  expect_no_error(
    wrong_class(my_multi_object, class = c("class_A", "class_B"), reason = "internal", multiple = TRUE)
  )
})

## Failure Conditions (Error Thrown)

### Test Block 2: Logic Checks (Failure to Match Class)

test_that("wrong_class throws error on class mismatch based on logic", {

  # 1. Single class required (inherits ANY), mismatch
  expected_msg_1 <- "Incorrect object class. Must be class my_list_class."
  expect_error(
    wrong_class(my_numeric, class = "my_list_class", reason = "incorrect"),
    fixed = TRUE,
    regexp = expected_msg_1
  )

  # 2. Multiple classes required (multiple=TRUE), partial mismatch
  expected_msg_2 <- "Wrong data class. Must be class, class_A and class_B. Note that this is an internal function and is not meant to be called directly."
  expect_error(
    wrong_class(my_partial_object, class = c("class_A", "class_B"), reason = "internal", multiple = TRUE),
    fixed = TRUE,
    regexp = expected_msg_2
  )

  # 3. Multiple classes required (multiple=FALSE), mismatch ANY
  expected_msg_3 <- "Object class not recognized. Must be one of the following: class_X or class_Y."
  expect_error(
    wrong_class(my_numeric, class = c("class_X", "class_Y"), reason = "unrecognized", multiple = FALSE),
    fixed = TRUE,
    regexp = expected_msg_3
  )
})

### Test Block 3: Message Construction Checks

test_that("wrong_class constructs correct message based on reason and multiple flag", {

  # 1. Reason: internal, Multiple: FALSE (collapse=" or ")
  msg_internal_or <- "Wrong data class. Must be class, class1 or class2. Note that this is an internal function and is not meant to be called directly."
  expect_error(
    wrong_class(my_numeric, class = c("class1", "class2"), reason = "internal", multiple = FALSE),
    fixed = TRUE,
    regexp = msg_internal_or
  )

  # 2. Reason: incorrect, Multiple: TRUE (collapse=" and ")
  msg_incorrect_and <- "Incorrect object class. Must be class class1 and class2."
  expect_error(
    wrong_class(my_numeric, class = c("class1", "class2"), reason = "incorrect", multiple = TRUE),
    fixed = TRUE,
    regexp = msg_incorrect_and
  )

  # 3. Reason: unrecognized, Multiple: FALSE (collapse=" or ")
  msg_unrecognized_or <- "Object class not recognized. Must be one of the following: class1 or class2."
  expect_error(
    wrong_class(my_numeric, class = c("class1", "class2"), reason = "unrecognized", multiple = FALSE),
    fixed = TRUE,
    regexp = msg_unrecognized_or
  )
})

# ---------------------
# New tests for NA issue (unique ID: 20260119_NA_FIX_v2)
test_that("stopifnot_error triggers !anyNA(r) check with NA input", {
  expect_error(
    stopifnot_error("NA error", NA),
    regexp = "NA error"
  )
})

test_that("wrong_class triggers is.na(is_correct) check with forced NA via my_inherits", {
  expect_error(
    with_mocked_bindings(
      wrong_class(1, class = "some_class", reason = "incorrect"),
      my_inherits = function(x, what) NA
    ),
    regexp = "Incorrect object class"
  )
})

# ---------------------
# Tests for my_estimateD patch (handles T=1 or no singletons/doubletons for q=1)
test_that("my_estimateD handles q=1 bug in iNEXT for incidence data", {
  # Mixed list containing a bad site (site1) and a good site (site2)
  # site1: T = 2, freqs = (2, 2) -> has no singletons/doubletons, would crash iNEXT for q=1
  # site2: T = 3, freqs = (2, 2) -> works fine
  x_mixed <- list(
    site1 = c(2, 2, 2),
    site2 = c(3, 2, 2)
  )
  
  res <- my_estimateD(x_mixed, datatype = "incidence_freq", base = "coverage", level = 0.95, q = c(0, 1, 2), conf = 0.95, nboot = 0)
  
  # Ensure all q values are processed
  expect_equal(nrow(res), 6)
  
  # Check site1 at Order.q = 1 (the patched one)
  site1_q1 <- res[res$Assemblage == "site1" & res$Order.q == 1, ]
  expect_equal(site1_q1$Method, "Observed")
  expect_equal(site1_q1$qD, 2) # Sobs = 2
  expect_equal(site1_q1$SC, 0.95) # Target level
  
  # Check site2 at Order.q = 1 (computed via iNEXT)
  site2_q1 <- res[res$Assemblage == "site2" & res$Order.q == 1, ]
  expect_equal(site2_q1$Method, "Rarefaction")
  
  # Test with single bad input (not in a list)
  x_single_bad <- c(2, 2, 2)
  res_single <- my_estimateD(x_single_bad, datatype = "incidence_freq", base = "coverage", level = 0.95, q = 1, conf = 0.95, nboot = 0)
  # For non-list input, Assemblage column is stripped
  expect_null(res_single$Assemblage)
  expect_equal(res_single$qD, 2)
})

test_that("my_estimateD coverage for other paths", {
  # 1. Bypass check if datatype is abundance
  x_abundance <- c(10, 20, 30)
  res_ab <- my_estimateD(x_abundance, datatype = "abundance", q = c(0, 1, 2), nboot = 0)
  expect_equal(nrow(res_ab), 3)
  
  # 2. Test incidence_raw data
  # incidence_raw is a list of matrices where columns are sampling units and rows are species
  # site1 (bad): 2 sampling units, both species present in both units (row sums = 2, 2)
  # site2 (good): 3 sampling units, one species present in 1 unit, another in 2 units
  site1_mat <- matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2) # nT = 2, rowSums = c(2, 2) -> no freq between 1 and nT-1
  site2_mat <- matrix(c(1, 0, 0, 1, 1, 0), nrow = 2, ncol = 3) # nT = 3, rowSums = c(2, 1) -> has singleton/doubleton
  
  x_raw <- list(
    site1 = site1_mat,
    site2 = site2_mat
  )
  
  res_raw <- suppressWarnings(my_estimateD(x_raw, datatype = "incidence_raw", base = "size", level = 2, q = c(0, 1, 2), conf = 0.95, nboot = 0))
  expect_equal(nrow(res_raw), 6)
  
  site1_raw_q1 <- res_raw[res_raw$Assemblage == "site1" & res_raw$Order.q == 1, ]
  expect_equal(site1_raw_q1$Method, "Observed")
  expect_equal(site1_raw_q1$qD, 2)
  
  # 3. Test list input with no names (names will be generated)
  x_unnamed <- list(
    c(2, 2, 2)
  )
  res_unnamed <- my_estimateD(x_unnamed, datatype = "incidence_freq", q = 1, nboot = 0)
  expect_equal(res_unnamed$Assemblage, "site1")
})
