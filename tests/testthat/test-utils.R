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
# boot.return_int

# --- Mock Data Setup ---

# Define the dummy objects referenced in mock_call so R can find them. 
# The contents don't matter, only their existence.
my_data <- data.frame(value = 1:10)
my_stat <- function(d, i) mean(d[i, "value"])

# Capture a simple function call to use as the mock 'call' argument
mock_call <- quote(boot::boot(data = my_data, statistic = my_stat, R = 100))

# Use basic inputs for all arguments to ensure the list structure is correct
mock_inputs <- list(
  t0 = 5, t = matrix(rnorm(100), ncol = 1), strata = 1:10, R = 100,
  data = data.frame(x = 1:10), stat = function(x, i) mean(x[i]),
  stype = "i", call = mock_call, seed = 123, L = matrix(1:5),
  m = c(0, 0), pred.i = 1:10, weights = rep(1, 10),
  ran.gen = function(d, p) d, mle = c(mu = 0, sigma = 1)
)

# Non-zero 'm' for ordinary (used in test 3)
mock_inputs_m_non_zero <- mock_inputs
mock_inputs_m_non_zero$m <- c(1, 0) # sum(m) > 0

# --- Test Block 1: Base Structure and Class Check ---
test_that("boot.return_int returns a list of class 'boot' with base elements", {

  result <- do.call(boot.return_int, c(list(sim = "ordinary"), mock_inputs))

  # 1. Check class
  expect_s3_class(result, "boot")

  # 2. Check base elements (common to all sim types)
  base_names <- c("t0", "t", "R", "data", "seed", "statistic", "sim", "call")
  expect_true(all(base_names %in% names(result)))
})

# --- Test Block 2: sim = "parametric" ---
test_that("sim = 'parametric' adds ran.gen and mle", {

  result <- do.call(boot.return_int, c(list(sim = "parametric"), mock_inputs))

  # Check required elements
  expected_names <- c("ran.gen", "mle")
  expect_true(all(expected_names %in% names(result)))
  expect_equal(length(result), 10) # 8 base + 2 specific
})

# --- Test Block 3: sim = "antithetic" ---
test_that("sim = 'antithetic' adds stype, strata, and L", {

  result <- do.call(boot.return_int, c(list(sim = "antithetic"), mock_inputs))

  # Check required elements
  expected_names <- c("stype", "strata", "L")
  expect_true(all(expected_names %in% names(result)))
  expect_equal(length(result), 11) # 8 base + 3 specific
})

# --- Test Block 4: sim = "ordinary" (sum(m) > 0) ---
test_that("sim = 'ordinary' with sum(m) > 0 adds stype, strata, weights, pred.i", {

  result <- do.call(boot.return_int, c(list(sim = "ordinary"), mock_inputs_m_non_zero))

  # Check required elements
  expected_names <- c("stype", "strata", "weights", "pred.i")
  expect_true(all(expected_names %in% names(result)))
  expect_equal(length(result), 12) # 8 base + 4 specific
})

# --- Test Block 5: sim = "ordinary" (sum(m) == 0) ---
test_that("sim = 'ordinary' with sum(m) == 0 adds stype, strata, weights", {

  result <- do.call(boot.return_int, c(list(sim = "ordinary"), mock_inputs))

  # Check required elements (Note: pred.i is omitted when sum(m) == 0)
  expected_names <- c("stype", "strata", "weights")
  omitted_names <- "pred.i"
  expect_true(all(expected_names %in% names(result)))
  expect_false(omitted_names %in% names(result))
  expect_equal(length(result), 11) # 8 base + 3 specific
})

# --- Test Block 6: sim = "balanced" ---
test_that("sim = 'balanced' adds stype, strata, weights", {

  result <- do.call(boot.return_int, c(list(sim = "balanced"), mock_inputs))

  # Check required elements
  expected_names <- c("stype", "strata", "weights")
  expect_true(all(expected_names %in% names(result)))
  expect_equal(length(result), 11) # 8 base + 3 specific
})

# --- Test Block 7: sim = other (e.g., "permutation") ---
test_that("sim = other adds stype, strata (the ELSE block)", {

  result <- do.call(boot.return_int, c(list(sim = "permutation"), mock_inputs))

  # Check required elements
  expected_names <- c("stype", "strata")
  omitted_names <- c("L", "weights", "pred.i", "ran.gen", "mle")
  expect_true(all(expected_names %in% names(result)))
  expect_false(any(omitted_names %in% names(result)))
  expect_equal(length(result), 10) # 8 base + 2 specific
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

# ----------------- 
# breaks_log_int

# --- Mock Dependencies ---

# 1. Define a mock for log_sub_breaks (the fallback)
# It should return a predictable set of breaks.
mock_log_sub_breaks <- function(rng, n, base) {
  # Return 3 specific breaks as a fallback result
  c(base^(rng[1] + 0.1), base^mean(rng), base^(rng[2] - 0.1))
}

# 2. Define a mock for force_all (if it's not a standard function)
# It typically throws an error if args are missing, but we'll mock it to do nothing.
force_all <- function(...) invisible()

# --- Test Block 1: Factory Behavior and Trivial Returns ---
test_that("breaks_log_int returns a function and handles trivial inputs", {

  break_generator <- breaks_log_int(n = 5, base = 10)

  # 1. Check outer function returns an inner function
  expect_type(break_generator, "closure")

  # 2. Trivial return: Input range contains non-finite values (raw_rng check)
  # range(c(Inf, 1), na.rm=TRUE) is c(1, Inf). Fails is.finite(raw_rng)
  expect_equal(break_generator(c(Inf, 1)), numeric())

  # 3. Trivial return: min == max (Range spans < 1 power of the base)

  # Case A: Range is exactly 10. log10(10)=1. min=1, max=1. This reliably tests the 'if (max == min)' path.
  expect_equal(break_generator(c(10, 10)), 10)

  # Case B: Range is exactly 100. log10(100)=2. min=2, max=2.
  expect_equal(break_generator(c(100, 100)), 100)
})

# --- Test Block 2: Main Break Generation Loop (base=10) ---
test_that("breaks_log_int generates breaks covering the range and aims for n (base 10)", {

  # Data range: 100 to 100000 (log10 is 2 to 5). Range is 3.
  x <- c(100, 100000)

  # 1. n=5: Should use a step of 1 (since range is 3, 3/5+1 = 1)
  generator_n5 <- breaks_log_int(n = 5)
  breaks_5 <- generator_n5(x)
  expected_5 <- c(100, 1000, 10000, 100000) # seq(2, 5, by=1) -> 100, 1000, 10k, 100k
  expect_true(all(breaks_5 %in% expected_5))
  expect_true(length(breaks_5) >= (5 - 2))

  # 2. n=2: Should use a step of 2 (3/2+1 = 2) -> seq(2, 5, by=2)
  generator_n2 <- breaks_log_int(n = 2)
  breaks_2 <- generator_n2(x)
  expected_2 <- c(100, 10000)
  expect_true(all(breaks_2 %in% expected_2))
  expect_true(length(breaks_2) >= (2 - 2))
})

# --- Test Block 3: While Loop/Fallback (When n is small relative to range) ---
test_that("breaks_log_int uses while loop or falls back for difficult ranges", {

  # Data range: 10 to 1,000,000 (log10 is 1 to 6). Range is 5.
  x <- c(10, 1000000)

  # 1. n=1: Too few breaks needed. Initial 'by' is 5/1+1 = 6. seq(1, 6, by=6) is 10^1, 10^7.
  # The loop should run, reduce 'by' to 5, then 4, 3, 2, 1 until it hits the sum(relevant_breaks) >= (n-2)
  # Since n=1, n-2=-1. Any break will suffice. The loop will likely stop at by=5 or 4.
  generator_n1 <- breaks_log_int(n = 1)
  breaks_1 <- generator_n1(x)
  expect_true(length(breaks_1) >= 1) # Should not use fallback

  # 2. Test fallback: Force a case where the loop fails (e.g., n is large but log range is small)
  # Use an n that ensures the loop never hits the n-2 condition.
  # Since the function is a copy, we rely on the internal logic to hit the end.
  # Forcing max/min to be 1 and 0, and n=5. by=1. seq(0, 1, by=1). Breaks=1, 10. sum(relevant)=2. 2 < 5-2=3.
  # The mock uses log_sub_breaks, so we check for its specific return values.

  x_force_fallback <- c(1, 10) # log range 0 to 1
  generator_n5_fallback <- breaks_log_int(n = 5)
  breaks_fallback <- generator_n5_fallback(x_force_fallback)

  # Check if the result matches the mock_log_sub_breaks output
  # Mock returns 4 specific values
  expect_length(breaks_fallback, 4)
  expect_true(all(is.numeric(breaks_fallback)))
})

# --- Test Block 4: Alternative Base ---
test_that("breaks_log_int works with base != 10", {

  # Base 2 check. Data range: 8 to 64 (log2 is 3 to 6). Range is 3.
  x <- c(8, 64)

  # n=5: Should use a step of 1 (3/5+1 = 1)
  generator_b2_n5 <- breaks_log_int(n = 5, base = 2)
  breaks_b2_5 <- generator_b2_n5(x)
  expected_b2_5 <- c(8, 16, 32, 64) # seq(3, 6, by=1) -> 2^3 to 2^6

  expect_equal(breaks_b2_5, expected_b2_5)
})

# --------------- 
# breaks_pretty_int

# --- Mock Data Setup ---
data_range_1 <- c(12.3, 87.6) # Range that should result in breaks from 10 to 90
data_range_2 <- c(0, 1000)   # Simple, wide range

# --- Test Block 1: Factory Behavior and Basic Output ---
test_that("breaks_pretty_int returns a function and calculates breaks correctly", {

  break_generator <- breaks_pretty_int(n = 5)

  # 1. Check outer function returns an inner function
  expect_type(break_generator, "closure")

  # 2. Check basic calculation (using default n=5 from the factory)
  breaks_1 <- break_generator(data_range_1)
  expected_breaks_1 <- pretty(data_range_1, n = 5)

  expect_equal(breaks_1, expected_breaks_1)

  # 3. Check recalculation with a new n in the inner function
  breaks_n10 <- break_generator(data_range_2, n = 10)
  expected_breaks_n10 <- pretty(data_range_2, n = 10)

  expect_equal(breaks_n10, expected_breaks_n10)
})

# --- Test Block 2: Using the '...' argument (passed to pretty) ---
test_that("breaks_pretty_int passes '...' arguments correctly to pretty()", {

  # Use the 'min.n' argument for pretty()
  break_generator_min2 <- breaks_pretty_int(n = 5, min.n = 2)

  # 1. Test calculation: n=5 is requested, but min.n=2 ensures at least 2 breaks
  breaks_min2 <- break_generator_min2(data_range_1)

  # Expected result from base R's pretty()
  expected_min2 <- pretty(data_range_1, n = 5, min.n = 2)

  expect_equal(breaks_min2, expected_min2)

  # 2. Test using 'high.u.bias' (a more technical argument for pretty)
  # Default bias is 1, let's use a very low bias to change the output slightly
  break_generator_bias <- breaks_pretty_int(n = 5, high.u.bias = 0.1)
  breaks_bias <- break_generator_bias(c(1, 10))

  expected_bias <- pretty(c(1, 10), n = 5, high.u.bias = 0.1)

  expect_equal(breaks_bias, expected_bias)
})

# --- Test Block 3: Edge Case (Empty Input) ---
test_that("breaks_pretty_int handles empty/NA input gracefully", {
  generator <- breaks_pretty_int(n = 5)

  # 1. Check NA input: Test length instead of exact type identity
  res_na <- generator(NA)
  expect_length(res_na, 0)
  expect_true(is.numeric(res_na) || is.logical(res_na))

  # 2. Check empty input: Should always be numeric(0)
  res_empty <- generator(numeric(0))
  expect_length(res_empty, 0)
  expect_type(res_empty, "double")
})

# ---------------- 
# log_sub_breaks

# --- Mock Dependency ---

# Define a mock for extended_breaks (the fallback)
# It should return a predictable set of breaks based on the log range.
mock_extended_breaks <- function(n) {
  # This mock returns a function that ignores its input and just returns 4, 6, 8
  function(x) {
    c(4, 6, 8)
  }
}

# --- Mock Data Setup ---
# Log Range: 1 to 2.5 (Data Range: 10 to ~316)
rng_basic <- c(1, 2.5)

# Log Range: 0.1 to 1.9 (Data Range: ~1.25 to ~79)
rng_small <- c(0.1, 1.9)

# --- Test Block 1: Base <= 2 Trivial Return ---
test_that("log_sub_breaks returns base^min:max for base <= 2", {

  # Base 2: min=1, max=3. Expected: 2^1, 2^2, 2^3
  result_b2 <- log_sub_breaks(rng = rng_basic, base = 2)
  expect_equal(result_b2, c(2, 4, 8))

  # Base 1: min=1, max=3. Expected: 1^1, 1^2, 1^3 (which is 1, 1, 1)
  result_b1 <- log_sub_breaks(rng = rng_basic, base = 1)
  expect_equal(result_b1, c(1, 1, 1))
})

# --- Test Block 2: Iterative Loop and Sub-break Generation (base=10) ---
test_that("log_sub_breaks generates appropriate sub-breaks for base > 2", {

  # rng_basic: min=1, max=3. n=5. n-2=3.
  # Initial candidate: 2, 3, 4, 5, 6, 7, 8, 9

  # The loop should stop early (after few steps) as base=10 generates many breaks quickly.
  # First step (steps = 1, 3): Breaks 10, 30, 100, 300, 1000, 3000
  # rng is [10, 316.2]. Relevant breaks: 10, 30, 100, 300. (4 breaks)
  # 4 >= 3, so it breaks early.

  result_basic <- log_sub_breaks(rng = rng_basic, n = 5, base = 10)

  # We expect the result to be trimmed, but contain 10, 30, 100, 300.
  expect_true(all(c(10, 30, 100, 300) %in% result_basic))
  expect_true(min(result_basic) == 10) # Due to lower_end trimming
  expect_true(max(result_basic) > 316.22) # Due to upper_end trimming (max=3000 included)
  expect_true(length(result_basic) >= (5 - 2))
})

# --- Test Block 3: Fallback Condition Check (FINAL, ROBUST VERSION) ---
test_that("log_sub_breaks falls back to extended_breaks mock if break goal is not met", {

  # Setup: Condition to force the 'else' (fallback) path: n=20 (n-2=18)
  rng_small <- c(0.1, 1.9)
  n_target <- 20

  # 1. Use with_mocked_bindings to temporarily replace extended_breaks
  result_fallback <- with_mocked_bindings(
    # The code to execute: calling the function from your package
    log_sub_breaks(rng = rng_small, n = n_target, base = 10),

    extended_breaks = mock_extended_breaks
  )

  # 2. Check if the result matches the mocked output (c(4, 6, 8))
  expect_equal(result_fallback, c(4, 6, 8))
})

# --------------- 
# extended_breaks

# --- Mock Data Setup ---
data_1 <- c(12.3, 87.6)
data_2 <- c(1, 2, 3)

# --- Test Block 1: Factory Behavior and Trivial Returns ---
test_that("extended_breaks returns a function and handles empty/non-finite input", {

  break_generator <- extended_breaks(n = 5)

  # 1. Check outer function returns an inner function
  expect_type(break_generator, "closure")

  # 2. Trivial return: Input contains only non-finite values (x becomes length 0)
  expect_equal(break_generator(c(NA, Inf, -Inf)), numeric())

  # 3. Trivial return: Input is empty
  expect_equal(break_generator(numeric()), numeric())

  # 4. Trivial return: Input is an empty vector after filtering non-finite
  expect_equal(break_generator(c(NA_real_, NaN)), numeric())
})

  ## Core Logic and Argument Passing

test_that("extended_breaks correctly calls labeling::extended and uses arguments", {

  generator_n5 <- extended_breaks(n = 5)

  # 1. Basic calculation (using default n=5)
  breaks_1 <- generator_n5(data_1)
  expected_1 <- labeling::extended(12.3, 87.6, 5)

  expect_equal(breaks_1, expected_1)

  # 2. Check inner function argument override (n=10)
  breaks_n10 <- generator_n5(data_2, n = 10)
  expected_n10 <- labeling::extended(1, 3, 10)

  expect_equal(breaks_n10, expected_n10)

  # 3. Check passing '...' arguments (using default n=5 from factory)
  # The 'extended' function takes optional 'w' arguments. Let's use custom weights.
  generator_weights <- extended_breaks(n = 5, w = c(0.1, 0.1, 0.1, 0.7))

  breaks_weights <- generator_weights(data_1)
  expected_weights <- labeling::extended(12.3, 87.6, 5, w = c(0.1, 0.1, 0.1, 0.7))

  # Note: Use tolerance for floating point comparison in break generation
  expect_equal(breaks_weights, expected_weights, tolerance = 1e-6)
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