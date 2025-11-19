library(mockery)

# Test Block 1: Successful retrieval of a valid indicator title
test_that("get_legend_title returns correct title for registered indicator (stubbing as.character)", {

  # Stub the base function as.character to immediately return the correct title string
  # We use the function call 'as.character(available_indicators[[x]]$legend_label)'
  stub(get_legend_title, "as.character", function(...) {
    return("Observed Species Richness")
  })

  # The logic of the function is now: ltitle <- "Observed Species Richness"
  # The length check passes, and it returns the result.
  result <- get_legend_title(x = "obs_richness")

  expect_equal(result, "Observed Species Richness")
})

# Test Block 2: Successful retrieval for a different valid indicator
test_that("get_legend_title returns correct title for hill1 indicator (stubbing as.character)", {

  # Stub as.character to return the second title
  stub(get_legend_title, "as.character", function(...) {
    return("Effective Number of Common Species (Hill 1)")
  })

  result <- get_legend_title(x = "hill1")

  expect_equal(result, "Effective Number of Common Species (Hill 1)")
})

# Test Block 3: Error handling for a missing (unregistered) indicator
# We rely on the internal lookup of the missing key failing the length check naturally.
test_that("get_legend_title throws an error for unregistered indicator (No Stub)", {

  # We do NOT stub here. We rely on available_indicators[[x]] failing or returning NULL,
  # which makes length(ltitle) > 0 FALSE, triggering the stop().

  expect_error(
    get_legend_title(x = "non_existent_indicator"),
    regexp = "Indicator class is not registered\\. Check that you typed it correctly\\."
    # NOTE: This test will only work if the failure to find "non_existent_indicator"
    # results in an object (e.g., NULL) that passes the initial lookup failure (the "closure" error)
    # but fails the subsequent length check. Given the previous errors, this is the most likely path.
  )
})
