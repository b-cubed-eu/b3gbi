# ==============================================================================
# 1. SETUP & MOCKS (Run this block first to enable testing)
# ==============================================================================

library(mockery)

create_dummy_map <- function() {
  p1 <- sf::st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0))))
  p2 <- sf::st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0))))
  geom <- sf::st_sfc(p1, p2, crs = 4326)
  # Row 1: 10.5 (Valid), Row 2: NA (Invalid)
  values <- c(10.5, NA)

  structure(
    list(data = list(geometry = geom, diversity_val = values), div_type = "species_richness"),
    class = "indicator_map"
  )
}

# ==============================================================================
# 2. TESTS
# ==============================================================================

describe("plot_mv() logic", {

  test_that("It correctly filters NAs from the data passed to mapview", {
    dummy <- create_dummy_map()

    # --- THE MOCK ---
    # We intercept the call to 'mapview::mapview'.
    # Instead of making a map, we just return the dataframe 'x' so we can inspect it.
    stub(plot_mv, "mapview::mapview", function(x, ...) {
      return(x)
    })

    # --- EXECUTION ---
    # This will now return the dataframe, NOT a map object
    result_data <- plot_mv(dummy, filter_NA = TRUE)

    # --- ASSERTION ---
    # Now nrow works perfectly because 'result_data' is just the clean SF object
    expect_equal(nrow(result_data), 1)
    expect_equal(result_data$indicator_value, 10.5)
  })

  test_that("It keeps NAs when filter_NA = FALSE", {
    dummy <- create_dummy_map()

    # Mock mapview to return the data again
    stub(plot_mv, "mapview::mapview", function(x, ...) {
      return(x)
    })

    result_data <- plot_mv(dummy, filter_NA = FALSE)

    expect_equal(nrow(result_data), 2)
    expect_true(any(is.na(result_data$indicator_value)))
  })

  test_that("It passes the correct arguments (legend title) to mapview", {
    dummy <- create_dummy_map()

    # Mock mapview to capture the arguments passed to it
    stub(plot_mv, "mapview::mapview", function(x, layer.name, ...) {
      return(layer.name) # Return the layer name argument
    })

    # Run with a custom title
    captured_title <- plot_mv(dummy, legend_title = "TEST TITLE")

    expect_equal(captured_title, "TEST TITLE")
  })

  test_that("It validates input class", {
    # We don't need to mock mapview here because it fails before calling it
    bad_input <- list(data = "garbage")
    expect_error(plot_mv(bad_input), "Incorrect object class.")
  })
})
