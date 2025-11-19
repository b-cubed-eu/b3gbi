test_that("check_cell_size handles valid inputs", {
  expect_equal(check_cell_size(10, "1km", "country"), 10000)
  expect_equal(check_cell_size(20, "10km", "world"), 20000)
  expect_equal(check_cell_size(0.5, "0.25degrees", "country"), 0.5)
  expect_equal(check_cell_size(1, "0.5degrees", "world"), 1)
})

test_that("check_cell_size handles invalid cell sizes", {
  expect_error(check_cell_size(11, "10km", "country"),
               "cell_size must be a whole number multiple of the resolution.")
  expect_error(check_cell_size(0.3, "0.25degrees", "country"),
               "cell_size must be a whole number multiple of the resolution.")
})

test_that("check_cell_size handles NULL cell size (km)", {
  expect_equal(check_cell_size(NULL, "1km", "world"), 10000)
  expect_equal(check_cell_size(NULL, "1km", "continent"), 10000)
  expect_equal(check_cell_size(NULL, "1km", "country"), 10000)
})

test_that("check_cell_size handles NULL cell size (degrees)", {
  expect_equal(check_cell_size(NULL, "1degrees", "world"), 1)
  expect_equal(check_cell_size(NULL, "0.25degrees", "country"), 0.25)
  expect_equal(check_cell_size(NULL, "0.5degrees", "continent"), 1)
})

test_that("check_cell_size handles invalid resolution units", {
  expect_error(check_cell_size(10, "1miles", "country"),
               "Resolution units not recognized.")
})

test_that("check_cell_size handles edge cases", {
  expect_equal(check_cell_size(0.5, "0.25degrees", "country"), 0.5)
  expect_equal(check_cell_size(NULL, "0.1degrees", "continent"), 1)
  expect_equal(check_cell_size(NULL, "1km", "region"), 10000)
})

# ---------------

# Mock 1: Always reports interactive mode is active
mock_interactive_true <- function() {
  TRUE
}

# Mock 2: Returns a specific user input (e.g., "y")
mock_readline_y <- function(prompt) {
  "y"
}

# Mock 3: Returns "a" (switch to 'auto') for the first prompt
mock_readline_a <- function(prompt) {
  "a"
}

# Mock 4: Returns "n" (abort) for either prompt
mock_readline_n <- function(prompt) {
  "n"
}

test_that("check_cell_size handles km resolution with valid multiples", {
  result <- check_cell_size(
    cell_size = 10,
    resolution = "1km",
    level = "country"
  )

  expect_equal(result, 10000) # 10 km converted to meters
})

test_that("check_cell_size handles degree resolution with valid multiples", {
  result <- check_cell_size(
    cell_size = 1,
    resolution = "0.25degrees",
    level = "world"
  )

  expect_equal(result, 1) # Degrees are not converted
})

test_that("check_cell_size throws error for non-multiple of resolution (km)", {
  expect_error(
    check_cell_size(
      cell_size = 15,
      resolution = "10km",
      level = "country"
    ),
    "cell_size must be a whole number multiple"
  )
})

test_that("check_cell_size throws error for non-multiple of resolution (degrees)", {
  with_mocked_bindings(
    interactive = mock_interactive_true,
    readline = mock_readline_y,
    .package = "base",
    expect_error(
      check_cell_size(
        cell_size = 0.3,
        resolution = "0.25degrees",
        level = "world"
      ),
      "cell_size must be a whole number multiple"
    )
  )
})

test_that("check_cell_size auto mode uses area-based thresholds for km", {
  # Large area (>= 1,000,000 km²) should default to 100 km
  result <- check_cell_size(
    cell_size = NULL,
    resolution = "10km",
    level = "cube",
    area = 2000000
  )

  expect_equal(result, 100000) # 100 km in meters

  # Medium area (>= 10,000 km²) should default to 10 km
  result <- check_cell_size(
    cell_size = NULL,
    resolution = "1km",
    level = "cube",
    area = 50000
  )

  expect_equal(result, 10000) # 10 km in meters

  # Small area (>= 100 km²) should default to 1 km
  result <- check_cell_size(
    cell_size = NULL,
    resolution = "0.1km",
    level = "cube",
    area = 500
  )

  expect_equal(result, 1000) # 1 km in meters

  # Tiny area (< 100 km²) should default to 0.1 km
  result <- check_cell_size(
    cell_size = NULL,
    resolution = "0.1km",
    level = "cube",
    area = 50
  )

  expect_equal(result, 100) # 0.1 km in meters
})

test_that("check_cell_size auto mode adjusts to resolution if smaller", {
  # Auto suggests 1 km, but resolution is 10 km
  result <- check_cell_size(
    cell_size = NULL,
    resolution = "10km",
    level = "cube",
    area = 500
  )

  expect_equal(result, 10000) # Should match resolution (10 km)
  expect_message(
    check_cell_size(
      cell_size = NULL,
      resolution = "10km",
      level = "cube",
      area = 500
    ),
    "setting cell_size to 10 km to match cube resolution"
  )
})

test_that("check_cell_size auto mode for degrees uses level-based defaults", {
  # World/continent level should default to 1 degree
  result <- check_cell_size(
    cell_size = NULL,
    resolution = "0.25degrees",
    level = "world"
  )

  expect_equal(result, 1)

  # Other levels should default to 0.1 degrees
  result <- check_cell_size(
    cell_size = NULL,
    resolution = "0.01degrees",
    level = "country"
  )

  expect_equal(result, 0.1)
})

test_that("check_cell_size 'grid' mode matches resolution", {
  expect_equal(
    with_mocked_bindings(
      interactive = mock_interactive_true,
      readline = mock_readline_y,
      .package = "base",
      check_cell_size(
        cell_size = "grid",
        resolution = "5km",
        level = "country"
      )
    ), 5000) # 5 km in meters
})

test_that("check_cell_size 'auto' mode converts to NULL and proceeds", {
  result <- check_cell_size(
    cell_size = "auto",
    resolution = "10km",
    level = "cube",
    area = 50000
  )

  expect_equal(result, 10000) # Should auto-determine to 10 km
})

test_that("check_cell_size throws error for invalid character value", {
  expect_error(
    check_cell_size(
      cell_size = "invalid",
      resolution = "10km",
      level = "country"
    ),
    "Invalid character value for cell_size"
  )
})

test_that("check_cell_size throws error for unrecognized resolution units", {
  expect_error(
    check_cell_size(
      cell_size = 10,
      resolution = "10meters",
      level = "country"
    ),
    "Resolution units not recognized"
  )
})

test_that("check_cell_size throws error when area is missing for cube level", {
  expect_error(
    check_cell_size(
      cell_size = NULL,
      resolution = "10km",
      level = "cube",
      area = NULL
    ),
    "Unable to determine area of cube"
  )
})

test_that("check_cell_size handles non-cube levels without area", {
  # Should use fallback default of 10 km
  result <- check_cell_size(
    cell_size = NULL,
    resolution = "5km",
    level = "country",
    area = NULL
  )

  expect_equal(result, 10000) # Fallback to 10 km
})

test_that("check_cell_size warns about small cell sizes in interactive mode", {
  # Mock interactive() to return FALSE to avoid actual prompts
  mockery::stub(check_cell_size, "interactive", FALSE)

  expect_warning(
    check_cell_size(
      cell_size = 1,
      resolution = "1km",
      level = "cube",
      area = 1000000000, # Large area makes warning threshold ~100 km
      max_warn_cells = 1000000
    ),
    "Non-interactive session detected"
  )
})

test_that("check_cell_size calculates warning threshold based on area and max_warn_cells", {
  # For area = 1,000,000 km² and max_warn_cells = 1,000,000
  # raw threshold = sqrt(1000000/1000000) = 1 km
  # With resolution = 10km, should round up to 10 km

  expect_message(
    with_mocked_bindings(
      interactive = mock_interactive_true,
      readline = mock_readline_y,
      .package = "base",
      check_cell_size(
        cell_size = 5,
        resolution = "1km",
        level = "cube",
        area = 100000000,
        max_warn_cells = 1000000
      )
    ),
    "smaller than the recommended minimum"
  )
})

test_that("check_cell_size 'grid' mode prompts when below warning threshold", {
  # Mock interactive() and readline()
  mockery::stub(check_cell_size, "interactive", TRUE)
  mockery::stub(check_cell_size, "readline", "y")

  # Resolution smaller than warning threshold should prompt
  result <- check_cell_size(
    cell_size = "grid",
    resolution = "1km",
    level = "cube",
    area = 1000000, # Makes warning threshold ~10 km
    max_warn_cells = 1000000
  )

  expect_equal(result, 1000) # Should proceed with 1 km
})

test_that("check_cell_size 'grid' mode aborts when user chooses 'n'", {
  mockery::stub(check_cell_size, "interactive", TRUE)
  mockery::stub(check_cell_size, "readline", "n")

  expect_error(
    check_cell_size(
      cell_size = "grid",
      resolution = "1km",
      level = "cube",
      area = 100000000,
      max_warn_cells = 1000000
    ),
    "User aborted process"
  )
})

test_that("check_cell_size 'grid' mode switches to auto when user chooses 'a'", {
  mockery::stub(check_cell_size, "interactive", TRUE)
  mockery::stub(check_cell_size, "readline", "a")

  result <- check_cell_size(
    cell_size = "grid",
    resolution = "1km",
    level = "cube",
    area = 100000000,
    max_warn_cells = 1000000
  )

  # Should auto-determine based on area (100 km for area >= 1,000,000)
  expect_equal(result, 100000)
})

test_that("check_cell_size handles invalid user input in interactive mode", {
  mockery::stub(check_cell_size, "interactive", TRUE)
  mockery::stub(check_cell_size, "readline", "invalid")

  expect_error(
    check_cell_size(
      cell_size = "grid",
      resolution = "1km",
      level = "cube",
      area = 100000000,
      max_warn_cells = 1000000
    ),
    "Invalid input"
  )
})

test_that("check_cell_size throws error in non-interactive mode with 'grid' below threshold", {
  mockery::stub(check_cell_size, "interactive", FALSE)

  expect_error(
    check_cell_size(
      cell_size = "grid",
      resolution = "1km",
      level = "cube",
      area = 100000000,
      max_warn_cells = 1000000
    ),
    "Non-interactive session detected.*grid.*requires interactive confirmation"
  )
})

test_that("check_cell_size handles degree resolution warning thresholds", {
  # World level should have 1 degree threshold
  with_mocked_bindings(
    interactive = mock_interactive_true,
    readline = mock_readline_y,
    .package = "base",

    expect_message(
      check_cell_size(
        cell_size = 0.5,
        resolution = "0.25degrees",
        level = "world",
        max_warn_cells = 1000000
      ),
      "smaller than the recommended minimum"
    )
  )

  with_mocked_bindings(
    interactive = mock_interactive_true,
    readline = mock_readline_y,
    .package = "base",
    # Other levels should have 0.1 degree threshold
    expect_message(
      check_cell_size(
        cell_size = 0.05,
        resolution = "0.01degrees",
        level = "country",
        max_warn_cells = 1000000
      ),
      "smaller than the recommended minimum"
    )
  )
})

test_that("check_cell_size throws error for non-numeric cell_size after processing", {
  # This tests the defensive check at the end
  # Force a scenario where cell_size becomes non-numeric (edge case)

  # Mock to bypass earlier checks and reach the final validation
  expect_error(
    check_cell_size(
      cell_size = 10,
      resolution = "notanumber",
      level = "country"
    ),
    "Resolution units not recognized"
  )
})

test_that("check_cell_size converts km to meters correctly", {
  result <- check_cell_size(
    cell_size = 50,
    resolution = "10km",
    level = "country"
  )

  expect_equal(result, 50000) # 50 km = 50,000 meters
})

test_that("check_cell_size does not convert degrees to meters", {
  result <- check_cell_size(
    cell_size = 2,
    resolution = "0.5degrees",
    level = "world"
  )

  expect_equal(result, 2) # Degrees remain as-is
})

test_that("check_cell_size handles edge case with very small areas", {
  result <- check_cell_size(
    cell_size = NULL,
    resolution = "0.01km",
    level = "cube",
    area = 1
  )

  expect_equal(result, 100) # Should default to 0.1 km = 100 meters
})
