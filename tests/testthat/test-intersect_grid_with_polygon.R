test_that("intersect_grid_with_polygon returns correct intersection", {
  # Create a simple grid
  grid <- sf::st_sf(
    cellid = 1:4,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
      sf::st_polygon(list(rbind(c(0, 1), c(1, 1), c(1, 2), c(0, 2), c(0, 1)))),
      sf::st_polygon(list(rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))))
    ),
    crs = 4326
  )

  # Create intersection target that overlaps with 2 cells
  target <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0.5, 0.5), c(1.5, 0.5), c(1.5, 1.5), c(0.5, 1.5), c(0.5, 0.5))))
    ),
    crs = 4326
  )

  suppressWarnings(result <- intersect_grid_with_polygon(grid, target))

  expect_s3_class(result, "sf")
  expect_true("cellid" %in% names(result))
  expect_true("geometry" %in% names(result))
  expect_gt(nrow(result), 0)
})

test_that("intersect_grid_with_polygon preserves area column if present", {
  grid <- sf::st_sf(
    cellid = 1:2,
    area = c(100, 200),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
    ),
    crs = 4326
  )

  target <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0.5, 0), c(1.5, 0), c(1.5, 1), c(0.5, 1), c(0.5, 0))))
    ),
    crs = 4326
  )

  suppressWarnings(result <- intersect_grid_with_polygon(grid, target))

  expect_true("area" %in% names(result))
})

test_that("intersect_grid_with_polygon throws error when no intersection", {
  grid <- sf::st_sf(
    cellid = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
    ),
    crs = 4326
  )

  # Target far away from grid
  target <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(10, 10), c(11, 10), c(11, 11), c(10, 11), c(10, 10))))
    ),
    crs = 4326
  )

  suppressWarnings(expect_error(
    intersect_grid_with_polygon(grid, target),
    "No spatial intersection between map data and grid"
  ))
})

test_that("intersect_grid_with_polygon handles geometry errors with s2 fallback", {
  # Mock st_intersection to fail first, succeed second
  original_intersection <- sf::st_intersection
  call_count <- 0

  mockery::stub(
    intersect_grid_with_polygon,
    'sf::st_intersection',
    function(...) {
      call_count <<- call_count + 1
      if (call_count == 1) {
        stop("Error in wk_handle.wk_wkb: Invalid geometry")
      } else {
        original_intersection(...)
      }
    }
  )

  grid <- sf::st_sf(
    cellid = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
    ),
    crs = 4326
  )

  target <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0.5, 0.5), c(1.5, 0.5), c(1.5, 1.5), c(0.5, 1.5), c(0.5, 0.5))))
    ),
    crs = 4326
  )

  suppressWarnings(expect_message(
    result <- intersect_grid_with_polygon(grid, target),
    "Encountered a geometry error"
  ))


  expect_s3_class(result, "sf")
})

test_that("intersect_grid_with_polygon re-throws non-geometry errors", {
  mockery::stub(
    intersect_grid_with_polygon,
    'sf::st_intersection',
    function(...) {
      stop("Some other unrelated error")
    }
  )

  grid <- sf::st_sf(
    cellid = 1,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
    ),
    crs = 4326
  )

  target <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0.5, 0.5), c(1.5, 0.5), c(1.5, 1.5), c(0.5, 1.5), c(0.5, 0.5))))
    ),
    crs = 4326
  )

  expect_error(
    intersect_grid_with_polygon(grid, target),
    "Some other unrelated error"
  )
})

test_that("intersect_grid_with_polygon only selects required columns", {
  grid <- sf::st_sf(
    cellid = 1,
    extra_col = "extra",
    area = 100,
    another_col = 42,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
    ),
    crs = 4326
  )

  target <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0.5, 0.5), c(1.5, 0.5), c(1.5, 1.5), c(0.5, 1.5), c(0.5, 0.5))))
    ),
    crs = 4326
  )

  suppressWarnings(result <- intersect_grid_with_polygon(grid, target))

  expect_true(all(c("cellid", "geometry") %in% names(result)))
  expect_true("area" %in% names(result))
  expect_false("extra_col" %in% names(result))
  expect_false("another_col" %in% names(result))
})
