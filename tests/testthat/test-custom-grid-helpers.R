# Unit tests for the internal helpers behind process_cube(grid = ...)

square <- function(x, y, size = 1000) {
  sf::st_polygon(list(rbind(c(x, y), c(x + size, y), c(x + size, y + size),
                            c(x, y + size), c(x, y))))
}

test_that("read_parquet_cube converts dates and drops nested columns", {
  skip_if_not_installed("arrow")
  f <- tempfile(fileext = ".parquet")
  tbl <- arrow::arrow_table(
    id = 1:2,
    day = as.Date(c("2001-03-04", "2002-05-06")),
    blob = arrow::Array$create(list(as.raw(1), as.raw(2)),
                               type = arrow::binary())
  )
  arrow::write_parquet(tbl, f)
  x <- read_parquet_cube(f)
  expect_named(x, c("id", "day"))
  expect_equal(x$day, c("2001-03-04", "2002-05-06"))
  expect_type(x$id, "character")
})

test_that("geoparquet_crs follows the GeoParquet metadata rules", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  tbl <- arrow::arrow_table(x = 1)
  expect_null(geoparquet_crs(tbl, "geometry"))

  tbl$metadata$geo <- '{"primary_column":"geometry","columns":{}}'
  expect_null(geoparquet_crs(tbl, "geometry"))

  # No crs member means OGC:CRS84
  tbl$metadata$geo <- paste0('{"primary_column":"geometry","columns":',
                             '{"geometry":{"encoding":"WKB"}}}')
  expect_equal(geoparquet_crs(tbl, "geometry"), sf::st_crs("OGC:CRS84"))

  # An explicit null crs means unknown
  tbl$metadata$geo <- paste0('{"primary_column":"geometry","columns":',
                             '{"geometry":{"encoding":"WKB","crs":null}}}')
  expect_null(geoparquet_crs(tbl, "geometry"))
})

test_that("read_geoparquet finds a WKB column without metadata", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  geom <- sf::st_sfc(square(0, 0), square(1000, 0))
  f <- tempfile(fileext = ".parquet")
  arrow::write_parquet(arrow::arrow_table(
    cell = c("a", "b"),
    geom = arrow::Array$create(lapply(sf::st_as_binary(geom), as.raw),
                               type = arrow::binary())
  ), f)
  x <- read_geoparquet(f)
  expect_s3_class(x, "sf")
  expect_equal(x$cell, c("a", "b"))
  expect_true(is.na(sf::st_crs(x)))

  # The CRS can then be given with grid_crs
  g <- read_custom_grid(f, grid_crs = "EPSG:3035")
  expect_equal(sf::st_crs(g), sf::st_crs(3035))

  f2 <- tempfile(fileext = ".parquet")
  arrow::write_parquet(arrow::arrow_table(cell = c("a", "b")), f2)
  expect_error(read_geoparquet(f2), "No geometry column")
})

test_that("read_custom_grid rejects unsupported inputs", {
  geom <- sf::st_sfc(square(0, 0), crs = 3035)
  expect_error(read_custom_grid(geom), "must contain an ID column")
  expect_error(read_custom_grid(42), "must be a file path")
  expect_error(wkt_df_to_sf(data.frame(id = 1, shape = "POINT (0 0)")),
               "No geometry column")

  # An sf object without CRS gets grid_crs
  g <- sf::st_sf(id = 1, geometry = sf::st_sfc(square(0, 0)))
  expect_equal(sf::st_crs(read_custom_grid(g, grid_crs = 3035)),
               sf::st_crs(3035))
})

test_that("find_grid_id_columns resolves the linking columns", {
  grid <- sf::st_sf(cellCode = "a", other = 1,
                    geometry = sf::st_sfc(square(0, 0)))
  cube <- data.frame(cellCode = "a", other = 1)

  # Several shared columns: cellCode is preferred
  expect_equal(find_grid_id_columns(cube, grid),
               list(cube = "cellCode", grid = "cellCode"))
  # Only the cube column given: same name in the grid
  expect_equal(find_grid_id_columns(cube, grid, cols_cellCode = "other"),
               list(cube = "other", grid = "other"))
  # Column present in the cube but not in the grid
  cube$cell <- "a"
  expect_error(find_grid_id_columns(cube, grid, cols_cellCode = "cell"),
               "not found in the grid")
})

test_that("prepare_custom_grid repairs invalid polygons", {
  bowtie <- sf::st_polygon(list(rbind(c(0, 0), c(1000, 1000), c(1000, 0),
                                      c(0, 1000), c(0, 0))))
  grid <- sf::st_sf(id = c("a", "b"),
                    geometry = sf::st_sfc(bowtie, square(2000, 0), crs = 3035))
  expect_false(all(sf::st_is_valid(grid)))
  out <- prepare_custom_grid(grid, "id")
  expect_true(all(sf::st_is_valid(out)))
  expect_equal(out$cellCode, c("a", "b"))
  expect_true(all(is.finite(out$xcoord)))
})

test_that("ISEA3H helpers handle edge cases", {
  expect_equal(isea3h_side_from_spacing(10, 50), 1000)
  expect_gt(isea3h_side_from_spacing(c(10, 11), c(50, 50)), 10000)

  # A point outside the visible hemisphere of an orthographic projection
  expect_null(project_ring_lonlat("+proj=ortho +lat_0=0 +lon_0=0",
                                  cbind(1e8, 1e8)))
  expect_equal(dim(project_ring_lonlat("EPSG:3857", cbind(0, 0))), c(1, 2))

  # Cells with unusable coordinates are dropped, the others kept
  df <- data.frame(cellCode = c("a", "b"), xcoord = c(10, NA),
                   ycoord = c(50, NA))
  grid <- suppressMessages(build_native_isea3h_cells(df, "EPSG:4326"))
  expect_equal(grid$cellCode, "a")
})

test_that("the ISEA branch falls back gracefully on any PROJ version", {
  # Pretend PROJ can invert +proj=isea; with older PROJ the inverse fails and
  # every cell must fall back to the local projection
  local_mocked_bindings(proj_supports_isea_roundtrip = function(...) TRUE)
  df <- data.frame(cellCode = c("452583359004665569", "4859758557001684287"),
                   xcoord = c(4.67, -1.68), ycoord = c(52.58, 59.76))
  grid <- suppressMessages(build_native_isea3h_cells(df, "ESRI:54012"))
  expect_equal(nrow(grid), 2)
  expect_true(all(sf::st_is_valid(grid)))
})
