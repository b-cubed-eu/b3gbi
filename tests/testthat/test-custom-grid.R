# Tests for cubes with user-supplied grid geometries (process_cube(grid = ...))

# A 4 x 4 grid of 1 km EEA-aligned cells, identified by an arbitrary integer ID
make_test_grid <- function() {
  x0 <- 4004000
  y0 <- 3092000
  cells <- expand.grid(i = 0:3, j = 0:3)
  polys <- lapply(seq_len(nrow(cells)), function(k) {
    x <- x0 + cells$i[k] * 1000
    y <- y0 + cells$j[k] * 1000
    sf::st_polygon(list(rbind(c(x, y), c(x + 1000, y), c(x + 1000, y + 1000),
                              c(x, y + 1000), c(x, y))))
  })
  sf::st_sf(
    grid_idx = 100L + seq_len(nrow(cells)),
    eea = sprintf("1kmE%dN%d", (x0 / 1000) + cells$i, (y0 / 1000) + cells$j),
    geometry = sf::st_sfc(polys, crs = 3035)
  )
}

# A small cube in the style of the BMD cubing engine: grid_idx, a datetime
# column instead of year, lower-case column names
make_test_cube <- function(grid) {
  set.seed(42)
  n <- 300
  idx <- sample(grid$grid_idx, n, replace = TRUE)
  sp <- sample(1:12, n, replace = TRUE)
  data.frame(
    grid_idx = idx,
    datetime = as.POSIXct(paste0(sample(2001:2010, n, replace = TRUE),
                                 "-01-01"), tz = "UTC"),
    kingdom = "Plantae",
    family = paste0("Family", (sp %% 3) + 1),
    species = paste0("Species ", sp),
    specieskey = as.character(1000 + sp),
    occurrences = sample(1:5, n, replace = TRUE),
    mincoordinateuncertaintyinmeters = 10
  )
}

test_that("process_cube joins a custom sf grid to the cube", {
  grid <- make_test_grid()
  cube_df <- make_test_cube(grid)

  cube <- process_cube(cube_df, grid = grid)
  expect_s3_class(cube, "processed_cube")
  expect_equal(cube$grid_type, "custom")
  expect_s3_class(cube$grid, "sf")
  expect_equal(cube$resolutions, "1km")
  expect_true(all(c("cellCode", "xcoord", "ycoord", "year") %in%
                    names(cube$data)))
  expect_setequal(unique(cube$data$year), 2001:2010)
  expect_true(all(cube$data$cellCode %in% cube$grid$cellCode))
  expect_true(all(cube$grid$cellCode %in% cube$data$cellCode))
})

test_that("indicators from a custom grid equal those from the EEA grid", {
  grid <- make_test_grid()
  cube_df <- make_test_cube(grid)
  custom <- process_cube(cube_df, grid = grid[, c("grid_idx")])

  eea_df <- cube_df
  eea_df$eeaCellCode <- grid$eea[match(eea_df$grid_idx, grid$grid_idx)]
  eea_df$year <- as.integer(format(eea_df$datetime, "%Y"))
  eea_df$datetime <- NULL
  eea_df$grid_idx <- NULL
  eea <- process_cube(eea_df, grid_type = "eea", cols_cellCode = "eeaCellCode")

  map_c <- suppressWarnings(obs_richness_map(custom))
  map_e <- suppressWarnings(obs_richness_map(eea))
  expect_equal(nrow(map_c$data), nrow(map_e$data))
  expect_equal(sort(map_c$data$diversity_val), sort(map_e$data$diversity_val))
  expect_equal(sum(as.numeric(map_c$data$area)), 16)

  ts_c <- suppressWarnings(total_occ_ts(custom))
  ts_e <- suppressWarnings(total_occ_ts(eea))
  expect_equal(ts_c$data$diversity_val, ts_e$data$diversity_val)

  # Aggregation to coarser cells works as for EEA cubes
  agg_c <- suppressWarnings(total_occ_map(custom, cell_size = 2))
  agg_e <- suppressWarnings(total_occ_map(eea, cell_size = 2))
  expect_equal(sum(agg_c$data$diversity_val, na.rm = TRUE),
               sum(agg_e$data$diversity_val, na.rm = TRUE))
})

test_that("custom grids are accepted as files and WKT data frames", {
  grid <- make_test_grid()
  cube_df <- make_test_cube(grid)

  gpkg <- tempfile(fileext = ".gpkg")
  sf::st_write(grid[, "grid_idx"], gpkg, quiet = TRUE)
  cube_gpkg <- process_cube(cube_df, grid = gpkg)
  expect_equal(nrow(cube_gpkg$grid), 16)

  wkt <- data.frame(grid_idx = grid$grid_idx,
                    wkt = sf::st_as_text(sf::st_geometry(grid)))
  expect_error(process_cube(cube_df, grid = wkt),
               "no coordinate reference system")
  cube_wkt <- process_cube(cube_df, grid = wkt, grid_crs = "EPSG:3035")
  expect_equal(sf::st_crs(cube_wkt$grid), sf::st_crs(3035))

  csv <- tempfile(fileext = ".csv")
  utils::write.csv(wkt, csv, row.names = FALSE)
  cube_csv <- process_cube(cube_df, grid = csv, grid_crs = "EPSG:3035")
  expect_equal(nrow(cube_csv$grid), 16)
})

test_that("cube and grid can be read from (Geo)Parquet files", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("jsonlite")
  grid <- make_test_grid()
  cube_df <- make_test_cube(grid)

  cube_file <- tempfile(fileext = ".parquet")
  arrow::write_parquet(cube_df, cube_file)

  # Write the grid as GeoParquet: WKB geometry plus "geo" metadata
  grid_file <- tempfile(fileext = ".parquet")
  tbl <- arrow::arrow_table(
    grid_idx = grid$grid_idx,
    geometry = arrow::Array$create(
      lapply(sf::st_as_binary(sf::st_geometry(grid)), as.raw),
      type = arrow::binary()
    )
  )
  crs_json <- sf::st_crs(3035)$ProjJson
  skip_if(is.null(crs_json) || is.na(crs_json), "PROJJSON not available")
  tbl$metadata$geo <- paste0(
    '{"version":"1.0.0","primary_column":"geometry","columns":{"geometry":',
    '{"encoding":"WKB","geometry_types":["Polygon"],"crs":', crs_json, '}}}'
  )
  arrow::write_parquet(tbl, grid_file)

  cube <- process_cube(cube_file, grid = grid_file)
  expect_equal(cube$grid_type, "custom")
  expect_equal(sf::st_crs(cube$grid), sf::st_crs(3035))
  expect_setequal(unique(cube$data$year), 2001:2010)

  ref <- process_cube(cube_df, grid = grid)
  expect_equal(nrow(cube$data), nrow(ref$data))
})

test_that("the linking column can be given or detected", {
  grid <- make_test_grid()
  cube_df <- make_test_cube(grid)

  # Two shared columns -> ambiguous
  cube_df2 <- cube_df
  cube_df2$eea <- "x"
  expect_error(process_cube(cube_df2, grid = grid),
               "Could not determine which column")
  cube <- process_cube(cube_df2, grid = grid, grid_id = "grid_idx")
  expect_equal(nrow(cube$grid), 16)

  # Differently named columns
  cube_df3 <- cube_df
  names(cube_df3)[names(cube_df3) == "grid_idx"] <- "cell"
  cube <- process_cube(cube_df3, grid = grid[, "grid_idx"],
                       grid_id = "grid_idx", cols_cellCode = "cell")
  expect_equal(nrow(cube$grid), 16)

  expect_error(process_cube(cube_df, grid = grid, grid_id = "nope"),
               "not found")
})

test_that("cube cells missing from the grid are dropped with a warning", {
  grid <- make_test_grid()
  cube_df <- make_test_cube(grid)
  expect_warning(cube <- process_cube(cube_df, grid = grid[1:8, ]),
                 "no matching cell")
  expect_true(all(cube$data$cellCode %in% as.character(grid$grid_idx[1:8])))

  # No matching cells at all
  cube_df$grid_idx <- cube_df$grid_idx + 1000L
  expect_error(suppressWarnings(process_cube(cube_df, grid = grid)),
               "None of the cube's grid cells")
})

test_that("invalid custom grids are rejected", {
  grid <- make_test_grid()
  cube_df <- make_test_cube(grid)

  dup <- rbind(grid, grid[1, ])
  expect_error(process_cube(cube_df, grid = dup), "duplicate")

  pts <- suppressWarnings(sf::st_centroid(grid))
  expect_error(suppressWarnings(process_cube(cube_df, grid = pts)),
               "must consist of polygons")

  expect_error(process_cube(cube_df, grid = grid, grid_type = "eea"),
               "grid_type = 'custom'")

  expect_error(process_cube(cube_df, grid = tempfile(fileext = ".gpkg")),
               "not found")
})

test_that("longitude/latitude custom grids use an equal-area projection", {
  grid <- sf::st_transform(make_test_grid(), 4326)
  cube_df <- make_test_cube(grid)
  cube <- process_cube(cube_df, grid = grid)
  expect_true(sf::st_is_longlat(cube$grid))

  map <- suppressWarnings(obs_richness_map(cube))
  expect_false(sf::st_is_longlat(map$data))
  expect_equal(sum(as.numeric(map$data$area)), 16, tolerance = 0.01)
})
