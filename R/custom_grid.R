# Support for cubes with user-supplied (custom) grid geometries ---------------
#
# Some cube-producing tools (e.g. the BMD cubing engine) identify grid cells by
# an arbitrary ID and deliver the cell geometries in a separate "dimension
# table". These helpers read such a table, join it to the cube and build the
# grid used by the indicator workflow.

#' Check that arrow (and jsonlite) are installed for reading parquet files
#' @noRd
check_parquet_support <- function(geo = FALSE) {
  rlang::check_installed(
    c("arrow", if (geo) "jsonlite"),
    reason = "to read parquet files."
  )
  invisible(TRUE)
}

#' Is a path a parquet file?
#' @noRd
is_parquet_path <- function(x) {
  is.character(x) && length(x) == 1 && grepl("\\.(geo)?parquet$", tolower(x))
}

#' Read a (non-spatial) parquet cube as a tibble of character columns
#'
#' All columns are converted to character, as when a CSV cube is read, so that
#' the rest of `process_cube()` treats both in the same way. Date-time columns
#' are written as ISO dates (YYYY-MM-DD).
#' @noRd
read_parquet_cube <- function(path) {
  check_parquet_support()
  x <- arrow::read_parquet(path)
  x <- as.data.frame(x)
  for (nm in names(x)) {
    col <- x[[nm]]
    if (inherits(col, "POSIXt")) {
      # Parquet timestamps without a time zone are read as UTC instants;
      # formatting them in the local time zone could shift dates (and years)
      tz <- attr(col, "tzone")
      if (is.null(tz) || identical(tz, "")) tz <- "UTC"
      x[[nm]] <- format(col, "%Y-%m-%d", tz = tz)
    } else if (inherits(col, "Date")) {
      x[[nm]] <- format(col, "%Y-%m-%d")
    } else if (is.list(col)) {
      # Binary or nested columns are not used by b3gbi
      x[[nm]] <- NULL
    } else {
      x[[nm]] <- as.character(col)
    }
  }
  tibble::as_tibble(x)
}

#' Read the CRS stored in GeoParquet metadata
#' @noRd
geoparquet_crs <- function(table, geom_col) {
  geo <- table$metadata$geo
  if (is.null(geo)) return(NULL)
  meta <- jsonlite::fromJSON(geo, simplifyVector = FALSE)
  col_meta <- meta$columns[[geom_col]]
  if (is.null(col_meta)) return(NULL)
  # GeoParquet: a missing crs means OGC:CRS84 (longitude/latitude)
  if (!"crs" %in% names(col_meta)) return(sf::st_crs("OGC:CRS84"))
  if (is.null(col_meta$crs)) return(NULL)
  crs <- sf::st_crs(as.character(jsonlite::toJSON(col_meta$crs,
                                                  auto_unbox = TRUE,
                                                  digits = NA)))
  # Prefer a compact "EPSG:xxxx" definition when the CRS has an EPSG code
  if (!is.na(crs$epsg)) crs <- sf::st_crs(paste0("EPSG:", crs$epsg))
  crs
}

#' Read a parquet file with a WKB geometry column as an sf object
#' @noRd
read_geoparquet <- function(path) {
  check_parquet_support(geo = TRUE)
  tb <- arrow::read_parquet(path, as_data_frame = FALSE)
  geo <- tb$metadata$geo
  geom_col <- if (!is.null(geo)) {
    jsonlite::fromJSON(geo, simplifyVector = FALSE)$primary_column
  } else {
    NULL
  }
  df <- as.data.frame(tb)
  if (is.null(geom_col)) {
    cand <- names(df)[vapply(df, is.list, logical(1))]
    if (length(cand) == 0) {
      stop("No geometry column found in '", basename(path), "'.",
           call. = FALSE)
    }
    geom_col <- cand[1]
  }
  crs <- geoparquet_crs(tb, geom_col)
  wkb <- structure(lapply(df[[geom_col]], as.raw), class = "WKB")
  geometry <- sf::st_as_sfc(wkb, crs = if (is.null(crs)) NA else crs)
  df[[geom_col]] <- NULL
  sf::st_sf(df, geometry = geometry)
}

#' Read a custom grid from a file or object
#'
#' @param grid An sf object, a data frame with a WKT geometry column, or a path
#'   to a GeoParquet, GeoPackage, shapefile, GeoJSON or CSV (with a WKT column)
#'   file.
#' @param grid_crs CRS for grids without CRS information (CSV/WKT).
#' @return An sf object.
#' @noRd
read_custom_grid <- function(grid, grid_crs = NULL) {
  if (inherits(grid, "sf")) {
    out <- grid
  } else if (inherits(grid, "sfc")) {
    stop("`grid` must contain an ID column as well as the geometries; ",
         "please provide an sf object (data frame with a geometry column).",
         call. = FALSE)
  } else if (is.data.frame(grid)) {
    out <- wkt_df_to_sf(grid, grid_crs)
  } else if (is.character(grid) && length(grid) == 1) {
    if (!file.exists(grid)) {
      stop("Grid file not found: ", grid, call. = FALSE)
    }
    if (is_parquet_path(grid)) {
      out <- read_geoparquet(grid)
    } else if (grepl("\\.(csv|tsv|txt)$", tolower(grid))) {
      delim <- switch(sub(".*\\.", "", tolower(grid)),
                      csv = ",", tsv = "\t", NULL)
      out <- wkt_df_to_sf(
        readr::read_delim(grid, delim = delim,
                          col_types = readr::cols(.default = "c"),
                          show_col_types = FALSE, progress = FALSE),
        grid_crs
      )
    } else {
      out <- sf::read_sf(grid)
    }
  } else {
    stop("`grid` must be a file path, an sf object or a data frame with a ",
         "WKT geometry column.", call. = FALSE)
  }

  if (!is.null(grid_crs) && is.na(sf::st_crs(out))) {
    sf::st_crs(out) <- sf::st_crs(grid_crs)
  }
  if (is.na(sf::st_crs(out))) {
    stop("The grid has no coordinate reference system. Please specify it ",
         "with `grid_crs` (e.g. grid_crs = \"EPSG:3035\").", call. = FALSE)
  }
  types <- unique(as.character(sf::st_geometry_type(out)))
  if (!all(types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("The grid must consist of polygons (found: ",
         paste(types, collapse = ", "), ").", call. = FALSE)
  }
  out
}

#' Convert a data frame with a WKT column to sf
#' @noRd
wkt_df_to_sf <- function(df, grid_crs = NULL) {
  df <- as.data.frame(df)
  wkt_col <- names(df)[tolower(names(df)) %in% c("geometry", "geom", "wkt",
                                                 "the_geom")]
  if (length(wkt_col) == 0) {
    stop("No geometry column found in the grid. Expected a column with WKT ",
         "geometries named 'geometry', 'geom' or 'wkt'.", call. = FALSE)
  }
  wkt_col <- wkt_col[1]
  geometry <- sf::st_as_sfc(as.character(df[[wkt_col]]),
                            crs = if (is.null(grid_crs)) NA else grid_crs)
  df[[wkt_col]] <- NULL
  sf::st_sf(df, geometry = geometry)
}

#' Find the column that links cube rows to grid cells
#'
#' @return A list with the column names in the cube and in the grid.
#' @noRd
find_grid_id_columns <- function(cube_df, grid, cols_cellCode = NULL,
                                 grid_id = NULL) {
  grid_cols <- setdiff(names(grid), attr(grid, "sf_column"))

  cube_col <- cols_cellCode
  grid_col <- grid_id

  if (is.null(cube_col) && is.null(grid_col)) {
    shared <- intersect(names(cube_df), grid_cols)
    if (length(shared) == 1) {
      cube_col <- grid_col <- shared
    } else if ("cellCode" %in% shared) {
      cube_col <- grid_col <- "cellCode"
    } else {
      stop("Could not determine which column links the cube to the grid. ",
           "Please give it with `grid_id` (the column name in the grid) and, ",
           "if it is named differently in the cube, `cols_cellCode`.",
           call. = FALSE)
    }
  } else if (is.null(cube_col)) {
    cube_col <- grid_col
  } else if (is.null(grid_col)) {
    grid_col <- cube_col
  }

  if (!cube_col %in% names(cube_df)) {
    stop("Column '", cube_col, "' not found in the cube.", call. = FALSE)
  }
  if (!grid_col %in% grid_cols) {
    stop("Column '", grid_col, "' not found in the grid.", call. = FALSE)
  }
  list(cube = cube_col, grid = grid_col)
}

#' Prepare a custom grid: keep the ID and geometry, validate, add centroids
#'
#' @return An sf object with columns `cellCode`, `xcoord`, `ycoord` (centroids
#'   in the grid's CRS) and the geometry.
#' @noRd
prepare_custom_grid <- function(grid, grid_col) {
  cellCode <- NULL
  out <- sf::st_sf(
    cellCode = as.character(grid[[grid_col]]),
    geometry = sf::st_geometry(grid)
  )
  out <- out[!is.na(out$cellCode), ]
  if (anyDuplicated(out$cellCode)) {
    stop("The grid ID column '", grid_col, "' contains duplicate values.",
         call. = FALSE)
  }
  if (!all(sf::st_is_valid(out))) {
    out <- sf::st_make_valid(out)
  }
  centroids <- suppressWarnings(sf::st_coordinates(
    sf::st_point_on_surface(sf::st_geometry(out))
  ))
  out$xcoord <- centroids[, 1]
  out$ycoord <- centroids[, 2]
  out
}

#' Typical cell size of a custom grid, as a resolution string in km
#' @noRd
custom_grid_resolution <- function(grid) {
  area_km2 <- as.numeric(units::set_units(sf::st_area(grid), "km^2"))
  side <- sqrt(stats::median(area_km2, na.rm = TRUE))
  paste0(signif(side, 3), "km")
}

#' Build the grid used by the indicator workflow from a custom grid
#'
#' Mirrors the output of `create_native_grid()`: an sf object with `cellid`,
#' `cellCode` and `area` (km^2), in the projected CRS.
#' @noRd
create_custom_grid <- function(grid, df, projection) {
  cellCode <- NULL
  grid <- grid[grid$cellCode %in% unique(df$cellCode), c("cellCode")]
  grid <- sf::st_transform(grid, projection)
  grid$cellid <- seq_len(nrow(grid))
  grid$area <- units::set_units(sf::st_area(grid), "km^2")
  grid %>%
    dplyr::select("cellid", "cellCode", "area", tidyselect::everything())
}

#' CRS used for calculations with a custom grid
#'
#' Projected grids are used in their own CRS; for longitude/latitude grids the
#' Eckert IV equal-area projection (ESRI:54012) is used.
#' @noRd
custom_grid_projected_crs <- function(grid) {
  crs <- sf::st_crs(grid)
  if (isTRUE(sf::st_is_longlat(crs))) sf::st_crs("ESRI:54012") else crs
}

#' Does a cube carry its own (custom) grid geometries?
#' @noRd
has_custom_grid <- function(data) {
  identical(data$grid_type, "custom") && inherits(data$grid, "sf")
}
