# Dispatch to the correct function to build a native grid for MGRS, EEA, or EQDGC cubes
#' @noRd
create_native_grid <- function(df, projection, grid_type, resolution = NULL) {

  cellCode <- NULL

  res <- if (grid_type == "mgrs") {
    create_mgrs_grid(df, projection, resolution)
  } else if (grid_type == "eea") {
    create_eea_grid(df, projection, resolution)
  } else if (grid_type == "eqdgc") {
    create_eqdgc_grid(df, projection, resolution)
  }

  if (!is.null(res) && "cellCode" %in% colnames(res)) {
    res <- res %>% dplyr::distinct(cellCode, .keep_all = TRUE)
  }

  return(res)
}

#' Build a square polygon centered on a point (for MGRS/EQDGC where coords are centers)
#' @noRd
make_square_poly <- function(x_center, y_center, half_res) {
  sf::st_polygon(list(matrix(c(
    x_center - half_res, y_center - half_res,
    x_center + half_res, y_center - half_res,
    x_center + half_res, y_center + half_res,
    x_center - half_res, y_center + half_res,
    x_center - half_res, y_center - half_res
  ), ncol = 2, byrow = TRUE)))
}

#' Build a square polygon from lower-left corner (for EEA where coords are lower-left)
#' @noRd
make_square_poly_ll <- function(x_ll, y_ll, res) {
  sf::st_polygon(list(matrix(c(
    x_ll, y_ll,
    x_ll + res, y_ll,
    x_ll + res, y_ll + res,
    x_ll, y_ll + res,
    x_ll, y_ll
  ), ncol = 2, byrow = TRUE)))
}

#' @noRd
create_mgrs_grid <- function(df, projection, resolution = NULL) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  cellCode <- x_center <- y_center <- cellid <- area <- utm_zone <- utm_hemi <- NULL

  df <- df[!is.na(df$cellCode) & df$cellCode != "", ]
  if (nrow(df) == 0) return(NULL)

  # 1. Determine resolution in meters
  res_str <- if (is.null(resolution) && "resolution" %in% names(df)) {
    df$resolution[1]
  } else {
    resolution
  }

  if (is.null(res_str) || is.na(res_str)) {
    res_m <- 1000 # Default to 1km
  } else {
    res_val <- as.numeric(gsub("[a-zA-Z]", "", res_str))
    res_unit <- gsub("[0-9.]", "", res_str)
    res_m <- if (res_unit == "km") res_val * 1000 else res_val
  }
  half_res <- res_m / 2

  # 2. Parse MGRS codes to UTM coordinates
  unique_codes <- unique(df$cellCode)
  utm_info <- mgrs::mgrs_to_utm(unique_codes)

  # Fallback to xcoord/ycoord if mgrs parsing failed (all or partial)
  na_utm <- is.na(utm_info$easting) | is.na(utm_info$northing)
  if (any(na_utm) && "xcoord" %in% names(df) && "ycoord" %in% names(df)) {
    idx <- match(unique_codes[na_utm], df$cellCode)
    utm_info$easting[na_utm] <- as.numeric(df$xcoord[idx])
    utm_info$northing[na_utm] <- as.numeric(df$ycoord[idx])

    if ("utmzone" %in% names(df)) {
      utm_info$zone[na_utm] <- df$utmzone[idx]
    }
    if ("hemisphere" %in% names(df)) {
      utm_info$hemisphere[na_utm] <- df$hemisphere[idx]
    }
  }

  # Report any codes that still couldn't be resolved
  still_na <- is.na(utm_info$easting) | is.na(utm_info$northing)
  if (any(still_na)) {
    warning(sprintf(
      "MGRS grid: %d of %d unique cell codes could not be resolved to coordinates and will be excluded.",
      sum(still_na), length(unique_codes)
    ))
  }

  # Build lookup
  lookup <- data.frame(
    cellCode = unique_codes,
    easting = utm_info$easting,
    northing = utm_info$northing,
    zone = utm_info$zone,
    hemisphere = utm_info$hemisphere,
    stringsAsFactors = FALSE
  )
  lookup <- lookup[!is.na(lookup$easting) & !is.na(lookup$northing), ]
  if (nrow(lookup) == 0) return(NULL)

  # 3. Group by UTM zone and hemisphere, build polygons per group
  lookup_list <- split(lookup, list(lookup$zone, lookup$hemisphere), drop = TRUE)

  all_grids <- lapply(lookup_list, function(grp) {
    if (nrow(grp) == 0) return(NULL)

    zone <- as.numeric(grp$zone[1])
    hemi <- grp$hemisphere[1]
    if (is.na(zone) || is.null(hemi)) return(NULL)

    epsg_code <- if (hemi %in% c("North", "N")) {
      paste0("EPSG:", 32600 + zone)
    } else {
      paste0("EPSG:", 32700 + zone)
    }

    # Build square polygons directly from center coordinates
    polys <- mapply(make_square_poly, grp$easting, grp$northing,
                    MoreArgs = list(half_res = half_res), SIMPLIFY = FALSE)
    sfc <- sf::st_sfc(polys, crs = epsg_code)
    sf_out <- sf::st_sf(cellCode = grp$cellCode, geometry = sfc)
    sf::st_transform(sf_out, projection)
  })

  all_grids <- all_grids[!vapply(all_grids, is.null, logical(1))]
  if (length(all_grids) == 0) return(NULL)

  grid <- do.call(rbind, all_grids)
  grid <- sf::st_make_valid(grid)
  grid <- grid[!sf::st_is_empty(grid), ]
  if (nrow(grid) == 0) return(NULL)

  grid$cellid <- seq_len(nrow(grid))
  grid$area <- grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  grid <- grid %>%
    dplyr::select(cellid, tidyselect::any_of("cellCode"), area, tidyselect::everything())

  return(grid)
}

#' Create a native EEA grid
#' @param df Data frame with `cellCode` and `resolution`.
#' @param projection The projected CRS the grid should be returned in.
#' @noRd
create_eea_grid <- function(df, projection, resolution = NULL) {

  cellCode <- NULL

  # 1. Determine resolution in meters
  res_str <- if (is.null(resolution) && "resolution" %in% names(df)) {
    df$resolution[1]
  } else {
    resolution
  }

  if (is.null(res_str) || is.na(res_str)) {
    coords_temp <- eea_code_to_coords(df$cellCode[1])
    res_m <- as.numeric(gsub("km", "", coords_temp$resolution[1])) * 1000
  } else {
    res_val <- as.numeric(gsub("[a-zA-Z]", "", res_str))
    res_unit <- gsub("[0-9.]", "", res_str)
    res_m <- if (res_unit == "km") res_val * 1000 else res_val
  }

  # 2. Parse EEA cell codes to coordinates (lower-left corners in EPSG:3035)
  unique_codes <- unique(df$cellCode)
  coords <- eea_code_to_coords(unique_codes)

  # If parsing failed (all or partial), fallback to xcoord/ycoord from the data
  na_coords <- is.na(coords$xcoord) | is.na(coords$ycoord)
  if (any(na_coords) && "xcoord" %in% names(df) && "ycoord" %in% names(df)) {
    idx <- match(coords$cellCode[na_coords], df$cellCode)
    coords$xcoord[na_coords] <- as.numeric(df$xcoord[idx])
    coords$ycoord[na_coords] <- as.numeric(df$ycoord[idx])
  }

  # Determine native resolution
  native_res_m <- as.numeric(gsub("km", "", coords$resolution[1])) * 1000
  if (is.na(native_res_m)) native_res_m <- res_m

  # Filter out NAs for robustness
  n_before <- nrow(coords)
  coords <- coords[!is.na(coords$xcoord) & !is.na(coords$ycoord), ]
  if (nrow(coords) == 0) return(NULL)
  n_dropped <- n_before - nrow(coords)
  if (n_dropped > 0) {
    warning(sprintf(
      "EEA grid: %d of %d unique cell codes could not be resolved to coordinates and were excluded.",
      n_dropped, n_before
    ))
  }

  # 3. Build square polygons directly from lower-left corners
  polys <- mapply(make_square_poly_ll, coords$xcoord, coords$ycoord,
                  MoreArgs = list(res = native_res_m), SIMPLIFY = FALSE)
  sfc <- sf::st_sfc(polys, crs = "EPSG:3035")
  grid <- sf::st_sf(cellCode = coords$cellCode, geometry = sfc)

  grid <- sf::st_transform(grid, projection)
  grid$cellid <- seq_len(nrow(grid))
  grid$area <- grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  grid <- grid %>%
    dplyr::select("cellid", tidyselect::any_of("cellCode"), "area", tidyselect::everything()) %>%
    dplyr::distinct(cellCode, .keep_all = TRUE)

  return(grid)
}

#' Create a native EQDGC grid
#' @param df Data frame with `cellCode` and `resolution`.
#' @param projection The projected CRS the grid should be returned in.
#' @noRd
create_eqdgc_grid <- function(df, projection, resolution = NULL) {
  cellCode <- long <- lat <- cellid <- area <- NULL

  res_str <- if (is.null(resolution) && "resolution" %in% names(df)) {
    df$resolution[1]
  } else {
    resolution
  }

  if (is.null(res_str) || is.na(res_str)) {
    res_deg <- 0.25 # Default
  } else {
    res_deg <- as.numeric(gsub("degrees", "", res_str))
  }
  half_res <- res_deg / 2

  # 2. Convert cell codes to center coordinates in lat/long (EPSG:4326)
  unique_codes <- unique(df$cellCode)
  latlong <- convert_eqdgc_latlong(unique_codes)
  latlong_df <- as.data.frame(latlong)
  latlong_df$cellCode <- unique_codes

  # Filter NAs
  n_before <- nrow(latlong_df)
  latlong_df <- latlong_df[!is.na(latlong_df$long) & !is.na(latlong_df$lat), ]
  if (nrow(latlong_df) == 0) return(NULL)
  n_dropped <- n_before - nrow(latlong_df)
  if (n_dropped > 0) {
    warning(sprintf(
      "EQDGC grid: %d of %d unique cell codes could not be resolved to coordinates and were excluded.",
      n_dropped, n_before
    ))
  }

  # 3. Build square polygons directly from center coordinates
  polys <- mapply(make_square_poly, latlong_df$long, latlong_df$lat,
                  MoreArgs = list(half_res = half_res), SIMPLIFY = FALSE)
  sfc <- sf::st_sfc(polys, crs = "EPSG:4326")
  grid <- sf::st_sf(cellCode = latlong_df$cellCode, geometry = sfc)

  grid <- sf::st_transform(grid, projection)
  grid$cellid <- seq_len(nrow(grid))
  grid$area <- grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  grid <- grid %>%
    dplyr::select(cellid, tidyselect::any_of("cellCode"), area, tidyselect::everything())

  return(grid)
}
