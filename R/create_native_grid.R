create_native_grid <- function(df, projection, grid_type, resolution = NULL) {
  if (grid_type == "mgrs") {
    create_mgrs_grid(df, projection, resolution)
  } else if (grid_type == "eea") {
    create_eea_grid(df, projection, resolution)
  } else if (grid_type == "eqdgc") {
    create_eqdgc_grid(df, projection, resolution)
  }
}

#' @noRd
create_mgrs_grid <- function(df, projection, resolution = NULL) {
  df_unique <- df[!duplicated(df$cellCode), ]
  
  # Always recalculate UTM info to ensure consistency with cellCode
  utm_info <- mgrs::mgrs_to_utm(df_unique$cellCode)
  df_unique$xcoord <- utm_info$easting
  df_unique$ycoord <- utm_info$northing
  df_unique$utmzone <- utm_info$zone
  df_unique$hemisphere <- utm_info$hemisphere
  
  # Calculate resolution in meters
  # Extract numeric part and units
  res_str <- if ("resolution" %in% names(df_unique)) {
    df_unique$resolution[1]
  } else {
    resolution
  }
  
  if (is.null(res_str) || is.na(res_str)) {
    # Default to 1km if not found
    res_m <- 1000
  } else {
    res_val <- as.numeric(gsub("[a-zA-Z]", "", res_str))
    res_unit <- gsub("[0-9.]", "", res_str)
    res_m <- if (res_unit == "km") res_val * 1000 else res_val
  }
  half_res <- res_m / 2
  
  # Group by UTM zone and hemisphere
  df_list <- split(df_unique, list(df_unique$utmzone, df_unique$hemisphere), drop = TRUE)
  
  all_grids <- lapply(df_list, function(group_df) {
    zone <- as.numeric(group_df$utmzone[1])
    hemi <- group_df$hemisphere[1]
    # Handle padding for zones 1-9
    epsg_code <- if (hemi %in% c("North", "N")) 32600 + zone else 32700 + zone
    
    make_square <- function(e, n) {
      pts <- matrix(c(
        e - half_res, n - half_res,
        e + half_res, n - half_res,
        e + half_res, n + half_res,
        e - half_res, n + half_res,
        e - half_res, n - half_res
      ), ncol = 2, byrow = TRUE)
      sf::st_polygon(list(pts))
    }
    
    polys <- lapply(seq_len(nrow(group_df)), function(i) {
      make_square(group_df$xcoord[i], group_df$ycoord[i])
    })
    
    group_grid <- sf::st_sf(
      cellid = seq_len(nrow(group_df)), 
      cellCode = group_df$cellCode,
      geometry = sf::st_sfc(polys, crs = epsg_code),
      stringsAsFactors = FALSE
    )
    
    sf::st_transform(group_grid, projection)
  })
  
  grid <- do.call(rbind, all_grids)
  grid <- sf::st_make_valid(grid)
  grid$cellid <- seq_len(nrow(grid))
  
  # Add area
  grid$area <- grid %>%
    sf::st_area() %>%
    units::set_units("km^2")
    
  return(grid[, c("cellid", "cellCode", "area", "geometry")])
}

#' Create a native EEA grid
#' @param df Data frame with `cellCode` and `resolution`.
#' @param projection The projected CRS the grid should be returned in.
#' @noRd
create_eea_grid <- function(df, projection, resolution = NULL) {
  df_unique <- df[!duplicated(df$cellCode), ]
  
  # EEA is already in EPSG:3035
  coords <- eea_code_to_coords(df_unique$cellCode)
  
  # Resolution handling
  res_str <- if ("resolution" %in% names(df_unique)) {
    df_unique$resolution[1]
  } else {
    resolution
  }
  
  if (is.null(res_str) || is.na(res_str)) {
    res_m <- as.numeric(gsub("km", "", coords$resolution[1])) * 1000
  } else {
    res_val <- as.numeric(gsub("[a-zA-Z]", "", res_str))
    res_unit <- gsub("[0-9.]", "", res_str)
    res_m <- if (res_unit == "km") res_val * 1000 else res_val
  }
  half_res <- res_m / 2
  
  make_square <- function(x, y) {
    pts <- matrix(c(
      x - half_res, y - half_res,
      x + half_res, y - half_res,
      x + half_res, y + half_res,
      x - half_res, y + half_res,
      x - half_res, y - half_res
    ), ncol = 2, byrow = TRUE)
    sf::st_polygon(list(pts))
  }
  
  polys <- lapply(seq_len(nrow(coords)), function(i) {
    make_square(coords$xcoord[i], coords$ycoord[i])
  })
  
  grid <- sf::st_sf(
    cellid = seq_len(nrow(df_unique)),
    cellCode = df_unique$cellCode,
    geometry = sf::st_sfc(polys, crs = "EPSG:3035"),
    stringsAsFactors = FALSE
  )
  
  grid <- sf::st_transform(grid, projection)
  grid$area <- grid %>%
    sf::st_area() %>%
    units::set_units("km^2")
    
  return(grid[, c("cellid", "cellCode", "area", "geometry")])
}

#' Create a native EQDGC grid
#' @param df Data frame with `cellCode` and `resolution`.
#' @param projection The projected CRS the grid should be returned in.
#' @noRd
create_eqdgc_grid <- function(df, projection, resolution = NULL) {
  df_unique <- df[!duplicated(df$cellCode), ]
  
  # EQDGC is in degrees (EPSG:4326)
  latlong <- convert_eqdgc_latlong(df_unique$cellCode)
  
  # Determine resolution in degrees
  # EQDGC resolution is e.g. "0.25degrees"
  res_str <- if ("resolution" %in% names(df_unique)) {
    df_unique$resolution[1]
  } else {
    resolution
  }
  
  if (is.null(res_str) || is.na(res_str)) {
    res_deg <- 0.25 # Default
  } else {
    res_deg <- as.numeric(gsub("degrees", "", res_str))
  }
  half_res <- res_deg / 2
  
  make_rect <- function(lat, lon) {
    pts <- matrix(c(
      lon - half_res, lat - half_res,
      lon + half_res, lat - half_res,
      lon + half_res, lat + half_res,
      lon - half_res, lat + half_res,
      lon - half_res, lat - half_res
    ), ncol = 2, byrow = TRUE)
    sf::st_polygon(list(pts))
  }
  
  polys <- lapply(seq_len(nrow(latlong)), function(i) {
    make_rect(latlong[i, 1], latlong[i, 2])
  })
  
  grid <- sf::st_sf(
    cellid = seq_len(nrow(df_unique)),
    cellCode = df_unique$cellCode,
    geometry = sf::st_sfc(polys, crs = "EPSG:4326"),
    stringsAsFactors = FALSE
  )
  
  grid <- sf::st_transform(grid, projection)
  grid$area <- grid %>%
    sf::st_area() %>%
    units::set_units("km^2")
    
  return(grid[, c("cellid", "cellCode", "area", "geometry")])
}
