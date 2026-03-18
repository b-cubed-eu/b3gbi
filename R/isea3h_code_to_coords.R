#' Convert ISEA3H Cell Codes to Coordinates (Pure R)
#'
#' Decodes GBIF ISEA3H cell codes (Mocnik identifier scheme) into latitude and
#' longitude coordinates using a native R implementation.
#'
#' The Mocnik ID is a decimal encoding of the grid cell's center point:
#' - Sign: positive = hexagon, negative = pentagon
#' - First 2 digits: resolution (offset by +22 for negative lat, +44 for
#'   negative lon, +66 for both negative)
#' - Next 8 digits: abs(latitude) with 6 decimal places
#' - Next 9 digits: abs(longitude) with 6 decimal places
#'
#' Formula: id = sign * (res_code * 10^17 + round(|lat| * 10^6) * 10^9 +
#'   round(|lon| * 10^6))
#'
#' @param cell_codes A character or numeric vector of ISEA3H cell codes
#'   (Long integers).
#' @return A data frame with columns `xcoord` (longitude), `ycoord` (latitude),
#'   and `resolution`.
#' @noRd
isea3h_code_to_coords <- function(cell_codes) {
  if (!requireNamespace("bit64", quietly = TRUE)) {
    stop("Package 'bit64' is required for ISEA3H decoding.")
  }

  pow17 <- bit64::as.integer64("100000000000000000")
  pow9 <- bit64::as.integer64("1000000000")

  decode_single <- function(cell_code) {
    if (is.na(cell_code) || cell_code == "") {
      return(c(NA_real_, NA_real_, NA_real_))
    }

    code_str <- as.character(cell_code)

    # Sign: positive = hexagon, negative = pentagon
    is_pentagon <- startsWith(code_str, "-")
    abs_str <- gsub("^-", "", code_str)

    id <- bit64::as.integer64(abs_str)

    # Extract components using integer division
    res_code <- as.double(id %/% pow17)
    remainder <- id - bit64::as.integer64(res_code) * pow17

    lat_scaled <- as.double(remainder %/% pow9)
    lon_scaled <- as.double(remainder - bit64::as.integer64(lat_scaled) * pow9)

    # Decode latitude and longitude from scaled integers
    lat <- lat_scaled / 1e6
    lon <- lon_scaled / 1e6

    # Determine lat/lon signs from resolution offset
    if (res_code >= 66) {
      resolution <- res_code - 66
      lat <- -lat
      lon <- -lon
    } else if (res_code >= 44) {
      resolution <- res_code - 44
      lon <- -lon
    } else if (res_code >= 22) {
      resolution <- res_code - 22
      lat <- -lat
    } else {
      resolution <- res_code
    }

    return(c(lon, lat, resolution))
  }

  # Helper for robust projection check
  has_proj_isea <- function() {
    tryCatch(
      {
        sf::st_crs("+proj=isea")$input == "+proj=isea"
      },
      error = function(e) FALSE
    )
  }

  n <- length(cell_codes)
  if (n == 0) {
    return(data.frame(
      xcoord = numeric(),
      ycoord = numeric(),
      resolution = character()
    ))
  }

  # Optimize by decoding unique codes only
  unique_codes <- unique(cell_codes)
  unique_results <- do.call(rbind, lapply(unique_codes, decode_single))

  # Map back to the full vector using match indices
  idx <- match(as.character(cell_codes), as.character(unique_codes))

  res <- data.frame(
    xcoord = as.numeric(unique_results[idx, 1]),
    ycoord = as.numeric(unique_results[idx, 2]),
    resolution = as.character(as.integer(unique_results[idx, 3]))
  )

  # Check for cellCode sign (negative = pentagon)
  # Correct for coordinate sign based on GBIF ISEA3H Mocnik ID scheme
  # cellCode < 0 indicates a pentagon, which doesn't change coordinates but is tracked
  res$is_pentagon <- startsWith(as.character(cell_codes), "-")

  return(res)
}

#' Create a Hexagonal Grid for ISEA3H Data
#'
#' Creates one hexagonal polygon per unique cellCode, centred on the decoded
#' centroid. Hexagons are constructed in a local equal-area projection
#' (\code{+proj=isea} or fallback \code{+proj=laea}) where the ISEA3H grid
#' forms a regular hexagonal lattice, then transformed to the requested
#' projection for display.
#' The grid is locked to the cube input — no resizing or spatial reassignment.
#'
#' @param df Data frame with `xcoord`, `ycoord`, and `cellCode` columns.
#' @param projection The projected CRS the grid should be returned in.
#' @return An sf object with columns `cellid`, `cellCode`, and `area`.
#' @noRd
create_isea3h_grid <- function(df, projection) {
  df_unique <- df[!duplicated(df$cellCode), ]

  use_dggridR <- getOption("b3gbi.use_dggridR", TRUE)

  if (use_dggridR && requireNamespace("dggridR", quietly = TRUE)) {
    # --- dggridR Strategy ---
    # Determine resolution from the first decoded coordinate
    coords_df <- isea3h_code_to_coords(df_unique$cellCode)
    res <- as.numeric(coords_df$resolution[1])

    # Construct the DGG
    dggs <- dggridR::dgconstruct(res = res, projection = "ISEA", topology = "HEXAGON", aperture = 3)

    # Get dggridR sequence numbers for the decoded coordinates
    seqnums <- dggridR::dgGEO_to_SEQNUM(dggs, coords_df$xcoord, coords_df$ycoord)$seqnum

    # Check for duplicates or missing geometries. If seqnums are identical for
    # different cellCodes, the points resolved to the same DGG cell.
    unique_seqnums <- unique(seqnums)

    # Generate spatial polygons for these sequence numbers
    grid <- dggridR::dgcellstogrid(dggs, unique_seqnums)

    # Grid returns with CRS 4326. Rename cell to seqnum to avoid conflict
    names(grid)[names(grid) == "cell"] <- "seqnum"

    # Merge the original B-Cubed Mocnik sequence ids back to the grid
    # using the discovered sequence numbers
    lookup <- data.frame(
      cellCode = df_unique$cellCode,
      seqnum = seqnums
    )
    # Deduplicate the lookup just in case multiple Mocnik IDs mapped to the same DGG cell
    lookup <- lookup[!duplicated(lookup$seqnum), ]

    grid <- merge(grid, lookup, by = "seqnum")

    # Reorder and format to match standard output expectations
    grid$cellid <- seq_len(nrow(grid))
    grid <- grid[, c("cellid", "cellCode", "geometry")]

    # Transform to requested projection
    if (projection != "+proj=longlat +datum=WGS84") {
      grid <- sf::st_transform(grid, crs = projection)
    }
  } else {
    # --- Native R Fallback Strategy ---
    # The ISEA3H grid is defined on the Icosahedral Snyder Equal Area (ISEA)
    # projection. In +proj=isea, the centroids form a regular
    # hexagonal lattice (NN distance variance < 0.001%).
    if (!use_dggridR) {
      message("Option 'b3gbi.use_dggridR' is FALSE. Falling back to native R geometry generation.")
    } else {
      message("Package 'dggridR' not available. Falling back to native R geometry generation. Install 'dggridR' for topologically correct polygons.")
    }

    # Identify working projection system
    isea_crs <- "+proj=isea"
    has_isea <- tryCatch(
      {
        # Test if transform works
        test_pt <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
        sf::st_transform(test_pt, crs = isea_crs)
        TRUE
      },
      error = function(e) FALSE
    )

    if (!has_isea) {
      # Fallback to LAEA centered on data if ISEA is missing (common on Windows PROJ)
      clon <- mean(df_unique$xcoord, na.rm = TRUE)
      clat <- mean(df_unique$ycoord, na.rm = TRUE)
      isea_crs <- paste0(
        "+proj=laea +lat_0=", round(clat, 2),
        " +lon_0=", round(clon, 2),
        " +datum=WGS84 +units=m"
      )
      message("System PROJ lacks '+proj=isea'. Using local LAEA fallback.")
    }

    centroids_sf <- sf::st_as_sf(df_unique,
      coords = c("xcoord", "ycoord"),
      crs = 4326
    )
    centroids_isea <- sf::st_transform(centroids_sf, crs = isea_crs)
    coords_isea <- sf::st_coordinates(centroids_isea)

    # Check for empty geometries from transform (indicates projection failure)
    if (nrow(coords_isea) == 0 || all(is.na(coords_isea[, 1]))) {
      stop("Failed to project ISEA3H centroids. Check if your PROJ installation supports '+proj=isea' or provide a different coordinate system.")
    }

    # --- Filter spatial outliers ---
    # Points near icosahedral face boundaries can project to distant faces.
    qx <- stats::quantile(coords_isea[, 1], probs = c(0.25, 0.75), na.rm = TRUE)
    qy <- stats::quantile(coords_isea[, 2], probs = c(0.25, 0.75), na.rm = TRUE)
    iqr_x <- qx[2] - qx[1]
    iqr_y <- qy[2] - qy[1]
    keep <- coords_isea[, 1] >= (qx[1] - 5 * iqr_x) &
      coords_isea[, 1] <= (qx[2] + 5 * iqr_x) &
      coords_isea[, 2] >= (qy[1] - 5 * iqr_y) &
      coords_isea[, 2] <= (qy[2] + 5 * iqr_y)
    keep[is.na(keep)] <- FALSE
    coords_main <- coords_isea[keep, , drop = FALSE]

    # --- Compute hex side from median NN distance ---
    if (nrow(coords_main) < 2) {
      # Single point case: estimate side from resolution code if possible,
      # but usually we have multiple. Fallback to a default if really solitary.
      side_m <- 1000 # 1km default
    } else {
      n_pts <- nrow(coords_main)
      n_sample <- min(n_pts, 500)
      sample_idx <- if (n_pts > 500) sample(n_pts, 500) else seq_len(n_pts)
      nn_dists <- numeric(n_sample)
      for (i in seq_len(n_sample)) {
        si <- sample_idx[i]
        dists <- sqrt(
          (coords_main[, 1] - coords_main[si, 1])^2 +
            (coords_main[, 2] - coords_main[si, 2])^2
        )
        dists[si] <- Inf
        nn_dists[i] <- min(dists)
      }
      side_m <- (stats::median(nn_dists) / sqrt(3)) * 1.05 # tight fit
    }

    # --- Create hexagonal polygons ---
    # Safety: ensure side_m is not zero or too tiny, avoiding degenerate geometries
    if (!is.finite(side_m) || side_m < 10) {
      side_m <- 10 # 10m minimum
    }

    make_hex <- function(x, y) {
      if (is.na(x) || is.na(y)) {
        return(sf::st_polygon())
      }
      # Create 6 vertices
      angles <- seq(0, 2 * pi, length.out = 7)
      px <- x + side_m * cos(angles)
      py <- y + side_m * sin(angles)
      # Explicitly close the polygon to avoid precision issues
      px[7] <- px[1]
      py[7] <- py[1]
      pts <- matrix(c(px, py), ncol = 2)
      sf::st_polygon(list(pts))
    }

    hex_list <- lapply(seq_len(nrow(coords_isea)), function(i) {
      make_hex(coords_isea[i, 1], coords_isea[i, 2])
    })

    grid <- sf::st_sf(
      cellid = seq_len(nrow(df_unique)),
      cellCode = df_unique$cellCode,
      geometry = sf::st_sfc(hex_list, crs = isea_crs),
      stringsAsFactors = FALSE
    )

    # Transform to requested projection — tryCatch prevents PROJ crashes
    # (some PROJ installations segfault on certain LAEA pipelines)
    grid <- tryCatch(
      {
        grid %>%
          sf::st_transform(crs = projection) %>%
          sf::st_make_valid() %>%
          sf::st_cast("POLYGON", warn = FALSE)
      },
      error = function(e) {
        warning("PROJ transform failed for ISEA3H grid: ", e$message)
        # Return empty sf with correct schema
        sf::st_sf(
          cellid = integer(0),
          cellCode = character(0),
          geometry = sf::st_sfc(crs = projection),
          stringsAsFactors = FALSE
        )
      }
    )

    # Ensure it's not empty after transform
    if (nrow(grid) > 0 && any(sf::st_is_empty(grid))) {
      grid <- grid[!sf::st_is_empty(grid), ]
    }
  }

  # Add area column (matches create_grid output)
  grid$area <- grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  return(grid)
}
