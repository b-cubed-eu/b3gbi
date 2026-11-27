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

    grid <- build_native_isea3h_cells(df_unique, projection)
    grid <- sf::st_sf(
      cellid = seq_len(nrow(grid)),
      cellCode = grid$cellCode,
      geometry = sf::st_geometry(grid),
      stringsAsFactors = FALSE
    )
  }

  # Add area column (matches create_grid output)
  grid$area <- grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  return(grid)
}

#' Does the system PROJ convert to and from a given CRS?
#'
#' The native ISEA3H fallback builds hexagons in \code{+proj=isea} and
#' transforms them back, so it needs both directions. Older PROJ versions
#' (< 9.5) have no inverse for \code{+proj=isea}.
#'
#' @param crs CRS to test (default \code{"+proj=isea"}).
#' @return \code{TRUE} if a test point survives the round trip, otherwise
#'   \code{FALSE}.
#' @noRd
proj_supports_isea_roundtrip <- function(crs = "+proj=isea") {
  tryCatch(
    {
      test_pt <- sf::st_sfc(sf::st_point(c(10, 50)), crs = 4326)
      fwd <- sf::st_transform(test_pt, crs = crs)
      back <- suppressWarnings(sf::st_transform(fwd, crs = 4326))
      xy <- sf::st_coordinates(back)
      all(is.finite(xy)) && abs(xy[1, 1] - 10) < 1e-6 &&
        abs(xy[1, 2] - 50) < 1e-6
    },
    error = function(e) FALSE
  )
}

#' Expected side length (m) of an ISEA3H hexagon at a given resolution
#'
#' An aperture-3 ISEA grid at resolution r has 10 * 3^r + 2 cells of (nearly)
#' equal area on the authalic sphere.
#' @noRd
isea3h_hex_side <- function(resolution) {
  r_authalic <- 6371007.181
  n_cells <- 10 * 3^resolution + 2
  cell_area <- 4 * pi * r_authalic^2 / n_cells
  sqrt(2 * cell_area / (3 * sqrt(3)))
}

#' Vertices of a regular polygon, densified along its edges
#'
#' @return A matrix of x, y coordinates (closed ring).
#' @noRd
regular_polygon_ring <- function(x, y, radius, n_sides = 6,
                                 points_per_edge = 8) {
  angles <- seq(0, 2 * pi, length.out = n_sides + 1)
  vx <- x + radius * cos(angles)
  vy <- y + radius * sin(angles)
  t <- seq(0, 1, length.out = points_per_edge + 1)[-(points_per_edge + 1)]
  rx <- unlist(lapply(seq_len(n_sides), function(i) {
    vx[i] + t * (vx[i + 1] - vx[i])
  }))
  ry <- unlist(lapply(seq_len(n_sides), function(i) {
    vy[i] + t * (vy[i + 1] - vy[i])
  }))
  cbind(c(rx, rx[1]), c(ry, ry[1]))
}

#' Turn a longitude/latitude ring into a polygon, handling the antimeridian
#'
#' Longitudes are unwrapped relative to the cell centre so that a cell
#' crossing the antimeridian does not span the globe.
#' @noRd
lonlat_ring_to_polygon <- function(ring, centre_lon) {
  d <- ring[, 1] - centre_lon
  ring[, 1] <- ring[, 1] - 360 * round(d / 360)
  sf::st_polygon(list(ring))
}

#' Build ISEA3H cell polygons without dggridR
#'
#' Hexagons are built in the ISEA projection (where the ISEA3H cell centres
#' form a regular lattice on each icosahedron face) when the system PROJ can
#' convert to and from it. Cells that cannot be built there, such as cells
#' straddling the edges between icosahedron faces, and the twelve pentagons
#' (centred on the icosahedron's vertices) are built as regular polygons in a
#' Lambert azimuthal equal-area projection centred on the cell. The cell size
#' follows from the grid resolution encoded in the cell codes.
#'
#' Cell shapes are approximations; install dggridR for exact cell geometries.
#'
#' @param df Data frame with `cellCode`, `xcoord` and `ycoord` (one row per
#'   cell).
#' @param projection CRS of the output.
#' @return An sf object with column `cellCode`, in `projection`.
#' @noRd
build_native_isea3h_cells <- function(df, projection) {
  n <- nrow(df)
  lon <- df$xcoord
  lat <- df$ycoord
  codes <- as.character(df$cellCode)
  is_pentagon <- startsWith(codes, "-")

  # Cell size from the resolution encoded in the codes; fall back to the
  # spacing of the cell centres when it cannot be decoded
  res <- tryCatch(
    suppressWarnings(as.numeric(isea3h_code_to_coords(codes)$resolution)),
    error = function(e) rep(NA_real_, n)
  )
  side <- isea3h_hex_side(res)
  if (any(is.na(side))) {
    side[is.na(side)] <- isea3h_side_from_spacing(lon, lat)
  }
  hex_area <- 3 * sqrt(3) / 2 * side^2

  polys <- vector("list", n)
  ok <- rep(FALSE, n)

  # Cell areas are checked on the sphere (s2)
  old_s2 <- suppressMessages(sf::sf_use_s2(TRUE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

  isea_crs <- "+proj=isea"
  if (proj_supports_isea_roundtrip(isea_crs)) {
    xy <- tryCatch(
      sf::sf_project("EPSG:4326", isea_crs, cbind(lon, lat), keep = TRUE,
                     warn = FALSE),
      error = function(e) matrix(NA_real_, n, 2)
    )
    for (i in which(!is_pentagon & is.finite(xy[, 1]) & is.finite(xy[, 2]))) {
      ring <- regular_polygon_ring(xy[i, 1], xy[i, 2], side[i], 6)
      ll <- project_ring_lonlat(isea_crs, ring)
      if (!is.null(ll)) {
        polys[[i]] <- lonlat_ring_to_polygon(ll, lon[i])
        ok[i] <- TRUE
      }
    }
    # Reject cells distorted by crossing an icosahedron edge
    if (any(ok)) {
      areas <- rep(NA_real_, n)
      areas[ok] <- suppressMessages(as.numeric(sf::st_area(
        sf::st_sfc(polys[ok], crs = 4326)
      )))
      ok <- ok & is.finite(areas) & abs(areas / hex_area - 1) < 0.05
    }
  } else {
    message("System PROJ cannot convert to and from '+proj=isea'. ",
            "Building cells in local equal-area projections.")
  }

  # Remaining cells (and pentagons): local LAEA centred on each cell
  for (i in which(!ok)) {
    if (!is.finite(lon[i]) || !is.finite(lat[i])) {
      polys[[i]] <- sf::st_polygon()
      next
    }
    laea <- sprintf("+proj=laea +lat_0=%.8f +lon_0=%.8f +datum=WGS84 +units=m",
                    lat[i], lon[i])
    n_sides <- if (is_pentagon[i]) 5 else 6
    ring <- regular_polygon_ring(0, 0, side[i], n_sides)
    ll <- project_ring_lonlat(laea, ring)
    if (!is.null(ll)) {
      polys[[i]] <- lonlat_ring_to_polygon(ll, lon[i])
    } else {
      polys[[i]] <- sf::st_polygon()
    }
  }

  grid <- sf::st_sf(
    cellCode = df$cellCode,
    geometry = sf::st_sfc(polys, crs = 4326),
    stringsAsFactors = FALSE
  )
  grid <- grid[!sf::st_is_empty(grid), ]

  # Split cells that cross the antimeridian
  crosses <- vapply(sf::st_geometry(grid), function(g) {
    any(abs(sf::st_coordinates(g)[, 1]) > 180)
  }, logical(1))
  if (any(crosses)) {
    sf::st_geometry(grid)[crosses] <- sf::st_wrap_dateline(
      sf::st_geometry(grid)[crosses],
      options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")
    )
  }

  if (sf::st_crs(projection) != sf::st_crs(4326)) {
    grid <- sf::st_transform(grid, crs = projection)
  }
  suppressMessages(sf::sf_use_s2(FALSE))
  grid <- sf::st_make_valid(grid)
  if (any(!sf::st_geometry_type(grid) %in% c("POLYGON", "MULTIPOLYGON"))) {
    grid <- sf::st_collection_extract(grid, "POLYGON", warn = FALSE) # nocov
  }
  grid
}

#' Hexagon side length (m) estimated from the spacing of cell centres
#' @noRd
isea3h_side_from_spacing <- function(lon, lat) {
  keep <- is.finite(lon) & is.finite(lat)
  lon <- lon[keep]
  lat <- lat[keep]
  if (length(lon) < 2) return(1000)
  pts <- sf::st_sfc(lapply(seq_along(lon), function(i) {
    sf::st_point(c(lon[i], lat[i]))
  }), crs = 4326)
  d <- suppressMessages(as.numeric(sf::st_distance(pts)))
  d <- matrix(d, length(lon))
  diag(d) <- Inf
  max(stats::median(apply(d, 1, min)) / sqrt(3), 10)
}

#' Project a ring of coordinates to longitude/latitude
#'
#' @return The projected coordinates, or `NULL` if the projection fails or
#'   gives non-finite coordinates.
#' @noRd
project_ring_lonlat <- function(from, ring) {
  ll <- tryCatch(
    sf::sf_project(from, "EPSG:4326", ring, keep = TRUE, warn = FALSE),
    error = function(e) NULL
  )
  if (is.null(ll) || !all(is.finite(ll))) NULL else ll
}
