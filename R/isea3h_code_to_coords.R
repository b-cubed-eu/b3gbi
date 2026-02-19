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

  return(data.frame(
    xcoord = as.numeric(unique_results[idx, 1]),
    ycoord = as.numeric(unique_results[idx, 2]),
    resolution = as.character(as.integer(unique_results[idx, 3]))
  ))
}

#' Create Hexagonal Polygons from ISEA3H Centroids
#'
#' @param df Data frame with `xcoord`, `ycoord`, and `cellCode` columns.
#' @return An sf object with hexagonal polygon geometries.
#' @noRd
create_isea3h_grid <- function(df) {
  hex_side <- 0.01
  make_hex <- function(lon, lat, side) {
    if (is.na(lon) || is.na(lat)) {
      return(sf::st_polygon())
    }
    angles <- seq(0, 2 * pi, length.out = 7)
    x <- lon + side * cos(angles)
    y <- lat + (side * 1.2) * sin(angles)
    sf::st_polygon(list(cbind(x, y)))
  }
  hex_list <- lapply(
    seq_len(nrow(df)),
    function(i) make_hex(df$xcoord[i], df$ycoord[i], hex_side)
  )
  sf::st_sf(
    cellid = seq_len(nrow(df)),
    cellCode = df$cellCode,
    geometry = sf::st_sfc(hex_list, crs = "EPSG:4326")
  )
}
