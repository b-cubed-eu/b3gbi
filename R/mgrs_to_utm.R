#' Convert MGRS grid references to UTM coordinates
#'
#' Pure-R conversion of Military Grid Reference System (MGRS) codes to
#' Universal Transverse Mercator (UTM) coordinates on the WGS84 ellipsoid.
#' Returns the south-west (lower-left) corner of each grid square, which is how
#' MGRS references are defined. Implemented from the published MGRS/UTM
#' specification (NGA.SIG.0012) so that b3gbi does not depend on external
#' compiled code.
#'
#' Codes in the polar UPS zones (latitude bands A, B, Y, Z) and malformed codes
#' return `NA` (with a single warning summarising how many failed).
#'
#' @param x Character vector of MGRS codes, e.g. `"32UNG1234"`. Spaces are
#'   ignored and letters may be lower case.
#'
#' @return A data frame with one row per input code and the columns `mgrs`
#'   (input code), `zone` (integer UTM zone), `hemisphere` (`"N"` or `"S"`),
#'   `easting` and `northing` (metres).
#'
#' @noRd
mgrs_to_utm <- function(x) {

  x_in <- as.character(x)
  code <- toupper(gsub("\\s", "", x_in))

  n <- length(code)
  out <- data.frame(
    mgrs = x_in,
    zone = rep(NA_integer_, n),
    hemisphere = rep(NA_character_, n),
    easting = rep(NA_real_, n),
    northing = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )
  if (n == 0) return(out)

  # zone (1-2 digits), latitude band, 100 km column and row letters, digits
  pattern <- "^([0-9]{1,2})([C-HJ-NP-X])([A-HJ-NP-Z])([A-HJ-NP-V])([0-9]*)$"
  ok <- !is.na(code) & grepl(pattern, code)

  zone <- suppressWarnings(as.integer(sub(pattern, "\\1", code)))
  band <- sub(pattern, "\\2", code)
  col_letter <- sub(pattern, "\\3", code)
  row_letter <- sub(pattern, "\\4", code)
  digits <- sub(pattern, "\\5", code)

  ok <- ok & !is.na(zone) & zone >= 1 & zone <= 60 & nchar(digits) %% 2 == 0 &
    nchar(digits) <= 10

  # 100 km column letters: three sets that repeat every three zones
  col_sets <- list(LETTERS[c(1:8)],                     # A-H (zones 1, 4, ...)
                   c(LETTERS[10:14], LETTERS[16:18]),   # J-R (zones 2, 5, ...)
                   LETTERS[19:26])                      # S-Z (zones 3, 6, ...)
  row_letters <- c(LETTERS[1:8], LETTERS[10:14], LETTERS[16:22]) # A-V w/o I, O

  col_index <- rep(NA_integer_, n)
  row_index <- rep(NA_integer_, n)
  for (i in which(ok)) {
    set <- col_sets[[(zone[i] - 1) %% 3 + 1]]
    col_index[i] <- match(col_letter[i], set)
    row_index[i] <- match(row_letter[i], row_letters)
  }
  ok <- ok & !is.na(col_index) & !is.na(row_index)

  if (any(ok)) {
    # Numerical part: half the digits are easting, half northing
    n_dig <- nchar(digits[ok]) / 2
    precision <- 10^(5 - n_dig)
    east_str <- substr(digits[ok], 1, n_dig)
    north_str <- substr(digits[ok], n_dig + 1, 2 * n_dig)
    east_num <- ifelse(n_dig == 0, 0, suppressWarnings(as.numeric(east_str)))
    north_num <- ifelse(n_dig == 0, 0, suppressWarnings(as.numeric(north_str)))

    easting <- col_index[ok] * 100000 + east_num * precision

    # Row letters cycle every 2,000 km; even zones are offset by 5 letters
    row_offset <- ifelse(zone[ok] %% 2 == 0, 5, 0)
    northing_100k <- ((row_index[ok] - 1 - row_offset) %% 20) * 100000
    northing_base <- northing_100k + north_num * precision

    # Resolve the 2,000 km ambiguity with the latitude band's minimum northing
    min_northing <- mgrs_band_min_northing(band[ok])
    k <- ceiling((min_northing - northing_base) / 2000000)
    k[k < 0] <- 0
    northing <- northing_base + k * 2000000

    out$zone[ok] <- zone[ok]
    out$hemisphere[ok] <- ifelse(band[ok] %in% c("N", "P", LETTERS[17:24]), "N", "S")
    out$easting[ok] <- easting
    out$northing[ok] <- northing
  }

  n_bad <- sum(!is.na(code) & !ok)
  if (n_bad > 0) {
    warning(sprintf(
      "%d of %d MGRS code(s) could not be converted to UTM and were set to NA.",
      n_bad, n
    ), call. = FALSE)
  }

  out
}

#' Minimum UTM northing of an MGRS latitude band
#'
#' Northing of the band's southern edge on the central meridian (the lowest
#' northing anywhere in the band), rounded down to the nearest 100 km so that
#' 100 km grid squares straddling the band edge are handled correctly.
#'
#' @param band Character vector of latitude band letters (C-X, without I, O).
#' @return Numeric vector of minimum northings in metres.
#' @noRd
mgrs_band_min_northing <- function(band) {
  bands <- c(LETTERS[3:8], LETTERS[10:14], LETTERS[16:24]) # C-X without I, O
  lat_min <- -80 + (match(band, bands) - 1) * 8            # X starts at 72
  northing <- utm_meridian_northing(lat_min)
  floor(northing / 100000) * 100000
}

#' UTM northing on the central meridian for a given latitude (WGS84)
#'
#' @param lat Latitude in decimal degrees.
#' @return Northing in metres (with the 10,000 km false northing applied in the
#'   southern hemisphere).
#' @noRd
utm_meridian_northing <- function(lat) {
  a <- 6378137
  f <- 1 / 298.257223563
  k0 <- 0.9996
  e2 <- f * (2 - f)
  e4 <- e2^2
  e6 <- e2^3
  phi <- lat * pi / 180

  # Meridian arc length from the equator
  m <- a * ((1 - e2 / 4 - 3 * e4 / 64 - 5 * e6 / 256) * phi -
              (3 * e2 / 8 + 3 * e4 / 32 + 45 * e6 / 1024) * sin(2 * phi) +
              (15 * e4 / 256 + 45 * e6 / 1024) * sin(4 * phi) -
              (35 * e6 / 3072) * sin(6 * phi))

  northing <- k0 * m
  ifelse(lat < 0, northing + 10000000, northing)
}
