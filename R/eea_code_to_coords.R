#' Convert EEA reference grid cell codes to coordinates
#'
#' Converts EEA reference grid cell codes (e.g. `"10kmE432N321"`) to the
#' coordinates of the lower-left corner of each cell in metres (EPSG:3035).
#'
#' Following the EEA reference grid / INSPIRE naming rule, the easting and
#' northing in a cell code are the coordinates in metres divided by 10^n, where
#' n is the number of trailing zeros of the cell size in metres. The
#' coordinates are therefore recovered by multiplying by 10^n, e.g.:
#' `100kmE51N29` (x 10^5), `10kmE510N293` (x 10^4), `5kmE5100N2930` and
#' `1kmE5105N2933` (x 10^3), `100mE51052N29336` (x 10^2), `250mE1025N22000`
#' (x 10^1) and `25mE5105200N2933600` (x 1: 25 has no trailing zeros).
#'
#' Non-standard codes whose numbers are already in metres (e.g.
#' `"10kmE4321000N3210000"`) would give coordinates far outside the EPSG:3035
#' extent; for these the numbers are used as metres, with a warning.
#'
#' @param cellCodes Character vector of EEA cell codes.
#'
#' @return A data frame with columns `cellCode`, `xcoord`, `ycoord` (metres)
#'   and `resolution` (e.g. `"10km"`).
#' @noRd
eea_code_to_coords <- function(cellCodes) {

  cellCode <- xcoord_base <- ycoord_base <- NULL
  xcoord <- ycoord <- resolution_final <- NULL
  resolution_value <- resolution_unit <- cell_size_m <- coord_multiplier <- NULL

  out <- data.frame(cellCode = cellCodes) %>%
    dplyr::mutate(
      # 1. Resolution value and unit (e.g., 10 and "km" from "10kmE432N321")
      resolution_value = as.numeric(stringr::str_extract(cellCode, "[0-9.]+")),
      resolution_unit = stringr::str_extract(cellCode, "(km|m)"),

      # 2. Cell size in metres
      cell_size_m = dplyr::case_when(
        resolution_unit == "km" ~ resolution_value * 1000,
        resolution_unit == "m" ~ resolution_value,
        TRUE ~ 1000 # Default to 1km if no unit found
      ),

      # 3. Multiplier: 10^(number of trailing zeros of the cell size in metres)
      coord_multiplier = 10^eea_trailing_zeros(cell_size_m),

      # 4. Easting and northing as given in the code (e.g., 432 from E432)
      xcoord_base = as.numeric(stringr::str_extract(
        cellCode,
        "(?<=[EW])-?\\d+"
      )),
      ycoord_base = as.numeric(stringr::str_extract(
        cellCode,
        "(?<=[NS])-?\\d+"
      )),

      # 5. Coordinates in metres
      xcoord = xcoord_base * coord_multiplier,
      ycoord = ycoord_base * coord_multiplier,

      # 6. Resolution string
      resolution_final = paste0(resolution_value, resolution_unit)
    )

  # Safety net for non-standard codes whose numbers are already in metres:
  # the EPSG:3035 grid does not extend beyond 10,000 km
  too_far <- !is.na(out$xcoord) & !is.na(out$ycoord) &
    (abs(out$xcoord) > 1e7 | abs(out$ycoord) > 1e7) &
    abs(out$xcoord_base) <= 1e7 & abs(out$ycoord_base) <= 1e7
  if (any(too_far)) {
    warning(sprintf(
      paste0("%d EEA cell code(s) (e.g. '%s') do not follow the EEA naming ",
             "rule; their numbers were taken to be in metres."),
      sum(too_far), out$cellCode[which(too_far)[1]]
    ), call. = FALSE)
    out$xcoord[too_far] <- out$xcoord_base[too_far]
    out$ycoord[too_far] <- out$ycoord_base[too_far]
  }

  out %>%
    dplyr::select(
      cellCode,
      xcoord,
      ycoord,
      resolution = resolution_final
    )
}

#' Number of trailing zeros of (whole) numbers
#'
#' @param x Numeric vector (e.g. cell sizes in metres).
#' @return Integer vector; 0 for NA or non-positive values.
#' @noRd
eea_trailing_zeros <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || v <= 0) return(0L)
    v <- round(v)
    n <- 0L
    while (v %% 10 == 0) {
      v <- v / 10
      n <- n + 1L
    }
    n
  }, integer(1))
}
