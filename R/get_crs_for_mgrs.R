#' Get the CRS for MGRS cell codes
#'
#' This function takes a vector of MGRS (Military Grid Reference System) cell codes
#' and determines the most likely UTM (Universal Transverse Mercator) coordinate
#' reference system (CRS) in EPSG format. It extracts the UTM zone and
#' determines the hemisphere (North or South) based on the letter codes.
#'
#' @param cellcodes A character vector of MGRS grid cell codes (e.g., "32UMA", "33TWE").
#'
#' @return A character string representing the most probable EPSG code for the given
#'   cell codes, for example, `"EPSG:32632"`.
#'
#' @examples
#' # Example with MGRS codes from a Northern Hemisphere zone (Zone 32)
#' mgrs_codes_north <- c("32TMT", "32TMR", "32TML")
#' get_crs_for_mgrs(mgrs_codes_north)
#'
#' # Example with MGRS codes from a Southern Hemisphere zone (Zone 14)
#' mgrs_codes_south <- c("14KMF", "14KMJ", "14KMG")
#' get_crs_for_mgrs(mgrs_codes_south)
#'
#' # The function can handle a mix of zones, choosing the median
#' mgrs_mixed_zones <- c("27TMT", "28TMR", "29TWE")
#' get_crs_for_mgrs(mgrs_mixed_zones)
#'
#' @export
get_crs_for_mgrs <- function(cellcodes) {

  utm_zone <- as.numeric(stringr::str_extract(cellcodes, "(^[0-9]*(?=[A-Z]{3}))"))
  dominant_utm <- median(utm_zone)
  first_letter_a_m <- stringr::str_extract(cellcodes, "(?<=[0-9]{2})[A-M]{1}")
  first_letter_n_z <- stringr::str_extract(cellcodes, "(?<=[0-9]{2})[N-Z]{1}")
  if (length(which(!is.na(first_letter_a_m))) > 0 & length(which(!is.na(first_letter_n_z)))>0) {
    if (length(which(!is.na(first_letter_n_z))) >= length(which(!is.na(first_letter_a_m)))) {
      utm_north <- TRUE
    } else {
      utm_north <- FALSE
    }
  } else if (length(which(!is.na(first_letter_a_m)))==0 & length(which(!is.na(first_letter_n_z)))>0) {
    utm_north <- TRUE
  } else if (length(which(!is.na(first_letter_a_m)))>0 & length(which(!is.na(first_letter_n_z)))==0) {
    utm_north <- FALSE
  } else {
    stop("Could not detect valid grid cell codes.")
  }

  if (utm_north == TRUE) {
    epsg_code <- paste0("EPSG: 326", dominant_utm)
  } else {
    epsg_code <- paste0("EPSG: 327", dominant_utm)
  }

  return(epsg_code)
}

