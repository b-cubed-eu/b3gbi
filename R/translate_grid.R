#' @noRd
convert_eqdgc_latlong <- function(cellCode) {
  # Extract base longitude and direction
  long_match <- stringr::str_match(cellCode, "([EW])(-?\\d+)")
  long_dir_char <- long_match[, 2]
  long_base <- as.numeric(long_match[, 3])
  long_dir <- ifelse(long_dir_char == "W", -1, 1)

  # Extract base latitude and direction
  lat_match <- stringr::str_match(cellCode, "([NS])(-?\\d+)")
  lat_dir_char <- lat_match[, 2]
  lat_base <- abs(as.numeric(lat_match[, 3]))
  lat_dir <- ifelse(lat_dir_char == "S", -1, 1)

  # Extract position codes
  position_codes <- stringr::str_replace_all(cellCode, "([EW]-?\\d+)|([NS]-?\\d+)", "")

  grid_level <- nchar(position_codes[1])
  ff <- c(0.25, 0.25/2, 0.25/4, 0.25/8, 0.25/16, 0.25/32)
  ff_cut <- ff[1:grid_level]

  long_ext <- numeric(length(long_base))
  lat_ext <- numeric(length(lat_base))

  for (i in 1:grid_level) {
    sub_codes <- substr(position_codes, i, i)
    long_signs <- ifelse(sub_codes %in% c("B", "D"), 1, -1)
    lat_signs <- ifelse(sub_codes %in% c("A", "B"), -1, 1)
    long_ext <- long_ext + long_signs * ff_cut[i]
    lat_ext <- lat_ext + lat_signs * ff_cut[i]
  }

  long <- long_dir * (long_base + 0.5 + long_ext)
  lat <- lat_dir * (lat_base + 0.5 + lat_ext)

  return(cbind(lat, long))
}

#' #' @noRd
#' convert_eqdgc_latlong <- function(cellCode) {
#'
#'   long_base = as.numeric(stringr::str_extract(cellCode, "(?<=[EW])-?\\d+"))
#'   lat_base = as.numeric(stringr::str_extract(cellCode, "(?<=[NS])-?\\d+"))
#'   long_dir = ifelse(stringr::str_extract(cellCode, "[EW]")=="W", -1, 1)
#'   lat_dir = ifelse(stringr::str_extract(cellCode, "[NS]")=="S", -1, 1)
#'   position_codes = stringr::str_replace_all(cellCode, "(E\\d+)|(N\\d+)|(W-\\d+)|(S-\\d+)", "")
#'   grid_level <- nchar(position_codes[1])
#'   ff <- c((0.25), (0.25/2), (0.25/4), (0.25/8), (0.25/16), (0.25/32))
#'   sign_long_m <- matrix("NA", nrow = length(long_base), ncol = grid_level)
#'   sign_lat_m <- matrix("NA", nrow = length(long_base), ncol = grid_level)
#'   for (i in 1:grid_level) {
#'     sign_long_m[,i] <- c(-1, 1, -1, 1, substr(position_codes, i, i))[match(substr(position_codes, i, i), c("A", "B", "C", "D"))]
#'     sign_lat_m[,i] <- c(1, 1, -1, -1, substr(position_codes, i, i))[match(substr(position_codes, i, i), c("A", "B", "C", "D"))]
#'   }
#'   sign_long <- apply(sign_long_m, 2, as.numeric)
#'   sign_lat <- apply(sign_lat_m, 2, as.numeric)
#'   ff_cut <- ff[1:grid_level]
#'   long_ext <- t(t(sign_long)*ff_cut)
#'   lat_ext <- t(t(sign_lat)*ff_cut)
#'   long <- rowSums(cbind(long_ext*long_dir, long_base+0.5))
#'   lat <- rowSums(cbind(lat_ext*lat_dir, lat_base+0.5))
#'   return(cbind(lat, long))
#'
#' }

#' @noRd
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


}
