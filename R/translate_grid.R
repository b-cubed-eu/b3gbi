#' @noRd
convert_eqdgc_latlong <- function(cellCode) {

  long_base = as.numeric(stringr::str_extract(cellCode, "(?<=[EW])-?\\d+"))
  lat_base = as.numeric(stringr::str_extract(cellCode, "(?<=[NS])-?\\d+"))
  position_codes = stringr::str_replace_all(cellCode, "(E\\d+)|(N\\d+)|(W-\\d+)|(S-\\d+)", "")

  grid_level <- nchar(position_codes[1])
  ff <- c((0.25), (0.25/2), (0.25/4), (0.25/8), (0.25/16), (0.25/32))
  sign_long_m <- matrix("NA", nrow = length(long_base), ncol = grid_level)
  sign_lat_m <- matrix("NA", nrow = length(long_base), ncol = grid_level)
  for (i in 1:grid_level) {
    sign_long_m[,i] <- c(-1, 1, -1, 1, substr(position_codes, i, i))[match(substr(position_codes, i, i), c("A", "B", "C", "D"))]
    sign_lat_m[,i] <- c(1, 1, -1, -1, substr(position_codes, i, i))[match(substr(position_codes, i, i), c("A", "B", "C", "D"))]
  }
  sign_long <- apply(sign_long_m, 2, as.numeric)
  sign_lat <- apply(sign_lat_m, 2, as.numeric)
  ff_cut <- ff[1:grid_level]
  long_ext <- t(t(sign_long)*ff_cut)
  lat_ext <- t(t(sign_lat)*ff_cut)
  long <- rowSums(cbind(long_ext, long_base))
  lat <- rowSums(cbind(lat_ext, lat_base))
  return(cbind(lat, long))

}
