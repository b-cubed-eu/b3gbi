#' Convert equal-area quarter-degree grid cell codes to latitude and longitude
#'
#' This is a helper function that takes a vector of EQDGC (Equal-Area Quarter-Degree
#' Grid Cell) codes and converts them to their central latitude and longitude
#' coordinates. The function parses the base coordinates and the nested sub-grid
#' codes to precisely locate the center of each grid cell.
#'
#' @param cellCode A character vector of EQDGC cell codes, which must follow
#'   the "direction-base_coord" format (e.g., "W10N34" or "E15S20"). The code
#'   can also contain additional letter characters for sub-grids (e.g., "W10N34A").
#'
#' @return A matrix with two columns, `lat` and `long`, representing the central
#'   coordinates of each input grid cell.
#'
#' @details The function works by first extracting the base longitude and
#'   latitude coordinates from the cell code, accounting for direction (East/West
#'   and North/South). It then iteratively processes any sub-grid codes (e.g.,
#'   `A`, `B`, `C`, `D`) to refine the coordinates. The final coordinates are
#'   the center point of the most specific grid cell.
#'
#' @examples
#' # A simple quarter-degree cell
#' convert_eqdgc_latlong("E10N10")
#'
#' # A sub-grid cell
#' convert_eqdgc_latlong("W-10S34ABCD")
#'
#' # Multiple cell codes
#' convert_eqdgc_latlong(c("E10N10", "W-10S34DDBA"))
#'
#' @export
convert_eqdgc_latlong <- function(cellCode) {

  # Extract base longitude and direction
  long_match <- stringr::str_match(cellCode, "([EW])(-?\\d+)")
  long_dir_char <- long_match[, 2]
  long_base <- abs(as.numeric(long_match[, 3]))
  long_dir <- ifelse(long_dir_char == "W", -1, 1)

  # Extract base latitude and direction
  lat_match <- stringr::str_match(cellCode, "([NS])(-?\\d+)")
  lat_dir_char <- lat_match[, 2]
  lat_base <- abs(as.numeric(lat_match[, 3]))
  lat_dir <- ifelse(lat_dir_char == "S", -1, 1)

  # Extract position codes
  position_codes <- stringr::str_replace_all(cellCode, "([EW]-?\\d+)|([NS]-?\\d+)", "")

  grid_level <- nchar(position_codes)
  ff <- c(0.5, 0.25, 0.25/2, 0.25/4, 0.25/8, 0.25/16, 0.25/32)

  long_ext <- numeric(length(long_base))
  lat_ext <- numeric(length(lat_base))

  for (i in 1:length(cellCode)) {

    # This conditional statement is the fix. It ensures the inner loop
    # only runs when there are sub-grid codes to process.
    if (grid_level[i] > 0) {
      sub_codes <- strsplit(position_codes[i], "")[[1]]
      ff_cut <- ff[1:grid_level[i]]

      for (j in 1:grid_level[i]) {
        code <- sub_codes[j]

        lat_add <- ifelse(lat_dir_char[i] == "S",
                          ifelse(code %in% c("C", "D"), ff_cut[j], 0),
                          ifelse(code %in% c("A", "B"), ff_cut[j], 0)
        )

        long_add <- ifelse(long_dir_char[i] == "E",
                           ifelse(code %in% c("B", "D"), ff_cut[j], 0),
                           ifelse(code %in% c("A", "C"), ff_cut[j], 0)
        )

        long_ext[i] <- long_ext[i] + long_add
        lat_ext[i] <- lat_ext[i] + lat_add
      }
    }
  }

  ff_final <- ff[grid_level + 1]

  long <- long_dir * (long_base + long_ext + ff_final)
  lat <- lat_dir * (lat_base + lat_ext + ff_final)

  return(cbind(lat, long))
}
