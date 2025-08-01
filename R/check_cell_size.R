#' Checks and adjusts cell size based on resolution and level.
#'
#' @param cell_size The desired cell size (numeric or NULL).
#' @param resolution The resolution of the data (e.g., "10km", "0.25degrees").
#' @param level The geographical level ("world", "continent", or other).
#'
#' @return A compatible cell size in meters (if resolution is in km) or degrees.
#'
#' @examples
#' check_cell_size(10, "1km", "country")
#' check_cell_size(NULL, "0.25degrees", "world")
#'
#' @noRd
#'
check_cell_size <- function(cell_size, resolution, level, area = NULL) {

  if (!is.null(cell_size)) {
    if (stringr::str_detect(resolution, "km")) {
      res_size <- as.numeric(stringr::str_extract(resolution, "[0-9.]+(?=km)"))
      if (!isTRUE(all.equal(cell_size/res_size, round(cell_size/res_size)))) {
        stop("cell_size must be a whole number multiple of the resolution.
             For example, if resolution is 1 km, cell_size can be 1, 2, 3,.. 10,.. 100, etc.")
      }
      # convert to meters
      cell_size <- cell_size * 1000
    } else if (stringr::str_detect(resolution, "degrees")) {
      res_size <- as.numeric(stringr::str_extract(resolution, "[0-9.]+(?=degrees)"))
      if (!isTRUE(all.equal(cell_size/res_size, round(cell_size/res_size)))) {
        stop("cell_size must be a whole number multiple of the resolution.
             For example, if resolution is 0.25 degrees, cell_size can be 0.25, 0.5, 0.75, 1, etc.")
      }
    } else {
      stop("Resolution units not recognized. Please check that you have entered it correctly (km or degrees).")
    }
  } else {
    if (stringr::str_detect(resolution, "km")) {
      if (level == "cube") {
        if (!is.null(area)) {
          cell_size <- ifelse(as.numeric(area) >= 1000, 1, 0.1)
          # convert to meters
          cell_size <- cell_size * 1000
        } else {
          stop("Unable to determine area of cube for automated cell size determination. Please enter cell size manually.")
        }
      } else {
        cell_size <- ifelse(level == "world", 100,
                            ifelse(level == "continent", 100, 10))
        # convert to meters
        cell_size <- cell_size * 1000
      }
    } else if (stringr::str_detect(resolution, "degrees")) {
      res_size <- as.numeric(stringr::str_extract(resolution, "[0-9.]+(?=degrees)"))
      if (res_size < 1) {
        cont_res_size <- 1
      } else {
        cont_res_size <- res_size
      }
      # if (res_size < 0.125) {
      #   res_size <- 0.125
      # }
      cell_size <- ifelse((level == "world" || level == "continent"),
                          cont_res_size, res_size)
    }
  }
  return(cell_size)
}
