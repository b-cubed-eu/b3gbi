#' @noRd
check_cell_size <- function(cell_size, cell_size_units, resolution, level) {
  if (!is.null(cell_size)) {
    if (cell_size_units == "km") {
      if (stringr::str_detect(resolution, "degrees")) {
        stop("cell_size_units must match resolution (degrees or km)")
      } else {
        res_size <- as.numeric(stringr::str_extract(resolution, "[0-9]*(?=km)"))

        if (cell_size %% res_size != 0) {
          stop(
            paste(
              "cell_size must be a whole number multiple of the resolution.",
              "For example, if resolution is 1 km, cell_size can be",
              "1, 2, 3, .. 10,.. 100, etc."
            )
          )
        }

        # convert to meters
        cell_size <- cell_size * 1000
      }
    } else if (cell_size_units == "degrees") {
      if (stringr::str_detect(resolution, "km")) {
        stop("cell_size_units must match resolution (degrees or km)")
      } else {
        res_size <- as.numeric(stringr::str_extract(resolution, "[0-9,.]*(?=degrees)"))

        if (cell_size %% res_size != 0) {
          stop(
            paste(
              "cell_size must be a whole number multiple of the resolution.",
              "For example, if resolution is 0.25 degrees, cell_size can be",
              "0.25, 0.5, 0.75, 1, etc."
            )
          )
        }
      }
    } else {
      stop(
        paste(
          "cell_size_units not a recognized unit type.",
          "Please check that you have entered it correctly."
        )
      )
    }
  } else {
    if (stringr::str_detect(resolution, "km")) {
      cell_size <- ifelse(level == "world", 100,
        ifelse(level == "continent", 100, 10)
      )

      # convert to meters
      cell_size <- cell_size * 1000
    } else if (stringr::str_detect(resolution, "degrees")) {
      res_size <- as.numeric(stringr::str_extract(
        resolution,
        "[0-9,.]*(?=degrees)"
      ))
      if (res_size < 1) {
        cont_res_size <- 1
      } else {
        cont_res_size <- res_size
      }
      cell_size <- ifelse(level == "world", 10,
        ifelse(level == "continent", cont_res_size, res_size)
      )
    }
  }

  return(cell_size)
}
