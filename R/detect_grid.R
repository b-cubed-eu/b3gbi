#' @noRd
detect_grid <- function(grid_code, stop_on_fail = FALSE) {

  grid_type <- ifelse(
    stringr::str_detect(
      grid_code,
      "^[0-9]{1,3}[km]{1,2}[EW]{1}[0-9]{2,7}[NS]{1}[0-9]{2,7}$"
    ),
    "eea",
    ifelse(
      stringr::str_detect(
        grid_code,
        "^[0-9]{2}[A-Z]{3}[0-9]{0,10}$"
      ),
      "mgrs",
      ifelse(
        stringr::str_detect(
          grid_code,
          "^[EW]{1}[0-9]{3}[NS]{1}[0-9]{2}[A-D]{0,6}$"
        ),
        "eqdgc",
        ifelse(
            stringr::str_detect(grid_code, "^[0-9]{15,}$"),
            "isea3h",
            NA
        )
      )
    )
  )

  if (stop_on_fail == TRUE && is.na(grid_type)) {

    stop("Could not detect grid type. Please specify manually.")

  }

  return(grid_type)

}
