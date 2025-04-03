#' @noRd
detect_grid_column <- function(df, grid_type) {
  for (col in colnames(df)) {
    valid_values <- df[[col]][!is.na(df[[col]])]
    if (length(valid_values) > 0) {
      grid_type_test <- switch(
        grid_type,
        "eea" = stringr::str_detect(
          valid_values,
          "^[0-9]{1,3}[km]{1,2}[EW]{1}[0-9]{2,7}[NS]{1}[0-9]{2,7}$"
        ),
        "mgrs" = stringr::str_detect(
          valid_values,
          "^[0-9]{2}[A-Z]{3}[0-9]{0,10}$"
        ),
        "eqdgc" = stringr::str_detect(
          valid_values,
          "^[EW]{1}[0-9]{3}[NS]{1}[0-9]{2}[A-D]{0,6}$"
        ),
        NA
      )

      if (all(grid_type_test)) {
        return(col)
      } else if (any(grid_type_test) && !all(grid_type_test)) {
        stop(
          paste0(
            "Column '", col, "' contains a mix of valid and invalid ",
            "grid cell codes. Please check the integrity of your data cube."
          )
        )
      }
    }
  }
  stop(
    paste0(
      "Could not detect specified grid type. Please specify column name ",
      "containing grid cell codes."
    )
  )
}
