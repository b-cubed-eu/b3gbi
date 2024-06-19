#' @noRd
detect_grid_column <- function(df, grid_type){

  for (col in colnames(df)) {

    temp_col <- df[[col]][!is.na(df[[col]])][1]
    grid_type_test <- ifelse(grid_type == "eea", stringr::str_detect(temp_col, "^[0-9]{1,3}[km]{1,2}[EW]{1}[0-9]{2,7}[NS]{1}[0-9]{2,7}$"),
                        ifelse(grid_type == "mgrs", stringr::str_detect(temp_col, "^[0-9]{2}[A-Z]{3}[0-9]{0,10}$"),
                               ifelse(grid_type == "eqdgc", stringr::str_detect(temp_col, "^[EW]{1}[0-9]{3}[NS]{1}[0-9]{2}[A-D]{0,6}$"),
                                      NA)))

    if (grid_type_test==TRUE) {

      return(temp_col)

    }

  }

  stop("Could not detect specified grid type. Please specify column name containing grid cell codes.")

}
