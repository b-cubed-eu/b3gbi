else if (grid_type == "eqdgc") {

  # Remove NA values in cell code
  occurrence_data <-
    occurrence_data %>%
    dplyr::filter(!is.na(eqdgccellcode))

  # Need to find W or E and take the 3 digits after it as eastwest (longitude), and find N or S and take the 2 digits after it as northsouth (latitude)
  occurrence_data <-
    occurrence_data %>%
    dplyr::mutate(eaqdgccellcode = stringr::str_replace(eqdgccellcode, "W", "W-")) %>%
    dplyr::mutate(eaqdgccellcode = stringr::str_replace(eqdgccellcode, "S", "S-"))

  # Separate cell code into resolution, coordinates
  occurrence_data <- occurrence_data %>%
    dplyr::mutate(
      xcoord = as.numeric(stringr::str_extract(eqdgccellcode, "(?<=[EW])-?\\d+")),
      ycoord = as.numeric(stringr::str_extract(eqdgccellcode, "(?<=[NS])-?\\d+")),
      resolution = stringr::str_replace_all(eqdgccellcode, "(E\\d+)|(N\\d+)|(W-\\d+)|(S-\\d+)", "")
    ) %>%
    dplyr::rename(cell_code = eqdgccellcode)

}


convert_eqdgc_latlong <- function(cell_codes) {

  base_coords <-
    cell_codes %>%
    dplyr::mutate(
      long_base = as.numeric(stringr::str_extract(eqdgccellcode, "(?<=[EW])-?\\d+")),
      lat_base = as.numeric(stringr::str_extract(eqdgccellcode, "(?<=[NS])-?\\d+")),
      position_codes = stringr::str_replace_all(eqdgccellcode, "(E\\d+)|(N\\d+)|(W-\\d+)|(S-\\d+)", "")
    )

  grid_level <- nchar(cell_codes$position_chodes)
  ff <- c((0.25), (0.25/2), (0.25/4), (0.25/8), (0.25/16), (0.25/32))
  sign_long_m <- matrix("NA", nrow = nrow(cell_codes$position_codes), ncol = grid_level)
  sign_lat_m <- matrix("NA", nrow = nrow(cell_codes$position_codes), ncol = grid_level)
  for (i in 1:grid_level) {
    sign_long_m[,i] <- c(-1, 1, -1, 1, cell_codes$position_codes[i])[match(cell_codes$position_codes[i], c("A", "B", "C", "D"))]
    sign_lat_m[,i] <- c(1, 1, -1, -1, cell_codes$position_codes[i])[match(cell_codes$position_codes[i], c("A", "B", "C", "D"))]
  }
  sign_long <-

  A = c(-1,1); B = c(1, 1); C = c(-1, -1); D = c(1, -1)


  lat_base

}
