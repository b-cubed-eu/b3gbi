eea_code_to_coords <- function(cellCodes) {
  # Requires dplyr for data manipulation and stringr for string extraction
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("The 'dplyr' package is required.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("The 'stringr' package is required.")

  cellCode <- resolution_text <- xcoord_base <- ycoord_base <- NULL
  km_multiplier <- xcoord <- ycoord <- resolution_final <- NULL

  data.frame(cellCode = cellCodes) %>%
    dplyr::mutate(
      # 1. Extract the resolution part (e.g., "1km", "250m")
      resolution_text = stringr::str_replace_all(
        cellCode,
        "(E-?\\d+)|(N-?\\d+)|(W-?\\d+)|(S-?\\d+)",
        ""
      ),

      # 2. Extract the numerical resolution value (e.g., 1, 250)
      resolution_value = as.numeric(stringr::str_extract(resolution_text, "^\\d+")),

      # 3. Determine the multiplication factor in KILOMETERS (e.g., 1.0 or 0.25)
      km_multiplier = dplyr::case_when(
        stringr::str_detect(resolution_text, "km") ~ resolution_value,
        stringr::str_detect(resolution_text, "m")  ~ resolution_value / 1000,
        TRUE ~ NA_real_
      ),

      # 4. Extract x-coordinate base value (e.g., 420 from E420)
      xcoord_base = as.numeric(stringr::str_extract(
        cellCode,
        "(?<=[EW])-?\\d+"
      )),

      # 5. Extract y-coordinate base value (e.g., 420 from N420)
      ycoord_base = as.numeric(stringr::str_extract(
        cellCode,
        "(?<=[NS])-?\\d+"
      )),

      # 6. Calculate the final coordinates in METERS
      xcoord = xcoord_base * km_multiplier * 1000,
      ycoord = ycoord_base * km_multiplier * 1000,

      # 7. Create the final resolution string: e.g., 0.25 + "km" -> "0.25km"
      resolution_final = paste0(km_multiplier, "km")
    ) %>%
    # Select only the final columns needed
    dplyr::select(
      xcoord,
      ycoord,
      resolution = resolution_final
    )
}
