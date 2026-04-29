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

      # 2. Determine resolution in km (numeric)
      resolution_value = as.numeric(stringr::str_extract(cellCode, "[0-9.]+")),
      # Extract resolution unit (km or m)
      resolution_unit = stringr::str_extract(cellCode, "(km|m)"),

      # Calculate multiplier to get meters per grid unit
      # (e.g., for 100km, the value is 100,000 meters)
      km_multiplier = dplyr::case_when(
        resolution_unit == "km" ~ resolution_value * 1000,
        resolution_unit == "m" ~ resolution_value,
        TRUE ~ 1000 # Default to 1km if no unit found
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

      # 6. Final coordinates in meters
      # Heuristic: If base coordinates are > 10,000, they are likely already in meters.
      # Standard EEA IDs use km-scale integers (e.g. E4321 for 4,321,000m).
      xcoord = dplyr::if_else(abs(xcoord_base) > 10000,
                               xcoord_base,
                               xcoord_base * km_multiplier),
      ycoord = dplyr::if_else(abs(ycoord_base) > 10000,
                               ycoord_base,
                               ycoord_base * km_multiplier),

      # 7. Create the final resolution string
      resolution_final = paste0(resolution_value, resolution_unit)
    ) %>%
    # Select only the final columns needed
    dplyr::select(
      cellCode,
      xcoord,
      ycoord,
      resolution = resolution_final
    )
}
