
library(dplyr)
library(stringr)

eea_code_to_coords <- function(cellCodes) {
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
        TRUE ~ 1
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
      xcoord = dplyr::if_else(abs(xcoord_base) > 10000,
                             xcoord_base,
                             xcoord_base * km_multiplier * 1000),
      ycoord = dplyr::if_else(abs(ycoord_base) > 10000,
                             ycoord_base,
                             ycoord_base * km_multiplier * 1000),

      # 7. Create the final resolution string
      resolution_final = paste0(km_multiplier, "km")
    )
}

test_codes <- c(
  "1kmE4321N3210",
  "10kmE432N321",
  "100kmE43N32",
  "100mE43210N32100",
  "0.25kmE4321N3210",
  "250mE4321N3210",
  "100kmE43N32",
  "100kmE4321N3210",
  "E4321000N3210000"
)

results <- eea_code_to_coords(test_codes)
print(results %>% select(cellCode, xcoord, ycoord, km_multiplier, xcoord_base))
