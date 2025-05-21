#' #' @noRd
#' convert_eqdgc_latlong <- function(cellCode) {
#'   # --- Error handling for invalid cellCode format ---
#'   if (!all(grepl("^[EW]-?\\d+[NS]-?\\d+[ABCD]*$", cellCode))) {
#'     stop("Invalid EQDGC cellCode format.")
#'   }
#'
#'   # Extract base longitude and direction
#'   long_match <- stringr::str_match(cellCode, "([EW])(-?\\d+)")
#'   long_dir_char <- long_match[, 2]
#'   long_base_val <- as.numeric(long_match[, 3])
#'   long_dir <- ifelse(long_dir_char == "W", -1, 1)
#'
#'   # Extract base latitude and direction
#'   lat_match <- stringr::str_match(cellCode, "([NS])(-?\\d+)")
#'   lat_dir_char <- lat_match[, 2]
#'   lat_base_val <- as.numeric(lat_match[, 3])
#'   lat_dir <- ifelse(lat_dir_char == "S", -1, 1)
#'
#'   # Extract position codes (subdivision details)
#'   position_codes <- stringr::str_replace_all(cellCode, "([EW]-?\\d+)|([NS]-?\\d+)", "")
#'
#'   # Ensure grid_level is calculated for each element if cellCode is a vector
#'   grid_levels <- nchar(position_codes)
#'
#'   # Define fractional factors. These are half the size of the square at each level.
#'   # ff_half_cell_size[k] = 0.5 / (2^k) where k is the number of subdivisions (grid_level)
#'   ff_half_cell_size <- 0.5 / (2^(1:max(grid_levels)))
#'
#'   # Initialize current_x and current_y as the *bottom-left* corner of the initial cell
#'   current_long_bl_initial <- long_base_val
#'   current_lat_bl_initial <- lat_base_val
#'
#'   # Initialize longitude and latitude for each cell code
#'   final_long <- numeric(length(cellCode))
#'   final_lat <- numeric(length(cellCode))
#'
#'   for (k in seq_along(cellCode)) { # Iterate through each cellCode
#'     pc <- position_codes[k]
#'     gl <- grid_levels[k]
#'
#'     # Start with the bottom-left of the initial 1-degree cell for this specific cellCode
#'     temp_long_bl <- current_long_bl_initial[k]
#'     temp_lat_bl <- current_lat_bl_initial[k]
#'
#'     # Determine hemisphere based on the *initial* latitude of the base cell.
#'     # We use lat_dir * lat_base_val to get the signed base latitude.
#'     is_southern_hemisphere <- (lat_dir[k] * lat_base_val[k]) < 0
#'
#'     # Loop through each subdivision level
#'     for (i in 1:gl) {
#'       sub_code <- substr(pc, i, i)
#'       # Half size of the cell *at this current level of subdivision*.
#'       current_half_cell_size <- ff_half_cell_size[i]
#'       full_cell_size <- current_half_cell_size * 2
#'
#'       # Determine quadrant and update temp_long_bl, temp_lat_bl
#'       # The logic for A, B, C, D changes based on the hemisphere
#'       if (!is_southern_hemisphere) {
#'         # Northern Hemisphere (original assumption: A=NW, B=NE, C=SW, D=SE)
#'         if (sub_code == "A") { # NW
#'           temp_lat_bl <- temp_lat_bl + full_cell_size
#'         } else if (sub_code == "B") { # NE
#'           temp_lat_bl <- temp_lat_bl + full_cell_size
#'           temp_long_bl <- temp_long_bl + full_cell_size
#'         } else if (sub_code == "C") { # SW (no shift)
#'           # No change to temp_long_bl, temp_lat_bl for SW
#'         } else if (sub_code == "D") { # SE
#'           temp_long_bl <- temp_long_bl + full_cell_size
#'         } else {
#'           stop(paste("Unknown subdivision code:", sub_code, "in cellCode", cellCode[k], "(Northern Hemisphere logic)"))
#'         }
#'       } else {
#'         # Southern Hemisphere (new assumption: A=SW, B=SE, C=NW, D=NE)
#'         # Note: This is an inversion of the Northern Hemisphere logic for A/C and B/D
#'         if (sub_code == "A") { # SW (no shift)
#'           # No change to temp_long_bl, temp_lat_bl for SW
#'         } else if (sub_code == "B") { # SE
#'           temp_long_bl <- temp_long_bl + full_cell_size
#'         } else if (sub_code == "C") { # NW
#'           temp_lat_bl <- temp_lat_bl + full_cell_size
#'         } else if (sub_code == "D") { # NE
#'           temp_lat_bl <- temp_lat_bl + full_cell_size
#'           temp_long_bl <- temp_long_bl + full_cell_size
#'         } else {
#'           stop(paste("Unknown subdivision code:", sub_code, "in cellCode", cellCode[k], "(Southern Hemisphere logic)"))
#'         }
#'       }
#'     }
#'     # After the loop, temp_long_bl and temp_lat_bl are the bottom-left of the smallest cell.
#'     # To get the center of this smallest cell, add half its width/height.
#'     final_cell_half_size <- ff_half_cell_size[gl] # Half size of the *final* cell
#'     final_long[k] <- temp_long_bl + final_cell_half_size
#'     final_lat[k] <- temp_lat_bl + final_cell_half_size
#'   }
#'
#'   # Apply overall direction to final coordinates (E/W, N/S from original code)
#'   long <- long_dir * final_long
#'   lat <- lat_dir * final_lat
#'
#'   return(cbind(lat, long))
#' }

#' @noRd
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

  grid_level <- nchar(position_codes[1])
  ff <- c(0.5, 0.25, 0.25/2, 0.25/4, 0.25/8, 0.25/16, 0.25/32)
  ff_cut <- ff[1:grid_level]
  ff_final <- ff[grid_level + 1]

  long_ext <- numeric(length(long_base))
  lat_ext <- numeric(length(lat_base))

  # if (lat_dir_char[1] == "S") {
  #   for (i in 1:grid_level) {
  #     sub_codes <- substr(position_codes, i, i)
  #     long_signs <- ifelse(sub_codes %in% c("B", "D"), -1, 1)
  #     lat_signs <- ifelse(sub_codes %in% c("A", "B"), 1, -1)
  #     long_ext <- long_ext + long_signs * ff_cut[i]
  #     lat_ext <- lat_ext + lat_signs * ff_cut[i]
  #   }
  # } else {
  #   for (i in 1:grid_level) {
  #     sub_codes <- substr(position_codes, i, i)
  #     long_signs <- ifelse(sub_codes %in% c("B", "D"), -1, 1)
  #     lat_signs <- ifelse(sub_codes %in% c("A", "B"), 1, -1)
  #     long_ext <- long_ext + long_signs * ff_cut[i]
  #     lat_ext <- lat_ext + lat_signs * ff_cut[i]
  #   }
  # }

  lat_add <- vector(length = length(cellCode))
  long_add <- vector(length = length(cellCode))
  for (i in 1:grid_level) {
    sub_codes <- substr(position_codes, i, i)
    for (j in 1:length(cellCode)) {
      lat_add[j] <- ifelse(lat_dir_char[j] == "S",
                        ifelse(sub_codes[j] %in% c("C", "D"), ff_cut[i], 0),
                        ifelse(sub_codes[j] %in% c("A", "B"), ff_cut[i], 0)
      )
      long_add[j] <- ifelse(long_dir_char[j] == "E",
                            ifelse(sub_codes[j] %in% c("B", "D"), ff_cut[i], 0),
                            ifelse(sub_codes[j] %in% c("A", "C"), ff_cut[i], 0)
      )
    }
    long_ext <- long_ext + long_add
    lat_ext <- lat_ext + lat_add
  }



  long <- long_dir * (long_base + long_ext + ff_final)
  lat <- lat_dir * (lat_base + lat_ext + ff_final)

  return(cbind(lat, long))
}


#' @noRd
get_crs_for_mgrs <- function(cellcodes) {

  utm_zone <- as.numeric(stringr::str_extract(cellcodes, "(^[0-9]*(?=[A-Z]{3}))"))
  dominant_utm <- median(utm_zone)
  first_letter_a_m <- stringr::str_extract(cellcodes, "(?<=[0-9]{2})[A-M]{1}")
  first_letter_n_z <- stringr::str_extract(cellcodes, "(?<=[0-9]{2})[N-Z]{1}")
  if (length(which(!is.na(first_letter_a_m))) > 0 & length(which(!is.na(first_letter_n_z)))>0) {
    if (length(which(!is.na(first_letter_n_z))) >= length(which(!is.na(first_letter_a_m)))) {
      utm_north <- TRUE
    } else {
      utm_north <- FALSE
    }
  } else if (length(which(!is.na(first_letter_a_m)))==0 & length(which(!is.na(first_letter_n_z)))>0) {
    utm_north <- TRUE
  } else if (length(which(!is.na(first_letter_a_m)))>0 & length(which(!is.na(first_letter_n_z)))==0) {
    utm_north <- FALSE
  } else {
    stop("Could not detect valid grid cell codes.")
  }

  if (utm_north == TRUE) {
    epsg_code <- paste0("EPSG: 326", dominant_utm)
  } else {
    epsg_code <- paste0("EPSG: 327", dominant_utm)
  }
}



#' @noRd
#' @title Guess EPSG Code for MGRS Grid
#'
#' @description This function attempts to guess the appropriate EPSG code for a
#'   biodiversity cube known to use the MGRS grid system.
#'
#' @param data A data cube object (class 'processed_cube' or similar). It is
#'   expected to have a `$grid_type` element.
#' @param df A data frame containing the data, expected to have a `cellCode`
#'   column for MGRS references and potentially `xcoord` and `ycoord`.
#' @param coord_range A numeric vector of length 4 representing the coordinate
#'   range (xmin, ymin, xmax, ymax) in longitude and latitude, if available
#'   in `data$coord_range`.
#'
#' @return A character string representing the likely EPSG code for the MGRS grid.
#'   Returns NULL if the grid type is not 'mgrs' or if insufficient information
#'   is available.
#'
#' @examples
#' # Assuming you have a 'data' object and a 'df' data frame
#' # guessed_crs <- guess_mgrs_epsg(data, df, data$coord_range)
#' # print(guessed_crs)
#'
guess_mgrs_epsg <- function(data, df, coord_range = NULL) {
  if (is.null(data$grid_type) || data$grid_type != "mgrs") {
    return(NULL)
  }

  # 1. Try to infer from a sample MGRS cellCode
  if (!is.null(df$cellCode) && nrow(df) > 0) {
    first_mgrs <- as.character(df$cellCode[1])
    if (nchar(first_mgrs) >= 5) {
      utm_zone_str <- substr(first_mgrs, 1, 2)
      lat_band_char <- toupper(substr(first_mgrs, 3, 3))

      # Check if the first two characters are a valid UTM zone
      utm_zone <- as.numeric(utm_zone_str)
      if (!is.na(utm_zone) && utm_zone >= 1 && utm_zone <= 60) {
        hemisphere <- ifelse(lat_band_char %in% LETTERS[1:13], "South", "North")
        if (hemisphere == "North") {
          return(paste0("EPSG:326", sprintf("%02d", utm_zone)))
        } else if (hemisphere == "South") {
          return(paste0("EPSG:327", sprintf("%02d", utm_zone)))
        }
      }
    }
  }

  # 2. If no MGRS code is available or parsing failed, try using coord_range
  if (!is.null(coord_range) && length(coord_range) == 4) {
    lon_center <- mean(c(coord_range[1], coord_range[3]))
    lat_center <- mean(c(coord_range[2], coord_range[4]))

    utm_zone <- floor((lon_center + 180) / 6) + 1
    if (lat_center >= 0) {
      return(paste0("EPSG:326", sprintf("%02d", utm_zone)))
    } else {
      return(paste0("EPSG:327", sprintf("%02d", utm_zone)))
    }
  }

  # 3. If no coord_range, and no usable MGRS code, return a generic default
  # (WGS 84 UTM Zone 1 North) as a fallback - user MUST verify
  warning(
    paste(
      "Could not automatically determine MGRS zone. Defaulting to EPSG:32601 ",
      "(WGS 84 / UTM zone 1N). Please verify this is correct and override if ",
      "necessary."
    )
  )
  return("EPSG:32601")
}
