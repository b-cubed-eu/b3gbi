#' #' Adjusts cell size to be compatible with input resolution, output units, and level.
#' #'
#' #' @param resolution The resolution of the input data (e.g., "10km", "0.1degrees").
#' #' @param cell_size The desired cell size.
#' #' @param input_units The units of the input resolution ("km" or "degrees").
#' #' @param output_units The units of the desired cell size ("km" or "degrees").
#' #'                    Note: When "km" is specified, the result is actually returned in meters.
#' #' @param level The geographical level ("world", "continent", or other).
#' #' @param data Optional data frame containing 'xcoord' and 'ycoord' columns for determining reference latitude.
#' #' @param reference_latitude Optional manual reference latitude. Used only if data is not provided.
#' #' @param input_crs The CRS of the input data (EPSG code or proj4string).
#' #' @param output_crs The CRS of the output data (EPSG code or proj4string).
#' #'
#' #' @return A compatible cell size (in meters when output_units="km", or in degrees when output_units="degrees").
#' #'
#' #' @examples
#' #' # With data frame
#' #' df <- data.frame(xcoord = c(10.1, 10.2, 10.3), ycoord = c(55.1, 55.2, 55.3))
#' #' check_cell_size("10km", 15, "km", "degrees", "country", data = df)
#' #'
#' #' # Without data frame
#' #' check_cell_size("0.1degrees", 15, "degrees", "km", "continent")  # Returns result in meters
#' #' @noRd
#' check_cell_size <- function(resolution, cell_size, input_units, output_units, level,
#'                             data = NULL, reference_latitude = 0,
#'                             input_crs = ifelse(input_units == "degrees", 4326, 3857),
#'                             output_crs = ifelse(output_units == "degrees", 4326, 3857)) {
#'
#'   # Input validation
#'   if (!requireNamespace("sf", quietly = TRUE)) {
#'     stop("Package 'sf' is required for accurate coordinate transformations.")
#'   }
#'
#'   # Extract resolution value
#'   res_value <- as.numeric(stringr::str_extract(resolution, "^[0-9,.]{1,6}(?=[a-z])"))
#'   if (is.na(res_value)) {
#'     stop(paste("Could not extract numerical value from resolution:", resolution))
#'   }
#'
#'   # Convert resolution to meters if in km
#'   if (input_units == "km") {
#'     res_value_meters <- res_value * 1000
#'   }
#'
#'   # Determine reference latitude from data if provided
#'   if (!is.null(data)) {
#'     if (!all(c("xcoord", "ycoord") %in% colnames(data))) {
#'       stop("Data frame must contain 'xcoord' and 'ycoord' columns.")
#'     }
#'
#'     # Calculate median latitude (more robust than mean)
#'     reference_latitude <- stats::median(data$ycoord, na.rm = TRUE)
#'
#'     # Determine the extent of the data for scale-appropriate adjustments
#'     lat_range <- max(data$ycoord, na.rm = TRUE) - min(data$ycoord, na.rm = TRUE)
#'     lon_range <- max(data$xcoord, na.rm = TRUE) - min(data$xcoord, na.rm = TRUE)
#'
#'     # Determine data extent (small, medium, large)
#'     data_extent <- "small"  # Default to small
#'     if (lat_range > 20 || lon_range > 20) {
#'       data_extent <- "large"  # Continental scale
#'     } else if (lat_range > 5 || lon_range > 5) {
#'       data_extent <- "medium"  # Country scale
#'     }
#'
#'     # Log the automatically determined reference point
#'     message(paste("Using reference latitude:", round(reference_latitude, 2),
#'                   "degrees (based on data with", data_extent, "extent)"))
#'   }
#'
#'   # Set default cell size based on level, data extent, and output units
#'   if (is.null(cell_size)) {
#'     # Determine data extent if we have data but haven't set it yet
#'     if (!is.null(data) && !exists("data_extent")) {
#'       lat_range <- max(data$ycoord, na.rm = TRUE) - min(data$ycoord, na.rm = TRUE)
#'       lon_range <- max(data$xcoord, na.rm = TRUE) - min(data$xcoord, na.rm = TRUE)
#'
#'       data_extent <- "small"  # Default to small
#'       if (lat_range > 20 || lon_range > 20) {
#'         data_extent <- "large"  # Continental scale
#'       } else if (lat_range > 5 || lon_range > 5) {
#'         data_extent <- "medium"  # Country scale
#'       }
#'     } else if (!exists("data_extent")) {
#'       # If no data and no data_extent, use level to determine size
#'       data_extent <- if (level %in% c("world", "continent")) "large" else "small"
#'     }
#'
#'     # Set default cell size based on data extent and output units
#'     if (data_extent == "large") {
#'       if (output_units == "km") {
#'         cell_size <- 100000  # 100km in meters
#'       } else {
#'         cell_size <- 1  # ~111km at equator
#'       }
#'     } else if (data_extent == "medium") {
#'       if (output_units == "km") {
#'         cell_size <- 25000  # 25km in meters
#'       } else {
#'         cell_size <- 0.25  # ~28km at equator
#'       }
#'     } else {  # small
#'       if (output_units == "km") {
#'         cell_size <- 5000  # 5km in meters
#'       } else {
#'         cell_size <- 0.05  # ~5.5km at equator
#'       }
#'     }
#'
#'     # Report in km for readability but store in meters
#'     report_cell_size <- if (output_units == "km") cell_size / 1000 else cell_size
#'     report_units <- if (output_units == "km") "km" else "degrees"
#'
#'     message(paste("Default cell size set to", report_cell_size, report_units,
#'                   "based on", data_extent, "data extent"))
#'   } else if (output_units == "km") {
#'     # If cell_size was provided in km, convert to meters
#'     cell_size <- cell_size * 1000
#'   }
#'
#'   # For large datasets, we'll sample multiple points for more accurate adjustment
#'   if (exists("data_extent") && data_extent == "large") {
#'     # Sample reference latitudes from the data range
#'     sample_lats <- if (!is.null(data)) {
#'       # Get quantiles of the latitudes
#'       stats::quantile(data$ycoord, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
#'     } else {
#'       c(reference_latitude - 10, reference_latitude, reference_latitude + 10)
#'     }
#'
#'     # Calculate conversion factor at each sampled latitude
#'     conversion_factors <- numeric(length(sample_lats))
#'
#'     for (i in seq_along(sample_lats)) {
#'       lat <- sample_lats[i]
#'
#'       # Create a point with known distance in input units
#'       if (input_units == "km") {
#'         # Create a point and a point that's res_value km east
#'         pt1 <- sf::st_point(c(0, lat))
#'         pt2 <- sf::st_point(c(res_value, lat))
#'
#'         # Convert to sf objects with correct CRS
#'         pt1_sf <- sf::st_sfc(pt1, crs = input_crs)
#'         pt2_sf <- sf::st_sfc(pt2, crs = input_crs)
#'
#'         # Transform to output CRS
#'         pt1_transformed <- sf::st_transform(pt1_sf, output_crs)
#'         pt2_transformed <- sf::st_transform(pt2_sf, output_crs)
#'
#'         # Calculate distance in output units
#'         if (output_units == "degrees") {
#'           # For degrees, we need the longitudinal difference
#'           converted_res <- abs(sf::st_coordinates(pt2_transformed)[1] -
#'                                  sf::st_coordinates(pt1_transformed)[1])
#'         } else {
#'           # For km, we use the actual distance in meters
#'           converted_res <- sf::st_distance(pt1_transformed, pt2_transformed)
#'         }
#'       } else { # input_units == "degrees"
#'         # Create a point and a point that's res_value degrees east
#'         pt1 <- sf::st_point(c(0, lat))
#'         pt2 <- sf::st_point(c(res_value, lat))
#'
#'         # Convert to sf objects with correct CRS
#'         pt1_sf <- sf::st_sfc(pt1, crs = input_crs)
#'         pt2_sf <- sf::st_sfc(pt2, crs = input_crs)
#'
#'         # Transform to output CRS
#'         pt1_transformed <- sf::st_transform(pt1_sf, output_crs)
#'         pt2_transformed <- sf::st_transform(pt2_sf, output_crs)
#'
#'         # Calculate distance in output units
#'         if (output_units == "km") {
#'           converted_res <- as.numeric(sf::st_distance(pt1_transformed, pt2_transformed))
#'         } else {
#'           converted_res <- abs(sf::st_coordinates(pt2_transformed)[1] -
#'                                  sf::st_coordinates(pt1_transformed)[1])
#'         }
#'       }
#'
#'       conversion_factors[i] <- as.numeric(converted_res) / res_value
#'     }
#'
#'     # Use median conversion factor to accommodate range of values
#'     median_factor <- stats::median(conversion_factors)
#'     converted_res_value <- res_value * median_factor
#'
#'     # If input is in km and output is in km, we need meters
#'     if (input_units == "km" && output_units == "km") {
#'       converted_res_value <- converted_res_value * 1000
#'     }
#'
#'     # Log the conversion factors for transparency
#'     # Report factors in terms of user-facing units
#'     report_factors <- conversion_factors
#'     if (output_units == "km" && input_units == "degrees") {
#'       report_factors <- report_factors / 1000  # Convert meters to km for reporting
#'     }
#'
#'     message(paste("Conversion factors at different latitudes:",
#'                   paste(round(report_factors, 4), collapse = ", ")))
#'
#'     report_factor <- median_factor
#'     if (output_units == "km" && input_units == "degrees") {
#'       report_factor <- report_factor / 1000  # Convert meters to km for reporting
#'     }
#'     message(paste("Using median conversion factor:", round(report_factor, 4)))
#'
#'   } else {
#'     # For small to medium datasets, use the reference latitude
#'     if (input_units != output_units) {
#'       # Create a point with known distance in input units
#'       if (input_units == "km") {
#'         # Create a point and a point that's res_value km east
#'         pt1 <- sf::st_point(c(0, reference_latitude))
#'         pt2 <- sf::st_point(c(res_value, reference_latitude))
#'
#'         # Convert to sf objects with correct CRS
#'         pt1_sf <- sf::st_sfc(pt1, crs = input_crs)
#'         pt2_sf <- sf::st_sfc(pt2, crs = input_crs)
#'
#'         # Transform to output CRS
#'         pt1_transformed <- sf::st_transform(pt1_sf, output_crs)
#'         pt2_transformed <- sf::st_transform(pt2_sf, output_crs)
#'
#'         # Calculate distance in output units
#'         if (output_units == "degrees") {
#'           # For degrees, we need the longitudinal difference
#'           converted_res_value <- abs(sf::st_coordinates(pt2_transformed)[1] -
#'                                        sf::st_coordinates(pt1_transformed)[1])
#'         } else {
#'           # For km, we use the actual distance in meters
#'           converted_res_value <- sf::st_distance(pt1_transformed, pt2_transformed)
#'         }
#'       } else { # input_units == "degrees"
#'         # Create a point and a point that's res_value degrees east
#'         pt1 <- sf::st_point(c(0, reference_latitude))
#'         pt2 <- sf::st_point(c(res_value, reference_latitude))
#'
#'         # Convert to sf objects with correct CRS
#'         pt1_sf <- sf::st_sfc(pt1, crs = input_crs)
#'         pt2_sf <- sf::st_sfc(pt2, crs = input_crs)
#'
#'         # Transform to output CRS
#'         pt1_transformed <- sf::st_transform(pt1_sf, output_crs)
#'         pt2_transformed <- sf::st_transform(pt2_sf, output_crs)
#'
#'         # Calculate distance in output units
#'         if (output_units == "km") {
#'           converted_res_value <- as.numeric(sf::st_distance(pt1_transformed, pt2_transformed))
#'         } else {
#'           converted_res_value <- abs(sf::st_coordinates(pt2_transformed)[1] -
#'                                        sf::st_coordinates(pt1_transformed)[1])
#'         }
#'       }
#'
#'       # Extract the numeric value from the possible units
#'       converted_res_value <- as.numeric(converted_res_value)
#'     } else {
#'       converted_res_value <- res_value
#'       # If input and output are both km, convert to meters
#'       if (input_units == "km" && output_units == "km") {
#'         converted_res_value <- converted_res_value * 1000
#'       }
#'     }
#'   }
#'
#'   # Check if cell_size is smaller than the converted resolution
#'   if (cell_size < converted_res_value) {
#'     # Report in km for readability
#'     report_cell_size <- if (output_units == "km") cell_size / 1000 else cell_size
#'     report_converted_res <- if (output_units == "km") converted_res_value / 1000 else converted_res_value
#'     report_units <- if (output_units == "km") "km" else "degrees"
#'
#'     warning(paste0("Cell size (", report_cell_size, " ", report_units,
#'                    ") is smaller than the input resolution (",
#'                    res_value, " ", input_units, ", which is approximately ",
#'                    round(report_converted_res, 4), " ", report_units,
#'                    "). Adjusting to match the input resolution."))
#'     cell_size <- converted_res_value
#'   }
#'
#'   # Ensure cell_size is a multiple of the converted resolution
#'   # This ensures grid alignment
#'   remainder <- cell_size %% converted_res_value
#'   if (remainder > 1e-10) {
#'     # Round to nearest multiple of converted_res_value
#'     cell_size <- ceiling(cell_size / converted_res_value) * converted_res_value
#'
#'     # Report in km for readability
#'     report_cell_size <- if (output_units == "km") cell_size / 1000 else cell_size
#'     report_units <- if (output_units == "km") "km" else "degrees"
#'
#'     message(paste0("Cell size adjusted to ", report_cell_size, " ", report_units,
#'                    " to ensure compatibility with input resolution."))
#'   }
#'
#'   # Round to reasonable precision based on units
#'   if (output_units == "degrees") {
#'     cell_size <- round(cell_size, 6)  # 6 decimal places for degrees
#'   } else {
#'     cell_size <- round(cell_size, 2)  # 2 decimal places for km
#'   }
#'
#'   return(cell_size)
#' }




#' #' Adjusts cell size to be compatible with input resolution, output units, and level.
#' #'
#' #' @param resolution The resolution of the input data (e.g., "10km", "0.1degrees").
#' #' @param cell_size The desired cell size.
#' #' @param input_units The units of the input resolution ("km" or "degrees").
#' #' @param output_units The units of the desired cell size ("km" or "degrees").
#' #' @param level The geographical level ("world", "continent", or other).
#' #'
#' #' @return A compatible cell size, or throws an error if no compatible size is found.
#' #'
#' #' @examples
#' #' tryCatch({
#' #'   print(check_cell_size("10km", 15, "km", "km", "world")) # Returns adjusted cell size
#' #'   print(check_cell_size("0.1degrees", 0.15, "degrees", "degrees", "continent")) # Returns adjusted cell size
#' #'   print(check_cell_size("10km", 15, "km", "degrees", "country")) # Returns adjusted cell size
#' #' }, error = function(e) {
#' #'   print(e$message)
#' #' })
#' #' @noRd
#' check_cell_size <- function(resolution, cell_size, input_units, output_units, level) {
#'   # Extract resolution value and units
#'   res_value <- as.numeric(stringr::str_extract(resolution, "^[0-9,.]{1,6}(?=[a-z])"))
#'
#'   # Set default cell size based on level
#'   if (is.null(cell_size)){
#'     if (level %in% c("world", "continent")) {
#'       if (output_units == "km"){
#'         cell_size <- 100000
#'       } else {
#'         cell_size <- 1 # Default for world/continent in degrees
#'       }
#'     } else {
#'       if (output_units == "km"){
#'         cell_size <- 10000
#'       } else {
#'         cell_size <- 0.25 # Default for other levels in degrees
#'       }
#'     }
#'   }
#'
#'   # Check if units are the same
#'   if (input_units == output_units) {
#'     # Adjust cell size to be a multiple of resolution
#'     if (cell_size %% res_value != 0) {
#'       cell_size <- ceiling(cell_size / res_value) * res_value
#'     }
#'   } else {
#'     # Approximate conversion factors (very rough)
#'     m_to_degrees <- 1 / 111000 # Roughly 111km per degree
#'     degrees_to_m <- 111000
#'
#'     # Compatibility check with approximate conversion
#'     if (input_units == "km" && output_units == "degrees") {
#'       converted_res <- res_value * m_to_degrees
#'       if (abs(cell_size - converted_res) > converted_res / 2) {
#'         cell_size <- round(converted_res, digits = 2)
#'       }
#'     } else if (input_units == "degrees" && output_units == "km") {
#'       converted_res <- res_value * degrees_to_m
#'       if (abs(cell_size - converted_res) > converted_res / 2) {
#'         cell_size <- round(converted_res)
#'       }
#'     } else {
#'       stop("Error: Unit conversion not supported or invalid combination of units.")
#'     }
#'   }
#'
#'   return(cell_size) # Return the adjusted cell size
#' }
#'


#' #' @noRd
#' check_cell_size <- function(cell_size, cell_size_units, resolution, level) {
#'
#'   if (!is.null(cell_size)) {
#'
#'     if (cell_size_units=="km") {
#'
#'       if (stringr::str_detect(resolution, "degrees")) {
#'
#'         stop("cell_size_units must match resolution (degrees or km)")
#'
#'       } else {
#'
#'         res_size <- as.numeric(stringr::str_extract(resolution, "[0-9]*(?=km)"))
#'
#'         if (cell_size %% res_size != 0) {
#'
#'           stop("cell_size must be a whole number multiple of the resolution.
#'                For example, if resolution is 1 km, cell_size can be 1, 2, 3,.. 10,.. 100, etc.")
#'
#'         }
#'
#'         # convert to meters
#'         cell_size <- cell_size * 1000
#'
#'       }
#'
#'     } else if (cell_size_units=="degrees") {
#'
#'       if (stringr::str_detect(resolution, "km")) {
#'
#'         stop("cell_size_units must match resolution (degrees or km)")
#'
#'       } else {
#'
#'         res_size <- as.numeric(stringr::str_extract(resolution, "[0-9,.]*(?=degrees)"))
#'
#'         if (cell_size %% res_size != 0) {
#'
#'           stop("cell_size must be a whole number multiple of the resolution.
#'                For example, if resolution is 0.25 degrees, cell_size can be 0.25, 0.5, 0.75, 1, etc.")
#'
#'         }
#'
#'       }
#'
#'     } else {
#'
#'       stop("cell_size_units not a recognized unit type. Please check that you have entered it correctly.")
#'
#'     }
#'
#'   } else {
#'
#'     if (stringr::str_detect(resolution, "km")) {
#'
#'       cell_size <- ifelse(level == "world", 100,
#'                           ifelse(level == "continent", 100, 10))
#'
#'       # convert to meters
#'       cell_size <- cell_size * 1000
#'
#'     } else if (stringr::str_detect(resolution, "degrees")) {
#'
#'       res_size <- as.numeric(stringr::str_extract(resolution, "[0-9,.]*(?=degrees)"))
#'       if (res_size < 1) {cont_res_size <- 1} else {cont_res_size <- res_size}
#'       cell_size <- ifelse(level == "world", 10,
#'                           ifelse(level == "continent", cont_res_size, res_size))
#'
#'     }
#'
#'   }
#'
#'   return(cell_size)
#'
#' }


#' @noRd
check_cell_size <- function(cell_size, resolution, level) {
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

  if (!is.null(cell_size)) {
    if (stringr::str_detect(resolution, "km")) {
      res_size <- as.numeric(stringr::str_extract(resolution, "[0-9]*(?=km)"))
      if (cell_size %% res_size != 0) {
        stop("cell_size must be a whole number multiple of the resolution.
             For example, if resolution is 1 km, cell_size can be 1, 2, 3,.. 10,.. 100, etc.")
      }
      # convert to meters
      cell_size <- cell_size * 1000
    } else if (stringr::str_detect(resolution, "degrees")) {
      res_size <- as.numeric(stringr::str_extract(resolution, "[0-9,.]*(?=degrees)"))
      if (cell_size %% res_size != 0) {
        stop("cell_size must be a whole number multiple of the resolution.
             For example, if resolution is 0.25 degrees, cell_size can be 0.25, 0.5, 0.75, 1, etc.")
      }
    } else {
      stop("Resolution units not recognized. Please check that you have entered it correctly (km or degrees).")
    }
  } else {
    if (stringr::str_detect(resolution, "km")) {
      cell_size <- ifelse(level == "world", 100,
                          ifelse(level == "continent", 100, 10))
      # convert to meters
      cell_size <- cell_size * 1000
    } else if (stringr::str_detect(resolution, "degrees")) {
      res_size <- as.numeric(stringr::str_extract(resolution, "[0-9,.]*(?=degrees)"))
      if (res_size < 1) {
        cont_res_size <- 1
      } else {
        cont_res_size <- res_size
      }
      cell_size <- ifelse(level == "world", 1,
                          ifelse(level == "continent", cont_res_size, 0.25))
    }
  }
  return(cell_size)
}
