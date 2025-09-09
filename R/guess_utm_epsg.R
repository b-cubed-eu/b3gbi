#' @title Guess Appropriate CRS
#' @description This function determines the most appropriate coordinate
#'  reference system (CRS) for a given area, following predefined rules.
#'
#'  It prioritizes inferring a specific UTM zone for localized data. If the
#'  data spans multiple UTM zones or falls outside of UTM boundaries, it
#'  defaults to a global, equal-area projection.
#'
#' @param x A data cube object (class 'processed_cube' or similar), expected to
#'  have a `$grid_type` element, a `$data` data frame with a `cellCode` column,
#'  and a `$coord_range` element. Alternatively, `x` can be an `sf::bbox`
#'  object representing the coordinate range in lat/long (EPSG:4326).
#'
#' @return A character string representing the appropriate EPSG or ESRI code
#'  (e.g., "EPSG:32631" or "ESRI:54012"). Returns `NULL` if a valid CRS cannot
#'  be determined or if the grid type is not 'mgrs'.
#'
#' @details This function implements a comprehensive logic for selecting a CRS:
#'  1. If the input is MGRS data, `cellCode` is used for a precise UTM zone.
#'  2. If the data's longitudinal span is > 6 degrees, it returns a global CRS.
#'  3. If the data's latitudinal span is > 15 degrees, it returns a global CRS.
#'  4. If the data extends into the polar regions (beyond 84°N or 80°S), it
#'     returns a global CRS.
#'  5. If the data meets all the criteria for a local projection, it returns
#'     the UTM CRS derived from the data's geographic center.
#'
#' @examples
#' # Example using a hypothetical data cube object that fits UTM criteria
#' data_cube_utm <- list(
#'   grid_type = "mgrs",
#'   data = data.frame(cellCode = "32U LV 85600 59900", value = 1),
#'   coord_range = sf::st_bbox(c(xmin = 10, ymin = 50, xmax = 11, ymax = 51),
#'                             crs = "EPSG:4326")
#' )
#' print(guess_utm_epsg(data_cube_utm))
#'
#' # Example using an sf::bbox object that is too large for UTM
#' large_bbox <- sf::st_bbox(c(xmin = 10, ymin = 40, xmax = 20, ymax = 60),
#'                           crs = "EPSG:4326")
#' print(guess_utm_epsg(large_bbox))
#'
#' @noRd
guess_utm_epsg <- function(x) {

  coord_range <- NULL

  if (inherits(x, "processed_cube")) {

    if (is.null(x$grid_type) || x$grid_type != "mgrs") {

      return(NULL)

    }

    if (!is.null(x$data$cellCode) && nrow(x$data) > 0) {

      # CHECK ALL UNIQUE MGRS CODES
      all_mgrs_codes <- as.character(x$data$cellCode)
      unique_utm_zones <- unique(substr(all_mgrs_codes, 1, 2))

      # If the data spans more than one UTM zone, return a global CRS
      if (length(unique_utm_zones) > 1) {

        warning("Data spans multiple UTM zones. Using a global equal-area CRS.")

        return("ESRI:54012")

      }

      # If there is only one UTM zone, proceed with the original logic
      first_mgrs <- all_mgrs_codes[1]
      utm_zone_str <- substr(first_mgrs, 1, 2)
      lat_band_char <- toupper(substr(first_mgrs, 3, 3))
      utm_zone <- as.numeric(utm_zone_str)

      if (!is.na(utm_zone) && utm_zone >= 1 && utm_zone <= 60) {

        hemisphere <- ifelse(lat_band_char %in% LETTERS[1:13],
                             "South", "North")

        if (hemisphere == "North") {

          return(paste0("EPSG:326", sprintf("%02d", utm_zone)))

        } else if (hemisphere == "South") {

          return(paste0("EPSG:327", sprintf("%02d", utm_zone)))

        }
      }
    }

    coord_range <- x$coord_range

  } else if (inherits(x, "bbox")) {

    coord_range <- x

  } else {

    warning("Input must be a 'processed_cube' object or an 'sf::bbox' object.")

    return(NULL)

  }

  if (is.null(coord_range) || length(coord_range) != 4) {

    warning("Could not automatically determine CRS from provided data.")

    return(NULL)

  }

  # Apply the checks for UTM suitability using the corrected logic
  long_span <- coord_range["xmax"] - coord_range["xmin"]
  lat_span <- coord_range["ymax"] - coord_range["ymin"]

  # Check if the data spans a single UTM zone
  single_utm_zone <-
    floor((coord_range["xmin"] + 180) / 6) ==
    floor((coord_range["xmax"] + 180) / 6)

  if (long_span > 6 ||
      lat_span > 15 ||
      coord_range["ymin"] < -80 ||
      coord_range["ymax"] > 84 ||
      !single_utm_zone) {

    return("ESRI:54012")

  }

  # If all checks pass, return the UTM CRS based on the center of the bbox
  lon_center <- mean(c(coord_range["xmin"], coord_range["xmax"]))
  lat_center <- mean(c(coord_range["ymin"], coord_range["ymax"]))
  utm_zone <- floor((lon_center + 180) / 6) + 1

  if (lat_center >= 0) {

    return(paste0("EPSG:326", sprintf("%02d", utm_zone)))

  } else {

    return(paste0("EPSG:327", sprintf("%02d", utm_zone)))

  }
}
