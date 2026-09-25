#' @title Create a Longitude/Latitude Bounding Box from MGRS Data
#' @description Converts UTM coordinates of MGRS cells, which may span several
#'   UTM zones, to WGS 84 and returns their combined bounding box.
#'
#' @param df A data frame with at least three columns: `cellCode` (MGRS codes;
#'   the first three characters give the UTM zone and latitude band), and
#'   `xcoord`/`ycoord` (UTM easting and northing).
#'
#' @return An `sf::st_bbox()` object in EPSG:4326 (longitude/latitude).
#'
#' @examples
#' \donttest{
#' df <- data.frame(
#'   cellCode = c("32UUC", "32UUD"),
#'   xcoord = c(500000, 501000),
#'   ycoord = c(5600000, 5601000)
#' )
#' bbox <- mgrs_to_latlong_bbox(df)
#' }
#'
#' @export
mgrs_to_latlong_bbox <- function(df) {

  hemisphere <- utm_zone <- NULL

  # Check if the specified columns exist
  if (!"xcoord" %in% names(df) || !"ycoord" %in% names(df)) {
    stop("Columns 'xcoord' and/or 'ycoord' not found in data frame.")
  }

  # Add a unique identifier for each row to handle potential duplicates
  df$row_id <- seq_len(nrow(df))

  # Extract UTM zone from MGRS code and determine hemisphere
  df$utm_zone <- stringr::str_sub(df$cellCode, 1, 2)
  df$lat_band <- stringr::str_sub(df$cellCode, 3, 3)
  df$hemisphere <- dplyr::if_else(df$lat_band %in% LETTERS[1:13],
                                  "South",
                                  "North")

  # Group by UTM zone and hemisphere
  df_list <- df %>%
    dplyr::group_by(utm_zone, hemisphere) %>%
    dplyr::group_split()

  # Create and transform an sf object for each UTM zone
  all_sf_objects <- purrr::map(df_list, function(group_df) {
    utm_zone <- group_df$utm_zone[1]
    hemisphere <- group_df$hemisphere[1]

    # Construct the correct EPSG code
    # UTM zones must be 2 digits (e.g. 05)
    utm_zone_num <- as.numeric(utm_zone)
    epsg_code <- if (hemisphere == "North") {
      paste0("EPSG:326", sprintf("%02d", utm_zone_num))
    } else {
      paste0("EPSG:327", sprintf("%02d", utm_zone_num))
    }

    # Create sf object and transform to EPSG:4326
    group_sf <- sf::st_as_sf(group_df, coords = c("xcoord", "ycoord"),
                             crs = epsg_code)
    sf::st_transform(group_sf, "EPSG:4326")
  })

  # Combine all sf objects and compute the final bbox
  final_sf_latlong <- do.call(rbind, all_sf_objects)

  # Return the bbox of the longitude/latitude data
  sf::st_bbox(final_sf_latlong)
}
