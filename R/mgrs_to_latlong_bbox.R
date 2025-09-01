#' @title Create a Single Bounding Box from MGRS Data in a Projected CRS
#' @description Converts MGRS coordinates to a single sf bounding box in a
#'   suitable projected CRS, handling data that spans multiple UTM zones.
#'
#' @param df A data frame with at least three columns: `cellCode` (containing
#'   MGRS strings), and `xcoord` and `ycoord` for easting and northing.
#'
#' @return An sf bounding box (`sf::st_bbox`) in a single projected CRS (e.g., Albers).
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
  df$hemisphere <- dplyr::if_else(df$lat_band %in% LETTERS[1:13], "South", "North")

  # Group by UTM zone and hemisphere
  df_list <- df %>%
    dplyr::group_by(utm_zone, hemisphere) %>%
    dplyr::group_split()

  # Create and transform an sf object for each UTM zone
  all_sf_objects <- purrr::map(df_list, function(group_df) {
    utm_zone <- group_df$utm_zone[1]
    hemisphere <- group_df$hemisphere[1]

    # Construct the correct EPSG code
    epsg_code <- if (hemisphere == "North") {
      paste0("EPSG:326", utm_zone)
    } else {
      paste0("EPSG:327", utm_zone)
    }

    # Create sf object and transform to EPSG:4326
    group_sf <- sf::st_as_sf(group_df, coords = c("xcoord", "ycoord"), crs = epsg_code)
    sf::st_transform(group_sf, "EPSG:4326")
  })

  # Combine all sf objects and compute the final bbox
  final_sf_latlong <- do.call(rbind, all_sf_objects)

  # # Transform to a single suitable projected CRS (e.g., Albers)
  # # This is the single CRS for calculations
  # final_sf_projected <- sf::st_transform(final_sf_latlong, "ESRI:54012")

  # Return the bbox of the projected data
 # sf::st_bbox(final_sf_projected)
  sf::st_bbox(final_sf_latlong)
}
