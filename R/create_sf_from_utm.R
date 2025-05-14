#' Create an sf object from UTM coordinates, handling multiple zones correctly.
#'
#' This function takes a data frame with UTM coordinates (xcoord, ycoord) and a
#' utmzone column, and creates an sf object with the correct CRS for each zone.
#'
#' @param df A data frame with columns: xcoord, ycoord, and utmzone.
#' @param output_crs The EPSG code or CRS string for the desired output CRS.
#'                   If NULL, the CRS of the first UTM zone will be used.
#' @return An sf object with the geometry correctly defined for each UTM zone.
#'
#' @export
create_sf_from_utm <- function(df, output_crs = NULL) {
  # 1. Input Validation
  required_cols <- c("xcoord", "ycoord", "utmzone")
  if (!all(required_cols %in% names(df))) {
    stop(paste("Input data frame must contain columns:", paste(required_cols, collapse = ", ")))
  }
  if (!is.numeric(df$xcoord) || !is.numeric(df$ycoord) || !is.numeric(df$utmzone)) {
    stop("xcoord, ycoord, and utmzone columns must be numeric.")
  }

  # 2. Split data by UTM zone
  df_list <- df %>%
    dplyr::group_split(utmzone)

  # 3. Create sf objects for each zone
  sf_list <- lapply(df_list, function(zone_df) {
    zone <- unique(zone_df$utmzone)
    #  northern hemisphere assumption.
    sf_zone <- sf::st_as_sf(zone_df, coords = c("xcoord", "ycoord"), crs = paste0("EPSG:326", zone))
    return(sf_zone)
  })

  # 4. Determine output CRS
  if (is.null(output_crs)) {
    output_crs <- sf::st_crs(sf_list[[1]]) # simplified.
  }

  # 5. Transform to a common CRS and combine
  if (length(sf_list) > 0){
    combined_sf <- sf::st_transform(sf_list[[1]], crs = output_crs)
    if (length(sf_list) > 1){
      for (i in 2:length(sf_list)){
        sf_list[[i]] <- sf::st_transform(sf_list[[i]], crs = output_crs)
        combined_sf <- rbind(combined_sf, sf_list[[i]])
      }
    }
  }
  else{
    combined_sf <- sf::st_sf(geometry = sf::st_sfc())
  }
  return(combined_sf)
}
