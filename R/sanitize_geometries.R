#' @noRd
sanitize_geometries <- function(sf_object, buffer_dist_meters = 10) {

  # Check for null object
  if (is.null(sf_object)) {
    return(sf_object)
  }

  # Check if object is sf and if not (e.g. sfc) convert to sf
  if (!inherits(sf_object, "sf")) {
    sf_object <- sf::st_as_sf(sf_object)
  }

  # Handle empty sf objects
  if (nrow(sf_object) == 0) {
    return(sf_object)
  }

  # Determine geometry type
  geom_type <- sf::st_geometry_type(sf_object)

  if (geom_type == "POINT" || geom_type == "MULTIPOINT") {
    # Check if the data is in Lon/Lat (unprojected)
    if (sf::st_is_longlat(sf_object)) {

      # 1. Store original CRS
      original_crs <- sf::st_crs(sf_object)

      # 2. Dynamically choose a suitable projected CRS (e.g., local UTM zone)
      #    We use the centroid to find a suitable local projection.
      center_point <- suppressWarnings(
        sf::st_centroid(sf::st_geometry(sf_object), of_largest_polygon = TRUE)
      )
      target_crs <- sf::st_crs(center_point)

      # Fallback check (st_crs(point) can sometimes return 4326 if unprojected)
      if (sf::st_is_longlat(target_crs) || is.na(target_crs)) {
        target_crs <- sf::st_crs("EPSG:3395") # Global fallback (World Mercator)
      }

      # 3. Transform to projected CRS, apply buffer (in meters), and transform back
      sf_object <- sf_object %>%
        sf::st_transform(crs = target_crs) %>%
        sf::st_buffer(dist = buffer_dist_meters) %>% # Use real-world distance
        sf::st_transform(crs = original_crs)

    } else {
      # Data is already projected (e.g., UTM), so buffer directly using original units
      # We assume the user's units align with the new buffer_dist_meters if it's projected.
      sf_object <- sf_object %>%
        sf::st_buffer(dist = buffer_dist_meters)
    }

  } else if (geom_type == "LINESTRING" || geom_type == "MULTILINESTRING") {
    # Cast to POLYGON by closing the line(s)
    sf_object <- sf_object %>%
      sf::st_cast("POLYGON")

  } else if (geom_type == "GEOMETRYCOLLECTION") {
    # 1. Extract and clean the geometry (keeps only POLYGONs)
    cleaned_geom <- sf_object %>%
      sf::st_geometry() %>%            # Extract sfc object
      sf::st_collection_extract("POLYGON") # Extract only POLYGONs

    # 2. Re-assign the cleaned geometry to the sf object
    sf_object <- sf_object %>%
      sf::st_set_geometry(cleaned_geom) %>%
      dplyr::select(-dplyr::starts_with("geom")) # Remove old geometry columns if any
  }

  # Ensure the final output is a MULTIPOLYGON for consistency
  if (sf::st_geometry_type(sf_object) != "MULTIPOLYGON") {
    sf_object <- sf_object %>% sf::st_cast("MULTIPOLYGON")
  }

  return(sf_object)
}
