#' @noRd
sanitize_geometries <- function(sf_object) {

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
    # Cast to POLYGON by first creating a small buffer around the point(s)
    # The buffer distance (e.g., 0.01) depends on your CRS and scale
    sf_object <- sf_object %>%
      sf::st_buffer(dist = 0.01)

  } else if (geom_type == "LINESTRING" || geom_type == "MULTILINESTRING") {
    # Cast to POLYGON by closing the line(s)
    sf_object <- sf_object %>%
      sf::st_cast("POLYGON")

  } else if (geom_type == "GEOMETRYCOLLECTION") {
    # Cast to MULTIPOLYGON to extract polygons from the collection
    sf_object <- sf_object %>%
      sf::st_cast("MULTIPOLYGON")
  }

  # Ensure the final output is a MULTIPOLYGON for consistency
  if (sf::st_geometry_type(sf_object) != "MULTIPOLYGON") {
    sf_object <- sf_object %>% sf::st_cast("MULTIPOLYGON")
  }

  return(sf_object)
}
