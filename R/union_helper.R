# union_helper()
# A robust helper function to perform sf::st_union on two sf objects,
# safely handling cases where one or both of the input objects are empty.
#
# This function prevents errors that occur when attempting to union empty
# and non-empty geometries.
#
# @param x The first sf or sfc object.
# @param y The second sf or sfc object.
#
# @return An sf object representing the union of the two inputs. If both
#         inputs are empty, it returns an empty sf object with the CRS
#         of the first input (or NULL if both are empty).
#
# @examples
# # Create some example data
# poly1 <- sf::st_sf(geom = sf::st_sfc(sf::st_polygon(list(cbind(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))))), crs = 4326)
# poly2 <- sf::st_sf(geom = sf::st_sfc(sf::st_polygon(list(cbind(c(1, 2, 2, 1, 1), c(1, 1, 2, 2, 1))))), crs = 4326)
# empty_sf <- sf::st_sf(geometry = sf::st_sfc(crs = 4326))
#
# # Example 1: Standard union (no empty objects)
# union_helper(poly1, poly2)
#
# # Example 2: Union with one empty object
# union_helper(poly1, empty_sf)
#
# # Example 3: Union with both objects empty
# union_helper(empty_sf, empty_sf)
#' @noRd
union_helper <- function(x, y) {
  x_empty <- is_sf_empty(x)
  y_empty <- is_sf_empty(y)

  if (!x_empty && !y_empty) {
    # Case 1: Both objects are non-empty
    result <- sf::st_union(x, y) %>%
      sf::st_as_sf()
  } else if (!x_empty) {
    # Case 2: y is empty, return x
    result <- x
  } else if (!y_empty) {
    # Case 3: x is empty, return y
    result <- y
  } else {
    # Case 4: Both are empty, return an empty sf object with the correct CRS
    # Handle the case where the first object's CRS is also empty
    crs <- if (!is.null(sf::st_crs(x))) sf::st_crs(x) else sf::st_crs(y)
    result <- sf::st_sf(geometry = sf::st_sfc(crs = crs))
  }

  return(result)
}
