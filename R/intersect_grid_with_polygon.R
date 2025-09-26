#' @noRd
intersect_grid_with_polygon <- function(grid,
                                        intersection_target) {
  # The following intersection operation requires special error handling
  # because it fails when the grid contains invalid geometries.
  # Therefore when invalid geometries are encountered, it will retry the
  # operation with spherical geometry turned off. This often succeeds.

  # Try to intersect grid with intersection_target
  result <- NULL  # Initialize to capture result of intersection
  tryCatch({
    # Attempt without altering the spherical geometry setting
    result <- grid %>%
      sf::st_intersection(intersection_target) %>%
      dplyr::select(dplyr::all_of(c("cellid", "geometry")),
                    dplyr::any_of("area"))
  }, error = function(e) {
    if (grepl("Error in wk_handle.wk_wkb", e) ||
        grepl("TopologyException", e)) {
      message(
        paste0(
          "Encountered a geometry error during intersection. This may be ",
          "due to invalid polygons in the grid."
        )
      )
    } else {
      stop(e)
    }
  })
  if (is.null(result)) {
    # If intersection failed, turn off spherical geometry
    message("Retrying the intersection with spherical geometry turned off.")
    sf::sf_use_s2(FALSE)
    # Retry the intersection operation
    result <- grid %>%
      sf::st_intersection(intersection_target) %>%
      dplyr::select(dplyr::all_of(c("cellid", "geometry")),
                    dplyr::any_of("area"))
    # Notify success after retry
    message("Intersection succeeded with spherical geometry turned off.")
  }
  # Check if there is any spatial intersection
  if (nrow(result) == 0) {
    stop("No spatial intersection between map data and grid.")
  }
  # Set grid to result
  clipped_grid <- result

  return(clipped_grid)
}
