#' @noRd
intersect_grid_with_polygon <- function(grid,
                                        intersection_target) {
  # The following intersection operation requires special error handling
  # because it fails when the grid contains invalid geometries.
  # Therefore when invalid geometries are encountered, it will retry the
  # operation with spherical geometry turned off. This often succeeds.

  # Try to intersect grid with intersection_target
  original_s2 <- sf::sf_use_s2()
  on.exit(sf::sf_use_s2(original_s2))

  result <- NULL
  tryCatch(
    {
      # Ensure geometries are valid before intersection
      grid_v <- sf::st_make_valid(grid)
      target_v <- sf::st_make_valid(intersection_target)

      result <- grid_v %>%
        sf::st_intersection(target_v) %>%
        dplyr::select(
          dplyr::all_of(c("cellid", "geometry")),
          dplyr::any_of(c("area", "cellCode"))
        )
    },
    error = function(e) {
      # Log issue but don't stop yet, we will retry with S2 OFF
      message(paste0("Intersection failed with: ", e$message))
    }
  )

  if (is.null(result) || nrow(result) == 0) {
    # If intersection failed or returned empty with S2, retry with S2 OFF
    message("Retrying intersection with S2 disabled and additional validation...")
    sf::sf_use_s2(FALSE)

    tryCatch(
      {
        grid_v <- sf::st_make_valid(grid)
        target_v <- sf::st_make_valid(intersection_target)

        result <- grid_v %>%
          sf::st_intersection(target_v) %>%
          dplyr::select(
            dplyr::all_of(c("cellid", "geometry")),
            dplyr::any_of(c("area", "cellCode"))
          )
        message("Intersection succeeded with spherical geometry turned off.")
      },
      error = function(e) {
        message("Intersection failed even with S2 off. Falling back to non-clipping intersection.")
        # Last resort: just find which cells intersect, don't clip them.
        # This avoids most GEOS topology errors.
        intersecting_indices <- sf::st_intersects(grid, intersection_target, sparse = FALSE)
        # st_intersects returns a logical matrix if sparse=FALSE
        row_intersects <- apply(intersecting_indices, 1, any)
        result <<- grid[row_intersects, ]
        # Area remains the full cell area
      }
    )
  }

  if (is.null(result)) {
    # This should technically not be reached now, but for safety:
    stop("Persistent geometry error in intersection.")
  }

  # Check if there is any spatial intersection
  if (nrow(result) == 0) {
    stop("No spatial intersection between map data and grid.")
  }

  return(result)
}
