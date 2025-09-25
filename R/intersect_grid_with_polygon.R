#' @noRd
intersect_grid_with_polygon <- function(grid,
                                        intersection_target,
                                        include_land,
                                        include_ocean) {
  # The following intersection operation requires special error handling
  # because it fails when the grid contains invalid geometries.
  # Therefore when invalid geometries are encountered, it will retry the
  # operation with spherical geometry turned off. This often succeeds.

  if (inherits(intersection_target, "list")) {
    # Perform separate intersections for land and ocean.
    if (include_land) {
      # Intersection for LAND
      result_land <- NULL
      tryCatch({
        result_land <- grid %>%
          sf::st_intersection(intersection_target$land) %>%
          dplyr::select(dplyr::all_of(c("cellid", "geometry")),
                        dplyr::any_of("area"))
      }, error = function(e) {
        if (grepl("Error in wk_handle.wk_wkb", e) || grepl("TopologyException", e)) {
          message("Encountered a land geometry error. Retrying with spherical geometry off.")
        } else {
          stop(e)
        }
      })
      if (is.null(result_land)) {
        sf::sf_use_s2(FALSE)
        result_land <- grid %>%
          sf::st_intersection(intersection_target$land) %>%
          dplyr::select(dplyr::all_of(c("cellid", "geometry")),
                        dplyr::any_of("area"))
        message("Land intersection succeeded with spherical geometry turned off.")
      }
    }

    if (include_ocean) {
      # Intersection for OCEAN
      result_ocean <- NULL
      tryCatch({
        result_ocean <- grid %>%
          sf::st_intersection(intersection_target$ocean) %>%
          dplyr::select(dplyr::all_of(c("cellid", "geometry")),
                        dplyr::any_of("area"))
      }, error = function(e) {
        if (grepl("Error in wk_handle.wk_wkb", e) || grepl("TopologyException", e)) {
          message("Encountered an ocean geometry error. Retrying with spherical geometry off.")
        } else {
          stop(e)
        }
      })

      if (is.null(result_ocean)) {
        sf::sf_use_s2(FALSE)
        result_ocean <- grid %>%
          sf::st_intersection(intersection_target$ocean) %>%
          dplyr::select(dplyr::all_of(c("cellid", "geometry")),
                        dplyr::any_of("area"))
        message("Ocean intersection succeeded with spherical geometry turned off.")
      }
    }

    if (include_land && include_ocean) {
      # Combine the clipped grids
      if (nrow(result_land) == 0 && nrow(result_ocean) == 0) {
        stop("No spatial intersection between map data and grid.")
      }
      clipped_grid <- dplyr::bind_rows(result_land, result_ocean)
    } else if (include_land) {
      if (nrow(result_land) == 0) {
        stop("No spatial intersection between land map data and grid.")
      }
      clipped_grid <- result_land
    } else if (include_ocean) {
      if (nrow(result_ocean) == 0) {
        stop("No spatial intersection between ocean map data and grid.")
      }
      clipped_grid <- result_ocean
    }
  } else {

    # Single geometry intersection
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
  }

  return(clipped_grid)
}
