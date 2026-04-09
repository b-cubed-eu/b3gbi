#' @noRd
intersect_grid_with_polygon <- function(grid,
                                        intersection_target) {
  # The following intersection operation can be slow when clipping large numbers
  # of cells. We optimize by identifying cells that are completely within the
  # target and only clipping those that cross boundaries.

  original_s2 <- sf::sf_use_s2()
  on.exit(suppressMessages(sf::sf_use_s2(original_s2)))

  # Ensure geometries are valid before intersection

  # Helper function for optimized intersection
  do_intersection <- function(g, t) {
    # 1. Quick filter: find which cells intersect at all
    intersects_idx <- sf::st_intersects(g, t, sparse = TRUE)
    touching_any <- lengths(intersects_idx) > 0
    if (!any(touching_any)) {
      return(NULL)
    }

    g_candidate <- g[touching_any, ]

    # 2. Identify cells COMPLETELY WITHIN the target
    # (These don't need clipping, preserving their original area/geometry)
    within_idx <- sf::st_within(g_candidate, t, sparse = TRUE)
    is_within <- lengths(within_idx) > 0

    # Separate into fully-enclosed and boundary cells
    grid_within <- g_candidate[is_within, ]
    grid_boundary <- g_candidate[!is_within, ]

    # 3. Clip only the boundary cells
    if (nrow(grid_boundary) > 0) {
      grid_clipped <- sf::st_intersection(grid_boundary, t) %>%
        dplyr::select(
          dplyr::all_of(c("cellid", "geometry")),
          dplyr::any_of(c("area", "cellCode"))
        )
    } else {
      grid_clipped <- NULL
    }

    # 4. Result is everything within plus everything clipped
    res <- grid_within %>%
      dplyr::select(
        dplyr::all_of(c("cellid", "geometry")),
        dplyr::any_of(c("area", "cellCode"))
      )

    if (!is.null(grid_clipped)) {
      res <- rbind(res, grid_clipped)
    }

    return(res)
  }

  # Try with current S2 setting
  result <- NULL
  tryCatch(
    {
      result <- do_intersection(sf::st_make_valid(grid), sf::st_make_valid(intersection_target))
    },
    error = function(e) {
      # Log issue but don't stop yet, we will retry with S2 OFF
      message(paste0("Intersection failed with: ", e$message))
    }
  )

  # Retry with S2 OFF if failed or returning empty
  if (is.null(result) || nrow(result) == 0) {
    message("Retrying intersection with S2 disabled...")
    suppressMessages(sf::sf_use_s2(FALSE))
    tryCatch(
      {
        result <- do_intersection(sf::st_make_valid(grid), sf::st_make_valid(intersection_target))
      },
      error = function(e) {
        message(paste0("Intersection failed even with S2 off: ", e$message))
      }
    )
  }

  # Final fallback if failed or returning empty
  if (is.null(result) || nrow(result) == 0) {
    message("Optimized intersection failed or returned empty. Falling back to st_filter.")
    # Last resort: just find which cells intersect, don't clip them.
    # This avoids most clipping/S2/GEOS bottlenecks.
    result <- sf::st_filter(grid, intersection_target)
  }

  # Final check
  if (is.null(result) || nrow(result) == 0) {
    stop("No spatial intersection between map data and grid.")
  }

  # Deduplicate by cellCode to ensure a clean one-to-one mapping for joins
  if ("cellCode" %in% names(result)) {
    result <- dplyr::distinct(result, .data[["cellCode"]], .keep_all = TRUE)
  }

  return(result)
}
