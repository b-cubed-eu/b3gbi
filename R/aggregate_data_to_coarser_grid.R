#' Aggregate data and grid to a coarser resolution before indicator calculation
#'
#' Instead of calculating the indicator at native resolution and then aggregating
#' the values, this function reassigns data rows to coarser grid cells.
#' The indicator is then calculated on this reassigned data, which gives correct
#' results for all indicator types without special handling.
#'
#' @param data_assigned Data (sf or data.frame) with cellid from native grid assignment
#' @param clipped_grid sf object with native grid cells (clipped to boundary)
#' @param cell_size Coarser cell size (meters for EEA/MGRS, degrees for EQDGC)
#' @param grid_crs CRS for grid creation
#' @return List with $data and $grid (coarser sf grid)
#' @noRd
aggregate_data_to_coarser_grid <- function(data_assigned, clipped_grid,
                                            cell_size, grid_crs) {
  has_geom <- inherits(data_assigned, "sf")

  # Transform grid to the CRS used for coarse grid creation
  grid_proj <- sf::st_transform(clipped_grid, grid_crs)
  grid_proj$orig_cellid <- grid_proj$cellid

  # Create a coarse grid covering the native grid extent
  bbox <- sf::st_bbox(grid_proj)
  coarse_grid <- bbox %>%
    sf::st_make_grid(cellsize = c(cell_size, cell_size),
                     offset = c(bbox["xmin"], bbox["ymin"])) %>%
    sf::st_sf() %>%
    dplyr::mutate(coarse_id = dplyr::row_number())

  # Assign each native grid cell to a coarse cell via spatial intersection
 # orig_s2 <- sf::sf_use_s2()
 # sf::sf_use_s2(FALSE)
  sf::st_agr(grid_proj) <- "constant"
  sf::st_agr(coarse_grid) <- "constant"
  overlaps <- sf::st_intersection(
    grid_proj[, c("orig_cellid", "geometry")],
    coarse_grid[, c("coarse_id", "geometry")]
  )
 # sf::sf_use_s2(orig_s2)

  if (nrow(overlaps) == 0) {
    warning("Aggregation failed: no overlap between native and coarse grid.")
    return(list(data = data_assigned, grid = clipped_grid))
  }

  # Keep only the largest overlap for each native cell
  overlaps$ov_area <- as.numeric(sf::st_area(overlaps))
  overlaps <- overlaps[order(overlaps$orig_cellid, -overlaps$ov_area), ]
  overlaps <- overlaps[!duplicated(overlaps$orig_cellid), ]

  # Build mapping: orig_cellid -> coarse_id
  cellid_map <- stats::setNames(overlaps$coarse_id, as.character(overlaps$orig_cellid))

  # Get data as data frame
  if (has_geom) {
    data_df <- sf::st_drop_geometry(data_assigned)
  } else {
    data_df <- data_assigned
  }

  # Reassign each data row's cellid to the coarse cell
  # Keep the original cellCode so gridded indicators have multiple values per coarse cell
  data_df$cellid <- cellid_map[as.character(data_df$cellid)]
  data_df <- data_df[!is.na(data_df$cellid), ]

  if (nrow(data_df) == 0) {
    warning("Aggregation failed: no data mapped to coarse grid cells.")
    return(list(data = data_assigned, grid = clipped_grid))
  }

  # Build coarse grid with matching identifiers
  # Do NOT include cellCode — the data keeps native cellCode for gridded indicators
  coarse_grid_out <- coarse_grid[coarse_grid$coarse_id %in% data_df$cellid, ]
  coarse_grid_out$cellid <- coarse_grid_out$coarse_id
  coarse_grid_out$area <- coarse_grid_out %>%
    sf::st_area() %>%
    units::set_units("km^2")

  # Keep only needed columns in grid (no cellCode — join uses cellid only)
  coarse_grid_out <- coarse_grid_out[, c("cellid", "area", "geometry")]

  # Remove old area column from data if present (will be joined from grid later)
  if ("area" %in% names(data_df)) {
    data_df$area <- NULL
  }

  # Build output data — use coarse cellid, keep native cellCode
  sf::st_agr(coarse_grid_out) <- "constant"
  if (has_geom) {
    # Create sf with coarse grid cell centroids as point geometries
    coarse_centers <- sf::st_centroid(coarse_grid_out)
    data_out <- sf::st_sf(data_df, geometry = sf::st_geometry(
      coarse_centers[match(data_df$cellid, coarse_centers$cellid), ]
    ))
  } else {
    data_out <- data_df
  }

  return(list(data = data_out, grid = coarse_grid_out))
}
