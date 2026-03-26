#' Aggregate indicator from native grid to a coarser user-specified grid
#'
#' @param diversity_grid sf object with native grid cells and indicator values
#' @param cell_size User-specified cell size (meters for EEA/MGRS, degrees for EQDGC)
#' @param grid_crs CRS for grid creation (projected for EEA/MGRS, EPSG:4326 for EQDGC)
#' @param output_crs Output CRS for final result
#' @param type Indicator type (determines aggregation method)
#' @return sf object with coarser grid cells and aggregated indicator values
#' @noRd
aggregate_to_coarser_grid <- function(diversity_grid, cell_size,
                                       grid_crs, output_crs, type) {
  # Remove NA indicator values (cells without data)
  dg <- diversity_grid[!is.na(diversity_grid$diversity_val), ]
  if (nrow(dg) == 0) return(diversity_grid)

  # Transform to the CRS used for grid creation
  dg_proj <- sf::st_transform(dg, grid_crs)
  sf::st_agr(dg_proj) <- "constant"

  # Calculate original cell areas for weighting
  dg_proj$orig_area <- as.numeric(sf::st_area(dg_proj))
  dg_proj$orig_row <- seq_len(nrow(dg_proj))

  # Create a coarser grid covering the data extent
  bbox <- sf::st_bbox(dg_proj)
  coarse_grid <- bbox %>%
    sf::st_make_grid(cellsize = c(cell_size, cell_size),
                     offset = c(bbox["xmin"], bbox["ymin"])) %>%
    sf::st_sf() %>%
    dplyr::mutate(coarse_id = dplyr::row_number())

  # Intersect native cells with coarse grid
  orig_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  overlaps <- sf::st_intersection(
    dg_proj[, c("diversity_val", "orig_area", "orig_row", "geometry")],
    coarse_grid[, c("coarse_id", "geometry")]
  )
  sf::sf_use_s2(orig_s2)

  if (nrow(overlaps) == 0) return(diversity_grid)

  # Calculate area weight: fraction of original cell in each coarse cell
  overlaps$overlap_area <- as.numeric(sf::st_area(overlaps))
  # Match back to original area via orig_row
  orig_area_lookup <- stats::setNames(dg_proj$orig_area, dg_proj$orig_row)
  overlaps$weight <- overlaps$overlap_area / orig_area_lookup[as.character(overlaps$orig_row)]

  # Weighted indicator value
  overlaps$weighted_val <- overlaps$diversity_val * overlaps$weight

  # Determine aggregation method
  sum_types <- c("total_occ", "spec_occ", "obs_richness", "hill0",
                  "tax_distinct", "cum_richness", "completeness")

  if (type %in% sum_types) {
    agg_vals <- stats::aggregate(
      overlaps$weighted_val,
      by = list(coarse_id = overlaps$coarse_id),
      FUN = sum, na.rm = TRUE
    )
  } else {
    agg_vals <- stats::aggregate(
      overlaps$weighted_val,
      by = list(coarse_id = overlaps$coarse_id),
      FUN = sum, na.rm = TRUE
    )
    weight_sums <- stats::aggregate(
      overlaps$weight,
      by = list(coarse_id = overlaps$coarse_id),
      FUN = sum, na.rm = TRUE
    )
    agg_vals$x <- agg_vals$x / weight_sums$x
  }

  names(agg_vals)[2] <- "diversity_val"

  # Join aggregated values back to coarse grid
  coarse_grid <- merge(coarse_grid, agg_vals, by = "coarse_id", all.x = TRUE)
  coarse_grid <- coarse_grid[!is.na(coarse_grid$diversity_val), ]

  # Transform to output CRS
  coarse_grid <- sf::st_transform(coarse_grid, crs = output_crs)

  # Add area column
  coarse_grid$area <- coarse_grid %>%
    sf::st_area() %>%
    units::set_units("km^2")

  # Add cellid for compatibility
  coarse_grid$cellid <- seq_len(nrow(coarse_grid))

  # Keep only necessary columns
  coarse_grid <- coarse_grid[, c("cellid", "area", "diversity_val", "geometry")]

  return(coarse_grid)
}
