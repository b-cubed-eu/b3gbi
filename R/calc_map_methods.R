#' @export
calc_map.default <- function(x, ...) {

  warning(paste0(
    "calc_map does not know how to handle object of class ", class(x), ". ",
    "Please ensure you are not calling calc_map directly on an object."))

}

#' @export
#' @rdname calc_map
calc_map.completeness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "completeness"))

  indicator <- calc_map_completeness_core(x = x, ...)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.hill0 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "hill0"))

  indicator <- calc_map_hill_core(x = x, type = "hill0", ...)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.hill1 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "hill1"))

  indicator <- calc_map_hill_core(x = x, type = "hill1", ...)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.hill2 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "hill2"))

  indicator <- calc_map_hill_core(x = x, type = "hill2", ...)
  return(indicator)

}

#' @param occ_type Integer controlling the occupancy denominator (default `0`):
#'   * `0` — **Total-area occupancy**: number of grid cells occupied by the
#'     species (across all years) / total number of grid cells in the study
#'     region (including unsampled cells). *Presence-only caveat*: empty cells
#'     cannot be assumed truly unoccupied; they may simply be unsampled.
#'   * `1` — **Relative-to-ever-occupied occupancy**: species' cells / cells
#'     with *at least one occurrence (any species, any year)*. Conditions on
#'     cells where sampling effort is documented.
#'   * `2` — **Temporal mean annual occupancy**: for each year, compute the
#'     proportion of that year's occupied cells (any species) in which the
#'     species was recorded; then average those annual proportions across all
#'     years in the data. This captures how consistently a species occupies the
#'     active sampling footprint over time.
#'
#'   **Note on presence-only data**: All three types rely on presence-only
#'   records. A cell with no records cannot be assumed to be truly unoccupied.
#'   Types 1 and 2 condition on cells with documented occurrences, but those
#'   still reflect sampling effort rather than true species absence.
#'
#'   **Note on cell aggregation**: When a coarser `cell_size` is chosen, data
#'   are aggregated to coarser grid cells before this calculation. All
#'   denominators count post-aggregation cells (`cellid`).
#' @export
#' @rdname calc_map
calc_map.relative_occupancy <- function(x, occ_type = 0, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "relative_occupancy"))

  if (!occ_type %in% c(0L, 1L, 2L)) {
    stop("`occ_type` must be 0, 1, or 2.")
  }

  cellid <- taxonKey <- scientificName <- diversity_val <- cellCode <- NULL
  year <- species_cells_yr <- n_occ_cells_yr <- annual_prop <- NULL

  if (occ_type == 0L) {
    # -------------------------------------------------------------------------
    # Type 0: denominator = total grid cells in study region (original behaviour)
    # -------------------------------------------------------------------------
    total_num_cells <- attr(x, "total_num_cells")
    if (is.null(total_num_cells)) {
      stop(paste0(
        "total_num_cells attribute not found. ",
        "Cannot calculate relative occupancy with occ_type = 0."
      ))
    }

    # Count distinct cells occupied by each species (across all years)
    occupied_cells <- x %>%
      dplyr::distinct(cellid, scientificName) %>%
      dplyr::count(scientificName, name = "occupied_cells")

    indicator <- occupied_cells %>%
      dplyr::mutate(diversity_val = occupied_cells / total_num_cells) %>%
      dplyr::left_join(
        x %>% dplyr::distinct(cellid, cellCode, scientificName, taxonKey),
        by = "scientificName"
      ) %>%
      dplyr::select(cellid, cellCode, taxonKey, scientificName, diversity_val) %>%
      dplyr::arrange(cellid)

  } else if (occ_type == 1L) {
    # -------------------------------------------------------------------------
    # Type 1: denominator = cells with >= 1 occurrence (any species, any year)
    # -------------------------------------------------------------------------
    ever_occupied <- dplyr::n_distinct(x$cellid)

    occupied_cells <- x %>%
      dplyr::distinct(cellid, scientificName) %>%
      dplyr::count(scientificName, name = "occupied_cells")

    indicator <- occupied_cells %>%
      dplyr::mutate(diversity_val = occupied_cells / ever_occupied) %>%
      dplyr::left_join(
        x %>% dplyr::distinct(cellid, cellCode, scientificName, taxonKey),
        by = "scientificName"
      ) %>%
      dplyr::select(cellid, cellCode, taxonKey, scientificName, diversity_val) %>%
      dplyr::arrange(cellid)

  } else {
    # -------------------------------------------------------------------------
    # Type 2: temporal mean annual occupancy.
    # For each year, count cells with >= 1 record (any species) as the
    # denominator; count cells with >= 1 record for the focal species as the
    # numerator. Compute the annual proportion, then average across years.
    # The map value for each species is the temporal mean of those proportions.
    #
    # Presence-only caveat: the denominator reflects annual recording footprint,
    # not true sampling effort; years with slim data may dominate the average.
    # -------------------------------------------------------------------------
    # Number of cells with any occurrence in each year
    active_cells_per_year <- x %>%
      dplyr::distinct(year, cellid) %>%
      dplyr::count(year, name = "n_occ_cells_yr")

    # Number of cells per species per year
    species_cells_per_year <- x %>%
      dplyr::distinct(year, cellid, scientificName, taxonKey) %>%
      dplyr::count(year, scientificName, taxonKey, name = "species_cells_yr")

    # Annual proportion then temporal mean per species
    species_mean_prop <- species_cells_per_year %>%
      dplyr::left_join(active_cells_per_year, by = "year") %>%
      dplyr::mutate(annual_prop = species_cells_yr / n_occ_cells_yr) %>%
      dplyr::summarise(
        diversity_val = mean(annual_prop, na.rm = TRUE),
        .by = c(scientificName, taxonKey)
      )

    # Join back to the cell-level data to produce one row per species x cell
    indicator <- species_mean_prop %>%
      dplyr::left_join(
        x %>% dplyr::distinct(cellid, cellCode, scientificName, taxonKey),
        by = c("scientificName", "taxonKey")
      ) %>%
      dplyr::select(cellid, cellCode, taxonKey, scientificName, diversity_val) %>%
      dplyr::arrange(cellid)
  }

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.obs_richness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "obs_richness"))

  taxonKey <- NULL

  # Calculate observed species richness over the grid
  indicator <- x %>%
    dplyr::summarize(diversity_val = sum(dplyr::n_distinct(taxonKey)),
                     .by = c("cellid", "cellCode"))

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.total_occ <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "total_occ"))

  obs <- NULL

  # Calculate total number of occurrences over the grid
  indicator <- x %>%
    dplyr::summarize(diversity_val = sum(obs, na.rm = TRUE),
                     .by = c("cellid", "cellCode"))

  return(indicator)

}

#' @param newness_min_year (Optional) If set, only shows values above this
#'  (e.g. 1970). Values below the minimum will be replaced with NA. This can be
#'  useful e.g. if you have outlier cells where the data is very old causing the
#'  legend gradient to stretch in a way that makes other cell values difficult
#'  to discern.
#' @export
#' @rdname calc_map
calc_map.newness <- function(x,
                             newness_min_year = NULL,
                             ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "newness"))

  year <- NULL

  # Calculate mean year of occurrence over the grid
  indicator <- x %>%
    dplyr::summarize(diversity_val = round(mean(year, na.rm = TRUE)),
                     .by = c("cellid", "cellCode"))

  if (!is.null(newness_min_year)) {
    indicator$diversity_val <- ifelse(
      indicator$diversity_val > newness_min_year,
      indicator$diversity_val,
      NA)
  }

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.spec_richness_density <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_richness_density"))

  diversity_val <- taxonKey <- area <- cellid <- cellCode <- NULL

  # Calculate density of species richness over the grid (per square km)
  indicator <- x %>%
    dplyr::reframe(diversity_val = dplyr::n_distinct(taxonKey) / area,
                   .by = c("cellid", "cellCode")) %>%
    dplyr::distinct(cellid, cellCode, diversity_val) %>%
    dplyr::mutate(diversity_val = as.numeric(diversity_val))

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.occ_density <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "occ_density"))

  diversity_val <- obs <- area <- cellid <- cellCode <- NULL

  # Calculate density of occurrences over the grid (per square km)
  indicator <- x %>%
    dplyr::reframe(diversity_val = sum(obs, na.rm = TRUE) / area,
                   .by = c("cellid", "cellCode")) %>%
    dplyr::distinct(cellid, cellCode, diversity_val) %>%
    dplyr::mutate(diversity_val = as.numeric(diversity_val))

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.williams_evenness <- function(x, ...) {

  stopifnot_error(
    "Wrong data class. This is an internal function and is not
    meant to be called directly.", inherits(x, "williams_evenness")
  )

  # Call function to calculate evenness over a grid
  indicator <- calc_map_evenness_core(x = x,
                                      type = "williams_evenness",
                                      ...)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.pielou_evenness <- function(x, ...) {

  stopifnot_error(
    "Wrong data class. This is an internal function and is not meant to be
    called directly.", inherits(x, "pielou_evenness")
  )

  # Call function to calculate evenness over a grid
  indicator <- calc_map_evenness_core(x = x,
                                      type = "pielou_evenness",
                                      ...)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.ab_rarity <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "ab_rarity"))

  obs <- taxonKey <- cellid <- obs_taxon <- rarity <- NULL
  obs_cell <- cellCode <- NULL

  # Select relevant columns
  x <- x %>%
    dplyr::select(cellid, cellCode, taxonKey, obs)

  # Remove invalid rows
  x <- x[stats::complete.cases(x), ]

  # Calculate total summed rarity (in terms of abundance) for each grid cell
  indicator <- x %>%
    # calculate number of records for each species
    dplyr::summarise(obs_taxon = sum(obs, na.rm = TRUE),
                     .by = c(cellid, cellCode, taxonKey)) %>%
    # calculate number of records for each grid cell
    dplyr::mutate(obs_cell = sum(obs_taxon, na.rm = TRUE),
                  .by = cellid) %>%
    # calculate rarity for each species
    dplyr::mutate(rarity = 1 / (obs_taxon / obs_cell)) %>%
    # calculate total rarity for each grid cell
    dplyr::summarise(diversity_val = sum(rarity), .by = c(cellid, cellCode)) %>%
    # arrange by cellid
    dplyr::arrange(cellid)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.area_rarity <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "area_rarity"))

  cellid <- taxonKey <- rarity <- cellCode <- NULL
  occ_by_taxa <- total_cells <- NULL

  # Select relevant columns
  x <- x %>%
    select(cellid, cellCode, taxonKey)

  # Remove invalid rows
  x <- x[stats::complete.cases(x), ]

  # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
  # frequency for each species
  indicator <- x %>%
    # calculate number of cells each species occurs in
    dplyr::mutate(occ_by_taxa = sum(dplyr::n_distinct(cellid)),
                  .by = c(taxonKey)) %>%
    # remove duplicates
    unique() %>%
    # calculate total number of cells
    dplyr::mutate(total_cells = sum(dplyr::n_distinct(cellid))) %>%
    # calculate rarity for each species
    dplyr::mutate(rarity = 1 / (occ_by_taxa / total_cells)) %>%
    # calculate total rarity for each grid cell
    dplyr::summarise(diversity_val = sum(rarity), .by = c(cellid, cellCode)) %>%
    # arrange by cellid
    dplyr::arrange(cellid)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.spec_occ <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "spec_occ"))

  diversity_val <- obs <- taxonKey <- cellid <- scientificName <-
    cellCode <- NULL

  # Calculate total occurrences for each species by grid cell
  indicator <- x %>%
    dplyr::mutate(diversity_val = sum(obs),
                  .by = c(taxonKey, cellid, cellCode)) %>%
    dplyr::distinct(cellid, cellCode, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, cellCode, taxonKey, scientificName, diversity_val)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.spec_range <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "spec_range"))

  cellid <- taxonKey <- scientificName <- diversity_val <- cellCode <- NULL

  # Flatten occurrences for each species by grid cell
  indicator <- x %>%
    dplyr::mutate(diversity_val = 1) %>%
    dplyr::distinct(cellid, cellCode, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, cellCode, taxonKey, scientificName, diversity_val)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.tax_distinct <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "tax_distinct"))

  cellid <- diversity_val <- cellCode <- NULL

  # Early check for empty input
  if (nrow(x) == 0) {
    return(tibble::tibble(cellid = integer(),
                          cellCode = character(),
                          diversity_val = numeric()))
  }

  if (requireNamespace("taxize", quietly = TRUE)) {

    # Retrieve taxonomic data from GBIF
    tax_hier <- my_classification(unique(x$scientificName),
                                       db = "gbif",
                                       ...)

  } else {
    stop(
      "The 'taxize' package is required to calculate taxonomic distinctness."
    )
  }

  # A helper function to check for empty rows
  is_not_empty <- function(df) {
    nrow(df) == 0
  }

  # Calculate taxonomic distinctness
  indicator <- x %>%
    tibble::add_column(diversity_val = NA) %>%
    dplyr::group_split(cellid) %>%
    purrr::discard(is_not_empty) %>%
    purrr::map(function(df) {
      dplyr::mutate(df,
                    diversity_val = compute_tax_distinct_formula(df, tax_hier)
      )
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(.data$diversity_val != 0) %>%
    dplyr::distinct(cellid, cellCode, diversity_val, .keep_all = TRUE) %>%
    dplyr::select(cellid, cellCode, diversity_val)

  return(indicator)

}