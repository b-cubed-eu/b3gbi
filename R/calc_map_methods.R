#' @export
calc_map.default <- function(x, ...) {

  warning(paste0(
    "calc_map does not know how to handle object of class ", class(x), ". ",
    "Please ensure you are not calling calc_map directly on an object."))

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