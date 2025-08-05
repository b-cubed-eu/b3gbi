#' @export
calc_map.default <- function(x, ...){

  warning(paste("calc_map does not know how to handle object of class ",
                class(x),
                ". Please ensure you are not calling calc_map directly on an object."))

}

#' @noRd
calc_map.hill0 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "hill0"))

  indicator <- calc_map.hill_core(x = x,
                                  type = "hill0",
                                  ...)

  return(indicator)

}

#' @noRd
calc_map.hill1 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "hill1"))

  indicator <- calc_map.hill_core(x = x,
                                  type = "hill1",
                                  ...)

  return(indicator)

}

#' @noRd
calc_map.hill2 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "hill2"))

  indicator <- calc_map.hill_core(x = x,
                                  type = "hill2",
                                  ...)

  return(indicator)
}

#' @noRd
calc_map.hill_core <- function(x,
                               type = c("hill0", "hill1", "hill2"),
                               ...) {
  stopifnot_error(
    paste0(
    "Please check the class and structure of your data. This is an internal ",
    "function, not meant to be called directly."),
    inherits(x, c("data.frame", "sf")) &
      rlang::inherits_any(x, c("hill0", "hill1", "hill2"))
  )

  qD <- cellid <- Assemblage <- taxonKey <- obs <- NULL

  qval <- as.numeric(gsub("hill", "", match.arg(type)))
  temp_opts <- list(...)
  cutoff_length <- temp_opts$cutoff_length
  coverage <- temp_opts$coverage

  # Create list of incidence matrices by cell
  incidence_list <- x %>%
    dplyr::select(taxonKey, obs, cellid) %>%
    dplyr::group_by(cellid) %>%
    dplyr::group_split() %>%
    purrr::map(function(cell_data) {
      cell_data %>%
        dplyr::distinct(taxonKey, .keep_all = TRUE) %>%
        dplyr::select(taxonKey, obs) %>%
        tibble::deframe() %>%
        as.matrix()
    })

  # Name the list elements by cellid
  names(incidence_list) <- unique(x$cellid)

  # Filter cells based on cutoff length (number of species >= cutoff)
  incidence_list_filtered <- purrr::keep(incidence_list,
                                         function(matrix) {
    if (!is.null(matrix) && is.matrix(matrix)) {
      return(nrow(matrix) >= cutoff_length)
    }
    return(FALSE)
  })

  # Ensure presence-absence (assuming obs > 0 is presence) and
  # convert to numeric
  incidence_list_processed <- lapply(incidence_list_filtered,
                                     function(matrix) {
    numeric_matrix <- matrix(as.numeric(matrix),
                             nrow = nrow(matrix),
                             dimnames = dimnames(matrix))
    numeric_matrix[numeric_matrix > 0] <- 1
    return(numeric_matrix)
  })

  # Compute Hill diversity using a wrapper for iNEXT::estimateD
  diversity_estimates <- incidence_list_processed %>%
    my_estimateD(datatype = "incidence_raw",
                 base = "coverage",
                 level = coverage,
                 q = qval)

  # Extract and format the results
  indicator <- diversity_estimates %>%
    dplyr::select(Assemblage, qD, t) %>%
    dplyr::rename(cellid = Assemblage,
                  diversity_val = qD,
                  samp_size_est = t) %>%
    dplyr::mutate(cellid = as.integer(cellid), .keep = "unused")

  return(indicator)
}

#' @export
#' @rdname calc_map
calc_map.obs_richness <- function(x, ...) {

  stopifnot_error(
    paste0(
      "Wrong data class. This is an internal function and is not meant to be ",
      "called directly."
    ),
    inherits(x, "obs_richness"))

  taxonKey <- NULL

  # Calculate observed species richness over the grid
  indicator <-
    x %>%
    dplyr::summarize(diversity_val = sum(dplyr::n_distinct(taxonKey)),
                     .by = "cellid")

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.total_occ <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "total_occ"))

  obs <- NULL

  # Calculate total number of occurrences over the grid
  indicator <-
    x %>%
    dplyr::summarize(diversity_val = sum(obs, na.rm = TRUE),
                     .by = "cellid")

  return(indicator)

}

#' @param newness_min_year If set, only shows values above this (e.g. 1970). Values
#'    below the minimum will be replaced with NA. This can be useful e.g. if you have
#'    outlier cells where the data is very old causing the legend gradient to stretch
#'    in a way that makes other cell values difficult to discern.
#' @export
#' @rdname calc_map
calc_map.newness <- function(x,
                             newness_min_year = NULL,
                             ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "newness"))

  year <- NULL

  # Calculate mean year of occurrence over the grid
  indicator <-
    x %>%
    dplyr::summarize(diversity_val = round(mean(year, na.rm = TRUE)),
                     .by = "cellid")

  if (!is.null(newness_min_year)) {
    indicator$diversity_val <- ifelse(indicator$diversity_val > newness_min_year,
                                           indicator$diversity_val,
                                           NA)
  }

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.occ_density <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "occ_density"))

  diversity_val <- obs <- area <- cellid <- NULL

  cell_size_units <- stringr::str_extract(x$resolution[1], "(?<=[0-9,.]{1,6})[a-z]*$")

  stopifnot_error(
    paste0(
      "To calculate occurrence density, please choose a projected CRS that ",
      "uses meters or kilometers, not degrees."), "area" %in% names(x))

  # Calculate density of occurrences over the grid (per square km)
  indicator <-
    x %>%
    dplyr::reframe(diversity_val = sum(obs, na.rm = TRUE) / area,
                   .by = "cellid") %>%
    dplyr::distinct(cellid, diversity_val) %>%
    dplyr::mutate(diversity_val = as.numeric(diversity_val))

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.williams_evenness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "williams_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_map.evenness_core(x = x,
                                      type = "williams_evenness",
                                      ...)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.pielou_evenness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "pielou_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_map.evenness_core(x = x,
                                      type = "pielou_evenness",
                                      ...)

  return(indicator)

}

#' @noRd
calc_map.evenness_core <- function(x,
                                   type,
                                   ...) {

  stopifnot_error(
    paste0(
      "Please check the class and structure of your data. This is an ",
      "internal function, not meant to be called directly."
    ),
    inherits(x, c("data.frame", "sf")))

  available_indicators <- NULL; rm(available_indicators)

  num_occ <- obs <- cellid <- taxonKey <- . <- NULL

  type <- match.arg(type,
                    names(available_indicators))

  if (nrow(x) == 0) {
    return(tibble::tibble(cellid = integer(0), diversity_val = numeric(0)))
  }

  # Calculate adjusted evenness fo r each grid cell
  indicator <-
    x %>%
    dplyr::summarize(num_occ = sum(obs, na.rm = TRUE),
                     .by = c(cellid, taxonKey)) %>%
    dplyr::arrange(cellid) %>%
    tidyr::pivot_wider(names_from = cellid,
                       values_from = num_occ) %>%
    replace(is.na(.), 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list() %>%
    purrr::map(~compute_evenness_formula(., type)) %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(diversity_val = ".") %>%
    tibble::rownames_to_column(var = "cellid") %>%
    dplyr::mutate(cellid = as.integer(cellid),
                  .keep = "unused")

}

#' @export
#' @rdname calc_map
calc_map.ab_rarity <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "ab_rarity"))

  obs <- taxonKey <- cellid <- records_taxon <- obs_taxon <- rarity <- NULL
  obs_cell <- NULL

  # Select relevant columns
  x <- x %>%
    dplyr::select(cellid, taxonKey, obs)

  # Remove invalid rows
  x <- x[stats::complete.cases(x), ]

# Calculate total summed rarity (in terms of abundance) for each grid cell
  indicator <-
    x %>%
    # calculate number of records for each species
    dplyr::summarise(obs_taxon = sum(obs, na.rm = TRUE),
                     .by = c(cellid, taxonKey)) %>%
    # calculate number of records for each grid cell
    dplyr::mutate(obs_cell = sum(obs_taxon, na.rm = TRUE),
                  .by = cellid) %>%
    # calculate rarity for each species
    dplyr::mutate(rarity = 1 / (obs_taxon / obs_cell)) %>%
    # calculate total rarity for each grid cell
    dplyr::summarise(diversity_val = sum(rarity), .by = cellid) %>%
    # arrange by cellid
    dplyr::arrange(cellid)

}

#' @export
#' @rdname calc_map
calc_map.area_rarity <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "area_rarity"))

  rec_tax_cell <- cellid <- taxonKey <- rarity <- NULL
  occ_by_tax <- total_cells <- NULL

  # Select relevant columns
  x <- x %>%
    select(cellid, taxonKey)

  # Remove invalid rows
  x <- x[stats::complete.cases(x), ]

  # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
  # frequency for each species
  indicator <-
    x %>%
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
    dplyr::summarise(diversity_val = sum(rarity), .by = cellid) %>%
    # arrange by cellid
    dplyr::arrange(cellid)

}

#' @export
#' @rdname calc_map
calc_map.spec_occ <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "spec_occ"))

  diversity_val <- obs <- taxonKey <- cellid <- scientificName <- NULL

  # Calculate total occurrences for each species by grid cell
  indicator <-
    x %>%
    dplyr::mutate(diversity_val = sum(obs), .by = c(taxonKey, cellid)) %>%
    dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, taxonKey, scientificName, diversity_val)

}

#' @export
#' @rdname calc_map
calc_map.spec_range <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "spec_range"))

  cellid <- taxonKey <- scientificName <- diversity_val <- NULL

  # Flatten occurrences for each species by grid cell
  indicator <-
    x %>%
    dplyr::mutate(diversity_val = 1) %>%
    dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, taxonKey, scientificName, diversity_val)

}

#' @export
#' @rdname calc_map
calc_map.tax_distinct <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "tax_distinct"))

  cellid <- . <- diversity_val <- NULL

  # Early check for empty input
  if (nrow(x) == 0) {
    return(tibble::tibble(cellid = character(),
                          diversity_val = numeric()))
  }

  if (requireNamespace("taxize", quietly = TRUE)) {

    # Retrieve taxonomic data from GBIF
    tax_hier <- my_classification(unique(x$scientificName),
                                       db = "gbif",
                                       ...)

  } else {

    stop("The 'taxize' package is required to calculate taxonomic distinctness.")

  }

  # Save data
  #  saveRDS(tax_hier, file = "taxonomic_hierarchy.RDS")

  #  tax_hier <- my_readRDS("taxonomic_hierarchy.RDS")

  # Calculate taxonomic distinctness
  indicator <-
    x %>%
    tibble::add_column(diversity_val = NA) %>%
    dplyr::group_split(cellid) %>%
    purrr::discard(~ nrow(.) == 0) %>%
    purrr::map(~{
                   dplyr::mutate(.,
                     diversity_val = compute_tax_distinct_formula(., tax_hier)
                   ) }
    ) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(.data$diversity_val != 0) %>%
    dplyr::distinct(cellid, diversity_val, .keep_all = TRUE) %>%
    dplyr::select(cellid, diversity_val)

  return(indicator)

}
