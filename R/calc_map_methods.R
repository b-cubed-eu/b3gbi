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
                               ...)
{

  stopifnot_error("Please check the class and structure of your data. This is an internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")) & rlang::inherits_any(x, c("hill0", "hill1", "hill2")))

  type <- match.arg(type)

  # Extract qvalue from hill diversity type
  qval <- as.numeric(gsub("hill", "", type))

  # Create list of occurrence matrices by grid cell, with species as rows
  spec_rec_raw_cell <-
    x %>%
    dplyr::group_split(cellid) %>%
    purrr::map(. %>%
                 dplyr::group_by(taxonKey) %>%
                 tidyr::pivot_wider(names_from = taxonKey,
                                    values_from = obs) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-scientificName,
                               -kingdom,
                            #   -rank,
                               -geometry,
                               -resolution,
                               -xcoord,
                               -ycoord,
                               -year,
                               -area_km2) %>%
                 dplyr::select(-any_of(c("basisOfRecord",
                                         "datasetKey"))) %>%
                 replace(is.na(.), 0) %>%
                 dplyr::mutate_if(is.numeric,
                                  as.integer) %>%
                 dplyr::select(-cellid) %>%
                 tibble::rownames_to_column() %>%
                 tidyr::gather(variable,
                               value,
                               -rowname) %>%
                 tidyr::spread(rowname, value) %>%
                 'row.names<-'(., NULL) %>%
                 tibble::column_to_rownames(var = "variable") %>%
                 as.matrix() %>%
                 replace(. > 1, as.integer(1))
    )

  # name list elements
  names(spec_rec_raw_cell) <- unique(x$cellid)

  temp_opts <- list(...)

  cutoff_length <- temp_opts$cutoff_length

  coverage <- temp_opts$coverage

  # remove all cells with too little data to avoid errors from iNEXT
  spec_rec_raw_cell2 <-
    purrr::keep(spec_rec_raw_cell, function(x) {
      # Check if the element is a matrix or data frame
      if (!is.null(x) && (is.data.frame(x) || is.matrix(x))) {
        # Check if the number of columns is greater than or equal to the cutoff
        return(ncol(x) >= cutoff_length)
      } else {
        # Return FALSE for any list elements that are not appropriately structured
        return(FALSE)
      }
    })

  # Convert list elements to numeric presence-absence matrices
  spec_rec_raw_cell2 <- lapply(spec_rec_raw_cell2, function(x) {
    # Attempt to convert all elements to numeric, assuming proper encoding of presence-absence
    x <- apply(x, 2, as.numeric)

    # Ensure all presence are converted to 1 (assuming all non-zero are present as original message suggests setting them as 1)
    x[x != 0] <- 1

    return(x)
  })

  # Compute hill diversity
  coverage_rare_cell <- spec_rec_raw_cell2 %>%
    iNEXT::estimateD(datatype="incidence_raw",
                     base = "coverage",
                     level = coverage,
                     q=qval)

  # Extract estimated relative diversity
  indicator <-
    coverage_rare_cell %>%
    #coverage_rare_cell$iNextEst$coverage_based %>%
    #dplyr::filter(abs(SC-coverage) == min(abs(SC-coverage)),
    #              .by = Assemblage) %>%
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

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "obs_richness"))

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

  # Calculate total number of occurrences over the grid
  indicator <-
    x %>%
    dplyr::summarize(diversity_val = sum(obs),
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

  # Calculate mean year of occurrence over the grid
  indicator <-
    x %>%
    dplyr::summarize(diversity_val = round(mean(year)),
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

  # Calculate density of occurrences over the grid (per square km)
  indicator <-
    x %>%
    dplyr::reframe(diversity_val = sum(obs) / area_km2,
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

  stopifnot_error("Please check the class and structure of your data. This is an internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")))

  type <- match.arg(type,
                    names(available_indicators))

  # Calculate adjusted evenness fo r each grid cell
  indicator <-
    x %>%
    dplyr::summarize(num_occ = sum(obs),
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

# Calculate total summed rarity (in terms of abundance) for each grid cell
indicator <-
  x %>%
  dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
  dplyr::mutate(rarity = 1 / (records_taxon / sum(obs))) %>%
  dplyr::summarise(diversity_val = sum(rarity), .by = "cellid") %>%
  dplyr::arrange(cellid)

}

#' @export
#' @rdname calc_map
calc_map.area_rarity <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "area_rarity"))

  # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
  # frequency for each species
  indicator <-
    x %>%
    dplyr::mutate(rec_tax_cell = sum(dplyr::n_distinct(cellid)),
                  .by = c(taxonKey)) %>%
    dplyr::mutate(rarity = 1 / (rec_tax_cell / sum(dplyr::n_distinct(cellid)))) %>%
    dplyr::summarise(diversity_val = sum(rarity), .by = cellid)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.spec_occ <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "spec_occ"))

  # Calculate total occurrences for each species by grid cell
  indicator <-
    x %>%
    dplyr::mutate(diversity_val = sum(obs), .by = c(taxonKey, cellid)) %>%
    dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, taxonKey, scientificName, diversity_val)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.spec_range <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "spec_range"))

  # Flatten occurrences for each species by grid cell
  indicator <-
    x %>%
    dplyr::mutate(diversity_val = 1) %>%
    dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, taxonKey, scientificName, diversity_val)

  return(indicator)

}

#' @export
#' @rdname calc_map
calc_map.tax_distinct <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "tax_distinct"))

  # Retrieve taxonomic data from GBIF
  tax_hier <- taxize::classification(unique(x$scientificName),
                                     db = "gbif",
                                     ...)

  # Save data
  #  saveRDS(tax_hier, file = "taxonomic_hierarchy.RDS")

  #  tax_hier <- readRDS("taxonomic_hierarchy.RDS")

  # Calculate taxonomic distinctness
  indicator <-
    x %>%
    tibble::add_column(diversity_val = NA) %>%
    dplyr::group_split(cellid) %>%
    purrr::map(. %>%
                 dplyr::mutate(diversity_val =
                                 compute_tax_distinct_formula(.,
                                                              tax_hier))) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(cellid, diversity_val, .keep_all = TRUE) %>%
    dplyr::select(cellid, diversity_val)

  return(indicator)

}
