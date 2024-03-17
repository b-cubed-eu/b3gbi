#' @export
calc_map.default <- function(data, ...) {
  NextMethod()
}

#' @noRd
calc_map.hill0 <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "hill0"))

  indicator <- calc_map.hill_core(data = data,
                                  type = "hill0",
                                  ...)

  return(indicator)

}

#' @noRd
calc_map.hill1 <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "hill1"))

  indicator <- calc_map.hill_core(data = data,
                                  type = "hill1",
                                  ...)

  return(indicator)

}

#' @noRd
calc_map.hill2 <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "hill2"))

  indicator <- calc_map.hill_core(data = data,
                                  type = "hill2",
                                  ...)

  return(indicator)
}

#' @noRd
calc_map.hill_core <- function(data,
                               type = c("hill0, hill1, hill2"),
                               cutoff_length = 100,
                               coverage = 0.95,
                               ...)
{

  stopifnot_error("Please check the class and structure of your data.
                  This is an internal function, not meant to be called directly.",
                  inherits(data, c("data.frame", "sf", "hill0" | "hill1" | "hill2")))

  type <- match.arg(type)

  # Extract qvalue from hill diversity type
  qval <- as.numeric(gsub("hill", "", type))

  # Create list of occurrence matrices by grid cell, with species as rows
  spec_rec_raw_cell <-
    data %>%
    dplyr::group_split(cellid) %>%
    purrr::map(. %>%
                 dplyr::group_by(eea_cell_code,
                                 taxonKey) %>%
                 tidyr::pivot_wider(names_from = taxonKey,
                                    values_from = obs) %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-eea_cell_code,
                               -scientificName,
                               -kingdom,
                               -rank,
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
  names(spec_rec_raw_cell) <- unique(data$cellid)

  # remove all cells with too little data to avoid errors from iNEXT
  spec_rec_raw_cell2 <- spec_rec_raw_cell %>%
    keep(., function(x) length(x) > cutoff_length)

  # Compute hill diversity
  coverage_rare_cell <- spec_rec_raw_cell2 %>%
    iNEXT::estimateD(datatype="incidence_raw",
                     base = "coverage",
                     level = coverage,
                     q=qval,
                     ...)

  # Extract estimated relative diversity
  indicator <-
    coverage_rare_cell %>%
    #coverage_rare_cell$iNextEst$coverage_based %>%
    #dplyr::filter(abs(SC-coverage) == min(abs(SC-coverage)),
    #              .by = Assemblage) %>%
    dplyr::select(Assemblage, qD, t, SC, Order.q) %>%
    dplyr::rename(cellid = Assemblage,
                  diversity_val = qD,
                  samp_size_est = t,
                  coverage = SC,
                  diversity_type = Order.q) %>%
    dplyr::mutate(cellid = as.integer(cellid), .keep = "unused")

  return(indicator)

}

#' @noRd
calc_map.obs_richness <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "obs_richness"))

  # Calculate observed species richness over the grid
  indicator <-
    data %>%
    dplyr::summarize(diversity_val = sum(dplyr::n_distinct(taxonKey)),
                     .by = "cellid")

  return(indicator)

}

#' @noRd
calc_map.total_occ <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "total_occ"))

  # Calculate total number of occurrences over the grid
  indicator <-
    data %>%
    dplyr::summarize(diversity_val = sum(obs),
                     .by = "cellid")

  return(indicator)

}

#' @noRd
calc_map.newness <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "newness"))

  # Calculate mean year of occurrence over the grid
  indicator <-
    data %>%
    dplyr::summarize(diversity_val = round(mean(year)),
                     .by = "cellid")

  if (!is.null(newness_min_year)) {
    indicator$diversity_val <- ifelse(indicator$diversity_val > newness_min_year,
                                           indicator$diversity_val,
                                           NA)
  }

  return(indicator)

}

#' @noRd
calc_map.occ_density <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "occ_density"))

  # Calculate density of occurrences over the grid (per square km)
  indicator <-
    data %>%
    dplyr::reframe(diversity_val = sum(obs) / area_km2,
                   .by = "cellid") %>%
    dplyr::distinct(cellid, diversity_val) %>%
    dplyr::mutate(diversity_val = as.numeric(diversity_val))

  return(indicator)

}

#' @noRd
calc_map.williams_evenness <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "williams_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_map.evenness_core(data = data,
                                      type = "williams_evenness",
                                      ...)

  return(indicator)

}

#' @noRd
calc_map.pielou_evenness <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "pielou_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_map.evenness_core(data = data,
                                      type = "pielou_evenness",
                                      ...)

  return(indicator)

}

#' @noRd
calc_map.evenness_core <- function(data,
                                   type,
                                   ...) {

  stopifnot_error("Please check the class and structure of your data.
                  This is an internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf", "williams_evenness" | "pielou_evenness")))


  type <- match.arg(type,
                    c(available_indicators$indicator_class))

  # Calculate adjusted evenness fo r each grid cell
  indicator <-
    data %>%
    dplyr::summarize(num_occ = sum(obs),
                     .by = c(cellid, taxonKey)) %>%
    dplyr::arrange(cellid) %>%
    tidyr::pivot_wider(names_from = cellid,
                       values_from = num_occ) %>%
    replace(is.na(.), 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    attr("class") <- type %>%
    purrr::map(~compute_formula(.)) %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(diversity_val = ".") %>%
    tibble::rownames_to_column(var = "cellid") %>%
    dplyr::mutate(cellid = as.integer(cellid),
                  .keep = "unused")

  return(indicator)

}

#' @noRd
calc_map.ab_rarity <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "ab_rarity"))

# Calculate total summed rarity (in terms of abundance) for each grid cell
indicator <-
  data %>%
  dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
  dplyr::mutate(rarity = 1 / (records_taxon / sum(obs))) %>%
  dplyr::summarise(diversity_val = sum(rarity), .by = "cellid") %>%
  dplyr::arrange(cellid)

}

#' @noRd
calc_map.area_rarity <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "area_rarity"))

  # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
  # frequency for each species
  indicator <-
    data %>%
    dplyr::mutate(rec_tax_cell = sum(dplyr::n_distinct(cellid)),
                  .by = c(taxonKey)) %>%
    dplyr::mutate(rarity = 1 / (rec_tax_cell / sum(dplyr::n_distinct(cellid)))) %>%
    dplyr::summarise(diversity_val = sum(rarity), .by = cellid)

  return(indicator)

}

#' @noRd
calc_map.spec_occ <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "spec_occ"))

  # Calculate total occurrences for each species by grid cell
  diversity_cell <-
    data %>%
    dplyr::mutate(num_records = sum(obs), .by = c(taxonKey, cellid)) %>%
    dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, taxonKey, scientificName, num_records)

  return(indicator)

}

#' @noRd
calc_map.spec_range <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "spec_range"))

  # Flatten occurrences for each species by grid cell
  indicator <-
    data %>%
    dplyr::mutate(obs = 1) %>%
    dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, taxonKey, scientificName, obs)

  return(indicator)

}

#' @noRd
calc_map.tax_distinct <- function(data, ...) {

  stopifnot.error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(data, "tax_distinct"))

  # Retrieve taxonomic data from GBIF
  tax_hier <- taxize::classification(unique(data$scientificName), db = "gbif", return_id = TRUE, accepted = TRUE)

  # Save data
  #  saveRDS(tax_hier, file = "taxonomic_hierarchy.RDS")

  #  tax_hier <- readRDS("taxonomic_hierarchy.RDS")

  # Calculate taxonomic distinctness
  indicator <-
    data %>%
    tibble::add_column(diversity_val = NA) %>%
    dplyr::group_split(cellid) %>%
    purrr::map(. %>%
                 dplyr::mutate(diversity_val =
                                 calc_tax_distinctness(.,
                                                       tax_hier))) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(cellid, diversity_val, .keep_all = TRUE) %>%
    dplyr::select(cellid, diversity_val)

  return(indicator)

}
