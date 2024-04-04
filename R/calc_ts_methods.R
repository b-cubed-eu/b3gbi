#' @export
calc_ts.default <- function(x, ...){

  warning(paste("calc_ts does not know how to handle object of class ",
                class(x),
                ". Please ensure you are not calling calc_ts directly on an object."))

}

#' @noRd
calc_ts.hill0 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill0"))

  indicator <- calc_ts.hill_core(x = x,
                                  type = "hill0",
                                  ...)

  return(indicator)

}

#' @noRd
calc_ts.hill1 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill1"))

  indicator <- calc_ts.hill_core(x = x,
                                  type = "hill1",
                                  ...)

  return(indicator)

}

#' @noRd
calc_ts.hill2 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill2"))

  indicator <- calc_ts.hill_core(x = x,
                                  type = "hill2",
                                  ...)

  return(indicator)
}

#' @noRd
calc_ts.hill_core <- function(x,
                               type = c("hill0, hill1, hill2"),
                               cutoff_length = 100,
                               coverage = 0.95,
                               ...)
{

  stopifnot_error("Please check the class and structure of your data.
                  This is an internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf", "hill0" | "hill1" | "hill2")))

  type <- match.arg(type)

  # Extract qvalue from hill diversity type
  qval <- as.numeric(gsub("hill", "", type))

  richness_by_year <-
    x %>%
    dplyr::summarise(obs_richness = dplyr::n_distinct(scientificName),
                     .by = "year")

  # Create list of occurrence matrices by year, with species as rows
  species_records_raw <-
    x %>%
    dplyr::group_by(year) %>%
    dplyr::group_split() %>%
    purrr::map(. %>%
                 dplyr::group_by(eea_cell_code,
                                 scientificName) %>% {. ->> temp1 } %>%
                 tidyr::pivot_wider(names_from = "scientificName",
                                    values_from = "obs") %>%
                 dplyr::ungroup() %>%
                 dplyr::select(-eea_cell_code,
                               -taxonKey,
                               -kingdom,
                               -rank,
                               -resolution,
                               -geometry) %>%
                 dplyr::select(-any_of(c("area_km2",
                                         "xcoord",
                                         "ycoord",
                                         "basisOfRecord",
                                         "datasetKey"))) %>%{. ->> temp2 } %>%
                 replace(is.na(.), 0) %>%
                 dplyr::mutate_if(is.numeric,
                                  as.integer) %>%
                 dplyr::select(-year,
                               -cellid) %>%
                 tibble::rownames_to_column() %>%
                 tidyr::gather(variable,
                               value,
                               -rowname) %>%
                 tidyr::spread(rowname, value) %>%{. ->> temp4 } %>%
                 'row.names<-'(., NULL) %>%
                 tibble::column_to_rownames(var = "variable") %>%
                 as.matrix() %>%
                 ifelse(. > 1, 1, .))

  # name list elements
  names(species_records_raw) <- richness_by_year$year

  # remove all years with too little data to avoid errors from iNEXT
  species_records_raw2 <- species_records_raw %>%
    keep(., function(x) length(x) > cutoff_length)

  # Calculate diversity estimates
  #  coverage_rare <- species_records_raw %>%
  #   iNEXT::iNEXT(endpoint=inext_sampsize, datatype="incidence_raw", q=qval)

  coverage_rare <- species_records_raw2 %>%
    iNEXT::estimateD(base = "coverage", level = coverage, datatype="incidence_raw", q=qval)

  # Extract estimated relative species richness
  est_richness <-
    coverage_rare %>%
    #  coverage_rare$iNextEst$coverage_based %>%
    #  dplyr::filter(abs(SC-coverage) == min(abs(SC-coverage)),
    #                .by = Assemblage) %>%
    dplyr::select(Assemblage, qD, t, SC, Order.q) %>%
    dplyr::rename(year = Assemblage,
                  est_relative_richness = qD,
                  samp_size_est = t,
                  coverage = SC,
                  diversity_type = Order.q)

  # Calculate estimated relative richness as an index
  est_richness <-
    est_richness %>%
    dplyr::mutate(index = est_relative_richness/lag(est_relative_richness)) %>%
    replace(is.na(.), 1) %>%
    dplyr::mutate(index = cumprod(index))

  # remove rows which are not present in estimated richness
  richness_by_year <- richness_by_year[richness_by_year$year %in% est_richness$year,]


  # Add observed richness and total records to df
  indicator <-
    richness_by_year %>%
    tibble::add_column(est_relative_richness = est_richness$est_relative_richness,
                       .after = "obs_richness") %>%
    tibble::add_column(diversity_val = est_richness$index,
                       .after = "est_relative_richness") %>%
    tibble::as_tibble()


}

#' @export
#' @rdname calc_ts
calc_ts.obs_richness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "obs_richness"))

  # Calculate observed species richness by year
  x <-
    x %>%
    dplyr::summarise(diversity_val = dplyr::n_distinct(scientificName),
                              .by = "year") %>%
    dplyr::arrange(year)

}

#' @export
#' @rdname calc_ts
calc_ts.cum_richness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "cum_richness"))

  # Calculate the cumulative number of unique species observed
  indicator <-
    x %>%
    dplyr::select(year, taxonKey) %>%
    dplyr::distinct(taxonKey, .keep_all = TRUE) %>%
    dplyr::summarize(unique_by_year = length(unique(taxonKey)),
                     .by = year) %>%
    dplyr::arrange(year) %>%
    dplyr::reframe(year = year,
                   diversity_val = cumsum(unique_by_year))

}

#' @export
#' @rdname calc_ts
calc_ts.total_occ <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "total_occ"))

  # Calculate total number of occurrences over the grid
  indicator <-
    x %>%
    dplyr::summarize(diversity_val = sum(obs),
                     .by = "year")

}

#' @export
#' @rdname calc_ts
calc_ts.occ_density <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "occ_density"))

  # Calculate density of occurrences over the grid (per square km)
  indicator <-
    x %>%
    dplyr::reframe(diversity_val = sum(obs) / area_km2,
                   .by = c("year", "cellid")) %>%
    dplyr::reframe(diversity_val = mean(diversity_val), .by = "year") %>%
    dplyr::mutate(diversity_val = as.numeric(diversity_val)) %>%
    dplyr::arrange(year)

}

#' @export
#' @rdname calc_ts
calc_ts.newness <- function(x,
                             ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not meant to be called directly.",
                  inherits(x, "newness"))

  yearvals <- vector()
  counter <- 1
  for (i in unique(x$year)) {
    yearvals[counter]  <- round(mean(x$year[x$year <= i]))
    counter <- counter + 1
  }

  indicator <- data.frame("year" = unique(x$year),
                          "diversity_val" = yearvals)

  # # Calculate mean year of occurrence over the grid
  # indicator <-
  #   x %>%
  #   dplyr::summarize(diversity_val = mean(year),
  #                    .by = c("year", "cellid")) %>%
  #   dplyr::summarize(diversity_val = round(mean(year)),
  #                    .by = c("year")) %>%
  #   dplyr::arrange(year)

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.williams_evenness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "williams_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_ts.evenness_core(x = x,
                                      type = "williams_evenness",
                                      ...)

}

#' @export
#' @rdname calc_ts
calc_ts.pielou_evenness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "pielou_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_ts.evenness_core(x = x,
                                      type = "pielou_evenness",
                                      ...)

}

#' @noRd
calc_ts.evenness_core <- function(x,
                                   type,
                                   ...) {

  stopifnot_error("Please check the class and structure of your data.
                  This is an internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")))


  type <- match.arg(type,
                    names(available_indicators))

  # Calculate number of records for each species by grid cell
  indicator <-
    x %>%
    dplyr::summarize(num_occ = sum(obs),
                     .by = c(year, taxonKey)) %>%
    dplyr::arrange(year) %>%
    tidyr::pivot_wider(names_from = year,
                       values_from = num_occ) %>%
    replace(is.na(.), 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list() %>%
    purrr::map(~compute_evenness_formula(. ,type)) %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(diversity_val = ".") %>%
    tibble::rownames_to_column(var = "year") %>%
    dplyr::mutate(year = as.integer(year),
                  .keep = "unused")

}

#' @export
#' @rdname calc_ts
calc_ts.ab_rarity <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "ab_rarity"))

  # Calculate total summed rarity (in terms of abundance) for each grid cell
  indicator <-
    x %>%
    dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
    dplyr::mutate(rarity = 1 / (records_taxon / sum(obs))) %>%
    dplyr::summarise(diversity_val = sum(rarity), .by = c("year", "cellid")) %>%
    dplyr::summarise(diversity_val = sum(diversity_val), .by = "year") %>%
    dplyr::arrange(year)

}

#' @export
#' @rdname calc_ts
calc_ts.area_rarity <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "area_rarity"))

  # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
  # frequency for each species
  indicator <-
    x %>%
    dplyr::mutate(rec_tax_cell = sum(dplyr::n_distinct(cellid)),
                  .by = c(taxonKey)) %>%
    dplyr::mutate(rarity = 1 / (rec_tax_cell / sum(dplyr::n_distinct(cellid)))) %>%
    dplyr::summarise(diversity_val = sum(rarity), .by = c("year", "cellid")) %>%
    dplyr::summarise(diversity_val = mean(diversity_val), .by = "year") %>%
    dplyr::arrange(year)

}

#' @noRd
calc_ts.spec_occ <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_occ"))

  # Calculate total occurrences for each species by grid cell
  indicator <-
    x %>%
    dplyr::mutate(num_records = sum(obs), .by = c(taxonKey, cellid)) %>%
    dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, taxonKey, scientificName, num_records)

}

#' @noRd
calc_ts.spec_range <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_range"))

  # Flatten occurrences for each species by grid cell
  indicator <-
    x %>%
    dplyr::mutate(obs = 1) %>%
    dplyr::distinct(cellid, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(cellid) %>%
    dplyr::select(cellid, taxonKey, scientificName, obs)

}

#' @noRd
calc_ts.tax_distinct <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "tax_distinct"))

  # Retrieve taxonomic data from GBIF
  tax_hier <- taxize::classification(unique(x$scientificName), db = "gbif", return_id = TRUE, accepted = TRUE)

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
                                 calc_tax_distinctness(.,
                                                       tax_hier))) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(cellid, diversity_val, .keep_all = TRUE) %>%
    dplyr::select(cellid, diversity_val)

}
