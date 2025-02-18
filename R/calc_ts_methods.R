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

#' @param type Choose which Hill number, or q, to calculate. Choose 'hill0' (q = 0)
#' for estimated species richness, 'hill1' for Hill-Shannon diversity, or 'hill2'
#' for Hill-Simpson diversity.
#'
#' @importFrom iNEXT estimateD
#'
#' @noRd
calc_ts.hill_core <- function(x,
                              type = c("hill0", "hill1", "hill2"),
                              ...)
{

  stopifnot_error("Please check the class and structure of your data.
                  This is an internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")) & rlang::inherits_any(x, c("hill0", "hill1", "hill2")))

  scientificName <- year <- obs <- cellCode <- . <- variable <- value <- NULL
  rowname <- Assemblage <- qD <- SC <- Order.q <- qD.LCL <- qD.UCL <- NULL

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
    dplyr::select(year, scientificName, obs, cellCode) %>%
    dplyr::group_by(year) %>%
    dplyr::group_split() %>%
    purrr::map(. %>%
                 dplyr::group_by(scientificName) %>%
                 tidyr::pivot_wider(names_from = "scientificName",
                                    values_from = "obs") %>%
                 dplyr::ungroup() %>%
                 replace(is.na(.), 0) %>%
                 dplyr::mutate_if(is.numeric,
                                  as.integer) %>%
                 dplyr::select(-year, -cellCode) %>%
                 tibble::rownames_to_column() %>%
                 tidyr::gather(variable,
                               value,
                               -rowname) %>%
                 tidyr::spread(rowname, value) %>%
                 'row.names<-'(., NULL) %>%
                 tibble::column_to_rownames(var = "variable") %>%
                 as.matrix() %>%
                 ifelse(. > 1, 1, .))

  # name list elements
  names(species_records_raw) <- richness_by_year$year


  temp_opts <- list(...)

  cutoff_length <- temp_opts$cutoff_length

  coverage <- temp_opts$coverage

  # remove all years with too little data to avoid errors from iNEXT
  species_records_raw2 <- species_records_raw %>%
    purrr::keep(., function(x) length(x) > cutoff_length)


  coverage_rare <- species_records_raw2 %>%
    iNEXT::estimateD(base = "coverage", level = coverage, datatype="incidence_raw", q=qval)

  # Extract estimated relative species richness
  indicator <-
    coverage_rare %>%
    dplyr::select(Assemblage, qD, t, SC, Order.q, qD.LCL, qD.UCL) %>%
    dplyr::rename(year = Assemblage,
                  diversity_val = qD,
                  samp_size_est = t,
                  coverage = SC,
                  diversity_type = Order.q,
                  ll = qD.LCL,
                  ul = qD.UCL) %>%
    dplyr::mutate(year = as.numeric(year))

}


#' @export
#' @rdname calc_ts
calc_ts.obs_richness <- function(x,
                                 ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "obs_richness"))

  year <- taxonKey <- NULL

  x <-
    x %>%
    dplyr::select(year, taxonKey) %>%
    dplyr::arrange(year)

    # Calculate observed species richness by year
    indicator <-
      x %>%
      dplyr::summarise(diversity_val = dplyr::n_distinct(taxonKey),
                       .by = "year")

}

#' @export
#' @rdname calc_ts
calc_ts.cum_richness <- function(x,
                                 ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "cum_richness"))

  year <- taxonKey <- unique_by_year <- NULL

    # Calculate the cumulative number of unique species observed
    x <-
      x %>%
      dplyr::select(year, taxonKey) %>%
      dplyr::arrange(year) %>%
      dplyr::distinct(taxonKey, .keep_all = TRUE) %>%
      dplyr::summarize(unique_by_year = length(taxonKey),
                       .by = year)

    # Calculate the cumulative number of unique species observed
    indicator <-
      x %>%
      dplyr::reframe(year = year,
                     diversity_val = cumsum(unique_by_year))

}

#' @export
#' @rdname calc_ts
calc_ts.total_occ <- function(x,
                              ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "total_occ"))

  obs <- NULL
  # Calculate total number of occurrences over the grid
  indicator <-
    x %>%
    dplyr::summarize(diversity_val = sum(obs),
                     .by = "year")

}

#' @export
#' @rdname calc_ts
calc_ts.occ_density <- function(x,
                                ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "occ_density"))

  year <- cellid <- diversity_val <- obs <- area_km2 <- NULL

  # Calculate density of occurrences over the grid (per square km)
  indicator <-
    x %>%
    dplyr::arrange(year, cellid) %>%
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

}

#' @export
#' @rdname calc_ts
calc_ts.williams_evenness <- function(x,
                                      ...) {

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
calc_ts.pielou_evenness <- function(x,
                                    ...) {

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

  available_indicators <- NULL; rm(available_indicators)

  num_occ <- obs <- year <- taxonKey <- . <- NULL

   type <- match.arg(type,
                     names(available_indicators))

  # Calculate number of records for each species by grid cell
  x <-
    x %>%
    dplyr::summarize(num_occ = sum(obs),
                     .by = c(year, taxonKey)) %>%
    dplyr::arrange(year) %>%
    tidyr::pivot_wider(names_from = year,
                       values_from = num_occ) %>%
    replace(is.na(.), 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list()

  indicator <-
    x %>%
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
calc_ts.ab_rarity <- function(x,
                              ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "ab_rarity"))

  obs <- taxonKey <- records_taxon <- year <- rarity <- diversity_val <- NULL

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
calc_ts.area_rarity <- function(x,
                                ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "area_rarity"))

  year <- cellid <- taxonKey <- rec_tax_cell <- rarity <- diversity_val <- NULL

  # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
  # frequency for each species
  indicator <-
    x %>%
    dplyr::arrange(year, cellid, taxonKey) %>%
    dplyr::mutate(rec_tax_cell = sum(dplyr::n_distinct(cellid)),
                  .by = c(taxonKey)) %>%
    dplyr::mutate(rarity = 1 / (rec_tax_cell / sum(dplyr::n_distinct(cellid)))) %>%
    dplyr::summarise(diversity_val = sum(rarity), .by = c("year", "cellid")) %>%
    dplyr::summarise(diversity_val = mean(diversity_val), .by = "year") %>%
    dplyr::arrange(year)

}

#' @export
#' @rdname calc_ts
calc_ts.spec_occ <- function(x,
                             ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_occ"))

  year <- scientificName <- taxonKey <- obs <- diversity_val <- NULL

  # Calculate total occurrences for each species by grid cell
  indicator <-
    x %>%
    dplyr::mutate(diversity_val = sum(obs), .by = c(taxonKey, year)) %>%
    dplyr::distinct(year, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(year) %>%
    dplyr::select(year, taxonKey, scientificName, diversity_val)

}

#' @export
#' @rdname calc_ts
calc_ts.spec_range <- function(x,
                               ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_range"))

  year <- taxonKey <- cellCode <- obs <- diversity_val <- scientificName <- NULL

  x <-
    x %>%
    dplyr::arrange(taxonKey, year, cellCode)

  # Flatten occurrences for each species by grid cell
  indicator <-
    x %>%
    dplyr::mutate(diversity_val = sum(obs >= 1), .by = c(taxonKey, year)) %>%
    dplyr::distinct(year, scientificName, .keep_all = TRUE) %>%
    dplyr::arrange(taxonKey) %>%
    dplyr::select(year, taxonKey, scientificName, diversity_val)

}

#' @param set_rows Automatically select which taxonomic information to keep when
#'    there are multiple options. Default value of 1 keeps the first option, which
#'    is usually the best.
#'
#' @export
#' @rdname calc_ts
calc_ts.tax_distinct <- function(x,
                                 set_rows = 1,
                                 ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "tax_distinct"))

  year <- . <- diversity_val <- NULL

  if (!requireNamespace("taxize", quietly = TRUE)) {
    stop("Please install the taxize package to use this function.")
  }

  # Retrieve taxonomic data from GBIF
  tax_hier <- taxize::classification(unique(x$scientificName),
                                     db = "gbif",
                                     ...)

  # Save data for use when calculating bootstraps
  saveRDS(tax_hier, file = "taxonomic_hierarchy.RDS")

  # tax_hier <- readRDS("taxonomic_hierarchy.RDS")

  # Calculate taxonomic distinctness
  indicator <-
    x %>%
    tibble::add_column(diversity_val = NA) %>%
    dplyr::group_split(year) %>%
    purrr::map(. %>%
                 dplyr::mutate(diversity_val =
                                 compute_tax_distinct_formula(.,
                                                              tax_hier))) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(year, diversity_val, .keep_all = TRUE) %>%
    dplyr::select(year, diversity_val)

}


#' @export
#' @rdname calc_ts
calc_ts.occ_turnover <- function(x,
                                 ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "occ_turnover"))

  year <- NULL

  x <- x %>%
    dplyr::arrange(year)

  # Determine the species present each year
  ind_list <- list_org_by_year(x, "taxonKey")

  # Determine the new species added each year
  tax_added <- list()
  tax_added[[1]] <- ind_list[[1]]
  tax_added[2:length(ind_list)] <-
    lapply(2:length(unique(x$year)), function(y){
      a <- setdiff(ind_list[[y]], ind_list[[y-1]])
      return(a)
    })

  # Determine the species lost each year
  tax_lost <- list()
  tax_lost[[1]] <- NULL
  tax_lost[2:length(ind_list)] <-
    lapply(2:length(unique(x$year)), function(y){
      a <- setdiff(ind_list[[y-1]], ind_list[[y]])
    })

  # Combine the species present in the current with those present in the previous year
  tax_present <- list()
  tax_present[[1]] <- ind_list[[1]]
  tax_present[2:length(ind_list)] <-
    lapply(2:length(unique(x$year)), function(y){
      a <- intersect(ind_list[[y-1]], ind_list[[y]])
    })

  # Calculate occupancy turnover as the sum of the number of species added and the
  # number of species lost divided by the total number of species present in the current
  # and previous year combined
  occ_turnover <- sapply(1:length(unique(x$year)), function(y){
    a <- (length(tax_added[[y]]) + length(tax_lost[[y]])) /
      (length(tax_present[[y]]) + length(tax_added[[y]]) + length(tax_lost[[y]]))
  })
  occ_turnover[[1]] <- NA

  indicator <- tibble::tibble(year = unique(x$year), diversity_val = occ_turnover)

}
