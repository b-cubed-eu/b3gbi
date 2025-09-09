#' @export
#' @rdname calc_ts
calc_ts.default <- function(x, ...){

  warning(paste0(
      "calc_ts does not know how to handle object of class ", class(x),
      ". Please ensure you are not calling calc_ts directly on an object."
    ))

}

#' @export
#' @rdname calc_ts
calc_ts.hill0 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill0"))

  indicator <- calc_ts_hill_core(x = x, type = "hill0", ...)

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.hill1 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill1"))

  indicator <- calc_ts_hill_core(x = x, type = "hill1", ...)

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.hill2 <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "hill2"))

  indicator <- calc_ts_hill_core(x = x, type = "hill2", ...)

  return(indicator)

}

#' @param type (Optional) Choose which Hill number, or q, to calculate. Choose
#'  'hill0' (q = 0) for estimated species richness, 'hill1' for Hill-Shannon
#'  diversity, or 'hill2' for Hill-Simpson diversity.
#' @param ... Additional arguments passed to iNEXT::estimateD(e.g.,nboot, conf).
#' @importFrom iNEXT estimateD
#' @noRd
calc_ts_hill_core <- function(x, type = c("hill0", "hill1", "hill2"), ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")) &
                    rlang::inherits_any(x, c("hill0", "hill1", "hill2")))

  scientificName <- year <- obs <- cellCode <- variable <- value <- NULL
  rowname <- Assemblage <- qD <- SC <- Order.q <- qD.LCL <- qD.UCL <- NULL

  type <- match.arg(type)

  # Extract qvalue from hill diversity type
  qval <- as.numeric(gsub("hill", "", type))

  richness_by_year <- x %>%
    dplyr::summarise(obs_richness = dplyr::n_distinct(scientificName),
                     .by = "year")

  species_records_raw <- x %>%
    dplyr::select(year, scientificName, obs, cellCode) %>%
    dplyr::group_by(year) %>%
    dplyr::group_split() %>%
    # Process each year's data frame
    purrr::map(function(df) {
      m <- df %>%
        # Pivot wider to get species as columns and `obs` as values
        tidyr::pivot_wider(
          names_from = "scientificName",
          values_from = "obs",
          values_fn = sum,
          values_fill = 0
        ) %>%
        # Remove the `year` and `cellCode` columns which are no longer needed
        dplyr::select(-year, -cellCode) %>%
        # Transpose the data frame efficiently using `t()`
        # We must first convert the data to a matrix before transposing
        as.matrix() %>%
        t()
      # Convert any values greater than 1 to 1 for presence-absence
      m[m > 1] <- 1
      return(m)
    })

  # name list elements
  names(species_records_raw) <- richness_by_year$year

  temp_opts <- list(...)
  cutoff_length <- temp_opts$cutoff_length
  coverage <- temp_opts$coverage
  conf <- temp_opts$conf
  nboot <- temp_opts$nboot

  # remove all years with too little data to avoid errors from iNEXT
  species_records_raw2 <- species_records_raw %>%
    purrr::keep(., function(x) length(x) > cutoff_length)

  coverage_rare <- species_records_raw2 %>%
    my_estimateD(base = "coverage",
                 level = coverage,
                 datatype="incidence_raw",
                 q=qval,
                 conf=conf,
                 nboot=nboot)

  # Extract estimated relative species richness
  indicator <- coverage_rare %>%
    dplyr::select(Assemblage, qD, t, SC, Order.q, qD.LCL, qD.UCL) %>%
    dplyr::rename(year = Assemblage,
                  diversity_val = qD,
                  samp_size_est = t,
                  coverage = SC,
                  diversity_type = Order.q,
                  ll = qD.LCL,
                  ul = qD.UCL) %>%
    dplyr::mutate(year = as.numeric(year))

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.obs_richness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "obs_richness"))

  year <- taxonKey <- NULL

  # Calculate observed species richness by year
  indicator <- x %>%
    dplyr::select(year, taxonKey) %>%
    dplyr::summarise(diversity_val = dplyr::n_distinct(taxonKey),
                     .by = "year") %>%
    dplyr::arrange(year)

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.cum_richness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "cum_richness"))

  year <- taxonKey <- unique_by_year <- NULL

    # Calculate the cumulative number of unique species observed
    indicator <- x %>%
      dplyr::select(year, taxonKey) %>%
      dplyr::distinct(taxonKey, .keep_all = TRUE) %>%
      dplyr::summarize(unique_by_year = dplyr::n_distinct(taxonKey),
                       .by = year) %>%
      dplyr::arrange(year) %>%
      dplyr::reframe(year = year, diversity_val = cumsum(unique_by_year))

    return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.total_occ <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "total_occ"))

  obs <- NULL

  # Calculate total number of occurrences over the grid
  indicator <- x %>%
    dplyr::summarize(diversity_val = sum(obs), .by = "year")

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.occ_density <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "occ_density"))

  year <- diversity_val <- obs <- area <- NULL

  # cell_size_units <- stringr::str_extract(x$resolution[1],
  #                                         "(?<=[0-9,.]{1,6})[a-z]*$")
  #
  # stopifnot_error("To calculate occurrence density, please choose a projected
  #                 CRS that uses meters or kilometers, not degrees.",
  #                 cell_size_units == "km")

  # Calculate density of occurrences over the grid (per square km)
  indicator <- x %>%
    dplyr::reframe(diversity_val = sum(obs) / area,
                   .by = c("year", "cellid")) %>%
    dplyr::reframe(diversity_val = mean(diversity_val), .by = "year") %>%
    dplyr::mutate(diversity_val = as.numeric(diversity_val)) %>%
    dplyr::arrange(year)

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.newness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "newness"))

  year <- cum_obs <- cum_year_sum <- NULL

  # Handle empty input data
  if (nrow(x) == 0) {
    return(tibble::tibble(year = numeric(), diversity_val = numeric()))
  }

  # Prepare the data with cumulative sums and counts
  cum_data <- x %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(cum_year_sum = cumsum(year),
                  cum_obs = seq_along(cum_year_sum))

  # Extract the final cumulative mean for each year
  indicator <- cum_data %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(diversity_val = dplyr::last(round(cum_year_sum / cum_obs)),
                     .groups = "drop")

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.williams_evenness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "williams_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_ts_evenness_core(x = x, type = "williams_evenness", ...)

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.pielou_evenness <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "pielou_evenness"))

  # Call function to calculate evenness over a grid
  indicator <- calc_ts_evenness_core(x = x, type = "pielou_evenness", ...)

  return(indicator)

}

#' @importFrom b3gbi available_indicators
#' @noRd
calc_ts_evenness_core <- function(x, type, ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")))

 # available_indicators <- NULL; rm(available_indicators)

  num_occ <- obs <- year <- taxonKey <- NULL

  type <- match.arg(type, names(available_indicators))

  # Check if the data is empty
  if (nrow(x) == 0) {
    return(tibble::tibble(year = integer(), diversity_val = numeric()))
  }

  # Calculate number of records for each species by grid cell
  x <- x %>%
    dplyr::summarize(num_occ = sum(obs), .by = c(year, taxonKey)) %>%
    dplyr::arrange(year) %>%
    tidyr::pivot_wider(names_from = year,
                       values_from = num_occ,
                       values_fill = 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list()

  # Apply evenness formula and format result as a data frame
  indicator <- x %>%
    purrr::map(function(y) {
      compute_evenness_formula(y ,type)
    }) %>%
    unlist() %>%
    as.data.frame() %>%
    dplyr::rename(diversity_val = ".") %>%
    tibble::rownames_to_column(var = "year") %>%
    dplyr::mutate(year = as.integer(year), .keep = "unused")

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.ab_rarity <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "ab_rarity"))

  obs <- taxonKey <- records_taxon <- year <- rarity <- diversity_val <- NULL

  total_obs <- sum(x$obs)

  # Calculate rarity as the sum of the inverse of relative abundance for each
  # species
  indicator <- x %>%
    dplyr::mutate(records_taxon = sum(obs), .by = taxonKey) %>%
    dplyr::mutate(rarity = 1 / (records_taxon / total_obs)) %>%
    dplyr::summarise(diversity_val = sum(rarity), .by = "year") %>%
    dplyr::arrange(year)

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.area_rarity <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.", inherits(x, "area_rarity"))

  year <- cellid <- taxonKey <- occupied_cells <- rarity <- NULL
  diversity_val <- NULL

  total_cells <- dplyr::n_distinct(x$cellid)

  # Calculate rarity as the sum (per grid cell) of the inverse of occupancy
  # frequency for each species
  indicator <- x %>%
    dplyr::mutate(occupied_cells = dplyr::n_distinct(cellid),
                  .by = c(taxonKey)) %>%
    dplyr::mutate(rarity = 1 / (occupied_cells / total_cells)) %>%
    dplyr::summarise(diversity_val = sum(rarity), .by = c("year", "cellid")) %>%
    dplyr::summarise(diversity_val = mean(diversity_val), .by = "year") %>%
    dplyr::arrange(year)

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.spec_occ <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_occ"))

  year <- scientificName <- taxonKey <- obs <- diversity_val <- NULL

  indicator <- x %>%
    dplyr::summarize(diversity_val = sum(obs),
                     scientificName = scientificName[1],
                     .by = c(taxonKey, year)) %>%
    dplyr::arrange(year) %>%
    dplyr::select(year, taxonKey, scientificName, diversity_val)

  # Add the 'spec_occ' class back to the object
  class(indicator) <- c("spec_occ", class(indicator))

  return(indicator)

}

#' @export
#' @rdname calc_ts
calc_ts.spec_range <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "spec_range"))

  year <- taxonKey <- cellCode <- obs <- diversity_val <- scientificName <- NULL

  # Flatten occurrences for each species by year
  indicator <- x %>%
    dplyr::summarize(diversity_val = sum(obs >= 1),
                     scientificName = scientificName[1],
                     .by = c(taxonKey, year)) %>%
    dplyr::arrange(taxonKey) %>%
    dplyr::select(year, taxonKey, scientificName, diversity_val)

  # Add the 'spec_range' class back to the object
  class(indicator) <- c("spec_range", class(indicator))

  return(indicator)

}

#' @param set_rows Automatically select which taxonomic information to keep when
#'  there are multiple options. Default value of 1 keeps the first option,
#'  which is usually the best.
#' @export
#' @rdname calc_ts
calc_ts.tax_distinct <- function(x, set_rows = 1, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "tax_distinct"))

  year <- diversity_val <- NULL

  # Early return for empty input
  if (nrow(x) == 0) {
    return(tibble::tibble(year = integer(), diversity_val = numeric()))
  }

  if (requireNamespace("taxize", quietly = TRUE)) {
    # Retrieve taxonomic data from GBIF
    tax_hier <- my_classification(unique(x$scientificName), db = "gbif", ...)
  } else {
    stop("Please install the taxize package to use this function.")
  }

  # Calculate taxonomic distinctness
  indicator <- x %>%
    tibble::add_column(diversity_val = NA) %>%
    dplyr::group_split(year) %>%
    purrr::map(function(y) {
      dplyr::mutate(
        diversity_val = compute_tax_distinct_formula(y, tax_hier))
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(year, diversity_val, .keep_all = TRUE) %>%
    dplyr::select(year, diversity_val)

  return(indicator)

}


#' @export
#' @rdname calc_ts
calc_ts.occ_turnover <- function(x, ...) {

  stopifnot_error("Wrong data class. This is an internal function and is not
                  meant to be called directly.",
                  inherits(x, "occ_turnover"))

  year <- taxonKey <- species <- prev_species <- gains <- losses <- NULL
  diversity_val <- shared <- NULL

  # Get a list of unique species for each year
  species_by_year <- x %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(species = list(unique(taxonKey)), .groups = "drop") %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(
      prev_species = dplyr::lag(species, n = 1, default = list(c()))
    )

  # Calculate gains, losses, and shared species between years
  indicator <- species_by_year %>%
    dplyr::mutate(
      gains = purrr::map2_dbl(species, prev_species,
                              ~length(setdiff(.x, .y))),
      losses = purrr::map2_dbl(species, prev_species,
                               ~length(setdiff(.y, .x))),
      shared = purrr::map2_dbl(species, prev_species,
                               ~length(intersect(.x, .y)))
      ) %>%
    dplyr::mutate(
      # Calculate diversity_val using a conditional statement
      diversity_val = ifelse(
        dplyr::row_number() == 1,
        NA_real_,
        (gains + losses) / (gains + losses + shared)
      )
    ) %>%
    dplyr::select(year, diversity_val)

  return(indicator)

}
