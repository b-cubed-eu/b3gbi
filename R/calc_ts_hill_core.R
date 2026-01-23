#' @param type (Optional) Choose which Hill number, or q, to calculate. Choose
#'  'hill0' (q = 0) for estimated species richness, 'hill1' for Hill-Shannon
#'  diversity, or 'hill2' for Hill-Simpson diversity.
#' @param ... Additional arguments passed to iNEXT::estimateD(e.g.,nboot, conf).
#' @importFrom iNEXT estimateD DataInfo
#' @noRd
calc_ts_hill_core <- function(x, type = c("hill0", "hill1", "hill2"), ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")) &
                    rlang::inherits_any(x, c("hill0", "hill1", "hill2")))

  scientificName <- year <- obs <- cellCode <- Assemblage <- qD <- SC <-
    Order.q <- qD.LCL <- qD.UCL <- NULL

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
  species_records_raw <- purrr::keep(
    species_records_raw, function(x) length(x) > cutoff_length
  )

  coverage_rare <- species_records_raw %>%
    my_estimateD(base = "coverage",
                 level = coverage,
                 datatype = "incidence_raw",
                 q = qval,
                 conf = conf,
                 nboot = nboot)

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
