#' Core function to calculate completeness (Sample Coverage) for time series output
#'
#' @param x A processed data cube.
#' @param ... Additional arguments.
#'
#' @return A data frame with year and completeness (sample coverage) values.
#' @noRd
calc_ts_completeness_core <- function(x, ...) {

  stopifnot_error("Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
                  inherits(x, c("data.frame", "sf")) &
                    inherits(x, "completeness"))

  scientificName <- year <- obs <- cellCode <- Assemblage <- SC <- NULL

  temp_opts <- list(...)
  cutoff_length <- temp_opts$cutoff_length

  # Prepare species records by year (incidence-raw format)
  species_records_raw <- x %>%
    dplyr::select(year, scientificName, obs, cellCode) %>%
    dplyr::group_by(year) %>%
    dplyr::group_split() %>%
    purrr::map(function(df) {
      m <- df %>%
        tidyr::pivot_wider(
          names_from = "scientificName",
          values_from = "obs",
          values_fn = sum,
          values_fill = 0
        ) %>%
        dplyr::select(-year, -cellCode) %>%
        as.matrix() %>%
        t()
      m[m > 1] <- 1
      return(m)
    })

  # Carry over year labels
  names(species_records_raw) <- x %>%
    dplyr::distinct(year) %>%
    dplyr::arrange(year) %>%
    dplyr::pull(year)

  # Filter years with too little data
  # For incidence_raw, length(x) is the total number of species-cell combinations?
  # Actually, iNEXT expects a list of matrices for incidence_raw.
  # If it's a list, purrr::keep(..., length(x) > cutoff_length)
  # In calc_ts_hill_core, it assumes cutoff_length refers to number of rows in transposed matrix (species?)
  # No, length(matrix) is rows * cols.
  # Let's align with calc_ts_hill_core:
  species_records_filtered <- purrr::keep(
    species_records_raw, function(x) length(x) > cutoff_length
  )

  if (length(species_records_filtered) == 0) {
    stop("No years left to process after filtering.")
  }

  # Calculate completeness using iNEXT::DataInfo
  info <- my_DataInfo(species_records_filtered, datatype = "incidence_raw")

  # Extract result
  indicator <- info %>%
    dplyr::select(Assemblage, SC) %>%
    dplyr::rename(year = Assemblage, diversity_val = SC) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::as_tibble()

  return(indicator)
}
