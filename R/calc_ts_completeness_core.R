#' Core function to calculate completeness (Sample Coverage) for time series output
#'
#' @param x A processed data cube.
#' @param ... Additional arguments.
#'
#' @return A data frame with year and completeness (sample coverage) values.
#' @noRd
calc_ts_completeness_core <- function(x, ...) {
  stopifnot_error(
    "Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
    inherits(x, c("data.frame", "sf")) &
      inherits(x, "completeness")
  )

  scientificName <- year <- obs <- cellCode <- Assemblage <- SC <- NULL
  cellid <- diversity_val <- NULL

  dots <- list(...)
  cutoff_length <- if ("cutoff_length" %in% names(dots)) dots$cutoff_length else 5
  gridded_average <- if ("gridded_average" %in% names(dots)) {
    dots$gridded_average
  } else {
    FALSE
  }

  # If gridded_average is TRUE, we expect a cellid column from the workflow
  if (gridded_average && !"cellid" %in% names(x)) {
    warning("gridded_average requested but no cellid found. Calculating over whole area.")
    gridded_average <- FALSE
  }

  if (gridded_average) {
    # 1. Calculate completeness PER CELL PER YEAR
    # Each row in 'x' is already assigned to a cellid
    grid_completeness <- x %>%
      dplyr::group_by(year, cellid) %>%
      dplyr::group_split() %>%
      purrr::map_dfr(function(df) {
        current_year <- df$year[1]
        current_cell <- df$cellid[1]

        # Prepare incidence-raw for this cell-year
        m <- df %>%
          tidyr::pivot_wider(
            names_from = "scientificName",
            values_from = "obs",
            values_fn = sum,
            values_fill = 0
          ) %>%
          dplyr::select(-dplyr::any_of(c(
            "year", "cellCode", "cellid", "taxonKey", "kingdomKey", "kingdom",
            "familyKey", "family", "speciesKey", "xcoord", "ycoord",
            "resolution", "familyCount", "speciesCount", "obs",
            "minCoordinateUncertaintyInMeters", "minTemporalUncertainty",
            "area"
          ))) %>%
          as.matrix() %>%
          t()
        m <- (m > 0) * 1L

        if (length(m) <= cutoff_length) {
          return(NULL)
        }

        info <- suppressWarnings(my_DataInfo(list(m), datatype = "incidence_raw"))
        return(tibble::tibble(year = current_year, cellid = current_cell, diversity_val = info$SC))
      })

    # 2. Average by year
    if (nrow(grid_completeness) == 0) {
      return(tibble::tibble(year = numeric(), diversity_val = numeric()))
    }

    indicator <- grid_completeness %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(diversity_val = mean(diversity_val, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(year)
  } else {
    # Original logic: Calculate over entire cube as a single area
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
        m <- (m > 0) * 1L
        return(m)
      })

    # Carry over year labels
    names(species_records_raw) <- x %>%
      dplyr::distinct(year) %>%
      dplyr::arrange(year) %>%
      dplyr::pull(year)

    # Filter years with too little data
    species_records_filtered <- purrr::keep(
      species_records_raw, function(x) length(x) > cutoff_length
    )

    if (length(species_records_filtered) == 0) {
      stop("No years left to process after filtering.")
    }

    # Calculate completeness using iNEXT::DataInfo
    info <- suppressWarnings(my_DataInfo(species_records_filtered, datatype = "incidence_raw"))

    # Extract result
    indicator <- info %>%
      dplyr::select(Assemblage, SC) %>%
      dplyr::rename(year = Assemblage, diversity_val = SC) %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::as_tibble()
  }

  return(indicator)
}
