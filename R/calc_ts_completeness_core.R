#' Core function to calculate completeness (Sample Coverage) for time series output
#'
#' @param x A processed data cube.
#' @param expected_years (Optional) Vector of years expected in the output.
#' @param ... Additional arguments.
#'
#' @return A data frame with year and completeness (sample coverage) values.
#' @noRd
calc_ts_completeness_core <- function(x, ...) {
  stopifnot_error(
    "Please check the class and structure of your data. This is an
                  internal function, not meant to be called directly.",
    rlang::inherits_any(x, c("data.frame", "sf")) &
      inherits(x, "completeness")
  )

  scientificName <- year <- obs <- cellCode <- cellid <- Assemblage <- SC <- NULL
  diversity_val <- NULL

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
    # 1. Prepare matrices for all cell-years
    # We group by year first to maintain the time series structure
    indicator_data <- x %>%
      dplyr::group_by(year, cellid) %>%
      dplyr::group_split() %>%
      purrr::map(function(df) {
        current_year <- df$year[1]
        current_cell <- df$cellid[1]

        m <- df %>%
          tidyr::pivot_wider(
            id_cols = "cellCode",
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

        # Check for sufficient samples
        if (ncol(m) < 2 || ncol(m) <= cutoff_length) {
          return(NULL)
        }

        # We need a unique name for this assemblage to avoid iNEXT naming conflicts
        # Use year__cellid as the name
        attr(m, "assemblage_name") <- paste0(current_year, "__", current_cell)
        attr(m, "year") <- current_year
        return(m)
      })

    # Filter out NULLs (insufficient data) before naming
    indicator_data <- purrr::compact(indicator_data)

    # Ensure all years from the original input are represented
    dots <- list(...)
    expected_years <- dots$expected_years
    all_years <- tibble::tibble(year = if (!is.null(expected_years)) expected_years else sort(unique(x$year)))

    if (length(indicator_data) == 0) {
      indicator <- all_years %>% dplyr::mutate(diversity_val = NA_real_)
    } else {
      # Extract names and call Info once
      input_list <- indicator_data
      names(input_list) <- purrr::map_chr(indicator_data, ~attr(.x, "assemblage_name"))

      info <- suppressWarnings(my_DataInfo(input_list, datatype = "incidence_raw"))

      # Extract results and average by year
      grid_completeness <- info %>%
        dplyr::select(Assemblage, SC) %>%
        dplyr::mutate(
          year = as.numeric(gsub("__.*", "", Assemblage))
        ) %>%
        dplyr::rename(diversity_val = SC)

      indicator <- grid_completeness %>%
        dplyr::group_by(year) %>%
        dplyr::summarise(diversity_val = mean(diversity_val, na.rm = TRUE), .groups = "drop") %>%
        dplyr::right_join(all_years, by = "year") %>%
        dplyr::arrange(year)
    }

    # Provide feedback to the user
    non_na_count <- sum(!is.na(indicator$diversity_val))
    if (non_na_count == 0) {
      message("Gridded completeness returned no results. Try lowering 'cutoff_length' or using a coarser grid.")
    } else {
      message(sprintf("Gridded completeness calculated for %d out of %d years.",
                      non_na_count, nrow(all_years)))
    }
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

    # Ensure all years from the original input are represented
    dots <- list(...)
    expected_years <- dots$expected_years
    all_years <- tibble::tibble(year = if (!is.null(expected_years)) expected_years else sort(unique(x$year)))

    if (length(species_records_filtered) == 0) {
      indicator <- all_years %>% dplyr::mutate(diversity_val = NA_real_)
    } else {
      # Calculate completeness using iNEXT::DataInfo
      info <- suppressWarnings(my_DataInfo(species_records_filtered, datatype = "incidence_raw"))

      # Extract result
      indicator <- info %>%
        dplyr::select(Assemblage, SC) %>%
        dplyr::rename(year = Assemblage, diversity_val = SC) %>%
        dplyr::mutate(year = as.numeric(year)) %>%
        dplyr::right_join(all_years, by = "year") %>%
        dplyr::arrange(year)
    }
  }

  return(indicator)
}
