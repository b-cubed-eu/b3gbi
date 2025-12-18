add_ci <- function(indicator,
                   num_bootstrap = 1000,
                   ci_type = c("norm",
                               "basic",
                               "perc",
                               "bca",
                               "none"),
                   trans = function(t) t,
                   inv_trans = function(t) t,
                   confidence_level = 0.95,
                   overwrite = TRUE) {

  # Check for correct object class
  if (!inherits(indicator, "indicator_ts")) {
    stop("indicator must be an indicator_ts object.")
  }

  # List of indicators for which bootstrapped confidence intervals should not
  # be calculated
  noci_list <- c("obs_richness",
                 "cum_richness",
                 "occ_turnover",
                 "tax_distinct",
                 "hill0",
                 "hill1",
                 "hill2")

  # Match ci_type argument
  ci_type <- match.arg(ci_type)

  # If indicator is in noci_list, return indicator without calculating CIs
  if (indicator$div_type %in% noci_list) {
    if (indicator$div_type %in% c("hill0", "hill1", "hill2")) {
      warning(
        paste0(
          "Confidence intervals cannot calculated for ",
          indicator$div_type,
          " as they are handled by the iNext package when calculating
          your indicator. Returning indicator without adding CIs."
        )
      )
    } else
      warning(
        paste0(
          "Cannot calculate sensible confidence intervals for ",
          indicator$div_type, ". Returning indicator without CIs."
        )
      )
    return(indicator)
  }

  # Extract data from indicator object
  x <- indicator$data
  raw_data <- indicator$raw_data

  if(any(c("ll", "ul") %in% names(x)) & !overwrite) {
    warning(
      paste0(
        "Indicator already contains confidence intervals. Returning indicator
        without adding CIs. Use 'replace = TRUE' argument to recalculate CIs."
      )
    )
    return(indicator)
  }

  # Remove existing confidence intervals if overwrite = TRUE
  if (overwrite &
      all(c("ll", "ul") %in% names(x))) {
    x <- x %>%
      dplyr::select(-ll, -ul)
  }

  # Bootstrap cube data
  bootstrap_results <- dubicube::bootstrap_cube(
    data_cube = raw_data,
    fun = calc_ts,
    grouping_var = "year",
    samples = num_bootstrap,
    seed = 123,
    progress = TRUE,
    processed_cube = FALSE,
    #method = "whole_cube",
  )
  # Calculate confidence intervals from bootstrap results
  ci_df <- dubicube::calculate_bootstrap_ci(
    bootstrap_samples_df = bootstrap_results,
    grouping_var = "year",
    type = ci_type,
    h = trans,
    hinv = inv_trans,
    conf = confidence_level,
    data_cube = raw_data,
    fun = calc_ts
  )
  # Join confidence intervals to indicator object
  if (length(ci_df) > 0) {
    # Convert negative values to zero as rarity cannot be less than zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values by year
    x <- x %>%
      dplyr::full_join(ci_df,
                       by = dplyr::join_by(year),
                       relationship = "many-to-many")

    indicator$data <- x

    return(indicator)

  } else {

    warning(
      paste0(
        "Unable to calculate confidence intervals. There may be ",
        "insufficient data."
      )
    )
  }
}
