#' Add Confidence Intervals to an Indicator Object
#'
#' @description
#' This function calculates bootstrap confidence intervals for an existing
#' `indicator_ts` object. It supports both cube-level bootstrapping (resampling
#' occurrence records) and indicator-level bootstrapping (resampling
#' calculated values), allowing for advanced transformations during the
#' CI calculation process.
#'
#' @param indicator An object of class `indicator_ts` to which confidence
#'   intervals should be added.
#' @param num_bootstrap (Optional) Number of bootstrap replicates to perform.
#'   (Default: 1000)
#' @param bootstrap_level (Optional) Level at which to perform bootstrapping:
#'   * `cube` (default): Bootstrapping is done by resampling the
#'     occurrence records in the cube. This is mathematically more robust as it
#'     captures the underlying sampling uncertainty.
#'   * `indicator`: Bootstrapping is done by resampling indicator
#'     values. This is faster for large cubes but less robust.
#'
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'   calculate. (Default: `"norm"`). Supported options are:
#'   * `norm`: Normal approximation intervals.
#'   * `basic`: Basic bootstrap intervals.
#'   * `perc`: Percentile intervals.
#'   * `bca`: Bias-corrected and accelerated intervals.
#'   * `none`: No confidence intervals calculated.
#'
#' @param trans (Optional) A function for transforming the indicator values
#'   before calculating confidence intervals (e.g., `log`).
#'   (Default: identity function)
#' @param inv_trans (Optional) The inverse of the transformation function
#'   `trans` (e.g., `exp`). Used to back-transform the intervals
#'   to the original scale. (Default: identity function)
#' @param confidence_level (Optional) The confidence level for the calculated
#'   intervals (e.g., 0.95 for 95% CIs). (Default: 0.95)
#' @param overwrite (Optional) Logical. If the indicator already contains
#'   confidence intervals (`ll` and `ul` columns), should they
#'   be replaced? (Default: TRUE)
#' @param boot_args (Optional) Named list of additional arguments passed to
#'   `dubicube::bootstrap_cube()`. (Default: `list()`)
#' @param ci_args (Optional) Named list of additional arguments passed to
#'   `dubicube::calculate_bootstrap_ci()`. (Default: `list()`)
#' @param ... Additional arguments passed to the underlying functions.
#'
#' @details
#' The function acts as a bridge to the `dubicube` package for statistical
#' heavy lifting.
#'
#' Confidence intervals can be calculated for the following indicators:
#' * `total_occ`
#' * `occ_density`
#' * `newness`
#' * `williams_evenness`
#' * `pielou_evenness`
#' * `ab_rarity`
#' * `area_rarity`
#' * `spec_occ`
#' * `spec_range`
#'
#' For certain indicators (e.g., Hill numbers), confidence
#' intervals cannot be added post-hoc as they are calculated internally by
#' the `iNext` package during the initial calculation. In such cases,
#' a warning is issued and the original object is returned. The following
#' indicators cannot have confidence intervals added via `add_ci()`:
#' * `hill0`, `hill1`, `hill2` (calculated internally)
#' * `obs_richness`
#' * `cum_richness`
#' * `occ_turnover`
#' * `tax_distinct`
#'
#' @return An updated object of class `indicator_ts` containing the
#'   original data with the following additional columns:
#'   * `ll`: Lower limit of the confidence interval.
#'   * `ul`: Upper limit of the confidence interval.
#'   * `est_boot`: The bootstrap estimate of the indicator value.
#'   * `se_boot`: The bootstrap standard error.
#'   * `bias_boot`: The bootstrap estimate of bias.
#'   * `int_type`: The type of interval calculated (e.g., 'perc').
#'   * `conf`: The confidence level used.
#'
#' @seealso [dubicube::bootstrap_cube()], [dubicube::calculate_bootstrap_ci()]
#'
#' @export
add_ci <- function(indicator,
                   num_bootstrap = 1000,
                   bootstrap_level = c("cube",
                                       "indicator"),
                   ci_type = c("norm",
                               "basic",
                               "perc",
                               "bca",
                               "none"),
                   trans = function(t) t,
                   inv_trans = function(t) t,
                   confidence_level = 0.95,
                   overwrite = TRUE,
                   boot_args = list(),
                   ci_args = list(),
                   ...) {

  # Check for correct object class
  if (!inherits(indicator, "indicator_ts")) {
    stop("indicator must be an indicator_ts object.")
  }

  ll <- ul <- year <- est_original <- NULL

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
  bootstrap_level <- match.arg(bootstrap_level)

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

  if (any(c("ll", "ul") %in% names(x)) & !overwrite) {
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

  # Calculate confidence intervals
  if (bootstrap_level == "indicator") {

    # Send data to calc_ci for indicator level bootstrapping
    indicator$data <- calc_ci(raw_data,
                              indicator = x,
                              num_bootstrap = num_bootstrap,
                              ci_type = ci_type,
                              ...)
    return(indicator)

  } else if (bootstrap_level == "cube") {
    # Get dubicube function parameters
    params_total  <- prepare_indicator_bootstrap(
      indicator = indicator,
      num_bootstrap = num_bootstrap,
      ci_type = ci_type,
      trans = trans,
      inv_trans = inv_trans,
      confidence_level = confidence_level,
      boot_args = boot_args,
      ci_args = ci_args
    )

    # Bootstrap cube data
    bootstrap_results <- do.call(dubicube::bootstrap_cube,
                                 params_total$bootstrap_params)

    # Calculate confidence intervals from bootstrap results
    ci_df <- do.call(
      dubicube::calculate_bootstrap_ci,
      c(bootstrap_samples_df = bootstrap_results, params_total$ci_params)
    ) %>%
      dplyr::select(-est_original)

    # Join confidence intervals to indicator object
    if (nrow(ci_df) > 0) {
      # Convert negative values to zero as rarity cannot be less than zero
      ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)
      # Join confidence intervals to indicator values by year
      x <- x %>%
        dplyr::full_join(ci_df,
                         by = group_cols)
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
  } else {
    stop("Invalid bootstrap_level. Choose 'cube' or 'indicator'.")
  }
}
