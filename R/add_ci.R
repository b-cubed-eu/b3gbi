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
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'   calculate. (Default: `"norm"`). Supported options are:
#'   * `perc`: Percentile intervals.
#'   * `bca`: Bias-corrected and accelerated intervals.
#'   * `norm`: Normal approximation intervals.
#'   * `basic`: Basic bootstrap intervals.
#'   * `none`: No confidence intervals calculated.
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
#'
#' @details
#' The function acts as a bridge to the \pkg{dubicube} package to calculate
#' bootstrap confidence intervals.
#'
#' ## Indicator-specific defaults
#'
#' Depending on the indicator, default settings are internally applied when
#' calculating bootstrap confidence intervals. These defaults control whether
#' bootstrapping is performed per group, which transformation is used, and
#' whether bias correction is disabled.
#'
#' The following defaults are used unless explicitly overridden via
#' `trans`, `inv_trans`, `boot_args`, or `ci_args`:
#'
#' - **`total_occ`**
#'   - Group-specific bootstrapping: **yes**
#'   - Transformation: **none (identity)**
#'   - Bias correction: **disabled** (`no_bias = TRUE`)
#'
#' - **`spec_occ`, `spec_range`**
#'   - Group-specific bootstrapping: **yes**
#'   - Transformation: **none (identity)**
#'   - Bias correction: enabled
#'
#' - **`pielou_evenness`, `williams_evenness`**
#'   - Group-specific bootstrapping: **no**
#'   - Transformation: **logit**
#'   - Inverse transformation: **inverse logit**
#'   - Bias correction: enabled
#'
#' - **`occ_density`, `ab_rarity`, `area_rarity`, `newness`**
#'   - Group-specific bootstrapping: **no**
#'   - Transformation: **none (identity)**
#'   - Bias correction: enabled
#'
#' Group-specific bootstrapping means that resampling is performed within each
#' group (e.g., species or year), which is required for indicators that are
#' inherently group-based. This in contrast to whole-cube bootstrapping
#' where resampling is performed across the whole dataset; applicable for
#' indicators that combine information across groups
#'
#' Transformations are applied prior to confidence interval calculation and
#' inverted afterwards to return intervals on the original scale.
#'
#' ## Indicators outside scope of this function
#'
#' For certain indicators, confidence intervals cannot be
#' added post-hoc as they are calculated internally via the `iNext` package, or
#' if they are not relevant. In such cases, a warning is issued and the
#' original object is returned. The following indicators cannot have confidence
#' intervals added via `add_ci()`:
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
                   ci_type = c("perc",
                               "bca",
                               "norm",
                               "basic",
                               "none"),
                   trans = function(t) t,
                   inv_trans = function(t) t,
                   confidence_level = 0.95,
                   overwrite = TRUE,
                   boot_args = list(),
                   ci_args = list()) {

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
      c(list(bootstrap_samples_df = bootstrap_results), params_total$ci_params)
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
