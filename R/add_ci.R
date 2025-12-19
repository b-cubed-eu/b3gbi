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
#' intervals should be added.
#' @param num_bootstrap (Optional) Number of bootstrap replicates to perform.
#' (Default: 1000)
#' @param bootstrap_level (Optional) Level at which to perform bootstrapping:
#' \itemize{
#'   \item \code{'cube'} (default): Bootstrapping is done by resampling the
#'   occurrence records in the cube. This is mathematically more robust as it
#'   captures the underlying sampling uncertainty.
#'   \item \code{'indicator'}: Bootstrapping is done by resampling indicator
#'    values. This is faster for large cubes but less robust..
#' }

#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#' calculate. (Default: \code{"norm"}). Supported options are:
#' \itemize{
#'   \item \code{'norm'}: Normal approximation intervals.
#'   \item \code{'basic'}: Basic bootstrap intervals.
#'   \item \code{'perc'}: Percentile intervals.
#'   \item \code{'bca'}: Bias-corrected and accelerated intervals.
#'   \item \code{'none'}: No confidence intervals calculated.
#' }
#' @param trans (Optional) A function for transforming the indicator values
#' before calculating confidence intervals (e.g., \code{log}).
#' (Default: identity function)
#' @param inv_trans (Optional) The inverse of the transformation function
#' \code{trans} (e.g., \code{exp}). Used to back-transform the intervals
#' to the original scale. (Default: identity function)
#' @param confidence_level (Optional) The confidence level for the calculated
#' intervals (e.g., 0.95 for 95\% CIs). (Default: 0.95)
#' @param overwrite (Optional) Logical. If the indicator already contains
#' confidence intervals (\code{ll} and \code{ul} columns), should they
#' be replaced? (Default: TRUE)
#'
#' @details
#' The function acts as a bridge to the \code{dubicube} package for statistical
#' heavy lifting. For certain indicators (e.g., Hill numbers), confidence
#' intervals cannot be added post-hoc as they are calculated internally by
#' the \code{iNext} package during the initial calculation. In such cases,
#' a warning is issued and the original object is returned.
#'
#' @return An updated object of class \code{indicator_ts} containing the
#' original data with the following additional columns:
#' \itemize{
#'   \item \code{ll}: Lower limit of the confidence interval.
#'   \item \code{ul}: Upper limit of the confidence interval.
#'   \item \code{est_boot}: The bootstrap estimate of the indicator value.
#'   \item \code{se_boot}: The bootstrap standard error.
#'   \item \code{bias_boot}: The bootstrap estimate of bias.
#'   \item \code{int_type}: The type of interval calculated (e.g., 'perc').
#'   \item \code{conf}: The confidence level used.
#' }
#'
#' @seealso \code{\link[dubicube]{bootstrap_cube}}, \code{\link[dubicube]{calculate_bootstrap_ci}}
#'
#' @rdname add_ci
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

  # Calculate confidence intervals
  if (bootstrap_level == "indicator") {

    # Send data to calc_ci for indicator level bootstrapping
    indicator <- calc_ci(data_final_nogeom,
                         indicator = indicator,
                         num_bootstrap = num_bootstrap,
                         ci_type = ci_type,
                         ...)

  } else if (bootstrap_level == "cube") {

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
    ) %>%
      dplyr::select(-est_original)

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
  } else {
    stop("Invalid bootstrap_level. Choose 'cube' or 'indicator'.")
  }
}
