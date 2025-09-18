#' Core function for handling the confidence interval calculations for different
#'  indicator types. This function is called by the calc_ci functions for each
#'  indicator type.
#' @param bootstraps Bootstrapped indicator values
#' @param indicator An indicator calculated over time, in the form of a data
#'  frame. *Note: this should NOT be an 'indicator_ts' object as it is meant to
#'  be called by the 'compute_indicator_workflow' function.
#' @param ci_type Type of confidence interval to calculate
#' @param ... Additional arguments
#' @noRd
calc_ci_core <- function(bootstraps,
                         indicator,
                         ci_type,
                         ...) {

  year <- NULL

  # Calculate confidence intervals
  ci_df <- get_bootstrap_ci(bootstraps, type = ci_type, ...)

  if (length(ci_df) > 0) {

    # Convert negative values to zero as rarity cannot be less than zero
    ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)

    # Join confidence intervals to indicator values by year
    indicator <- indicator %>%
      dplyr::full_join(ci_df,
                       by = dplyr::join_by(year),
                       relationship = "many-to-many")

  } else {

    warning(
      paste0(
        "Unable to calculate confidence intervals. There may be ",
        "insufficient data."
      )
    )
  }

  return(indicator)

}
