#' Core function to calculate confidence intervals for evenness. This is called
#'  by the calc_ci.pielou_evenness and calc_ci.williams_evenness functions.
#' @param x A data cube object.
#' @param type Evenness measure ("pielou_evenness" or "williams_evenness)
#' @param indicator An indicator calculated over time, in the form of a data
#'  frame. *Note: this should NOT be an 'indicator_ts' object as it is meant to
#'  be called by the 'compute_indicator_workflow' function.
#' @param num_bootstrap (Optional) Set the number of bootstraps to calculate for
#'  generating confidence intervals. (Default: 1000)
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'  calculate. (Default: "norm". Select "none" to avoid calculating bootstrap
#'  CIs.)
#' @param ... Additional arguments
#' @noRd
calc_ci_evenness_core <- function(x,
                                  type,
                                  indicator,
                                  num_bootstrap = 1000,
                                  ci_type = ci_type,
                                  ...) {

  obs <- year <- taxonKey <- num_occ <- NULL

  stopifnot_error(
    "Please check the class and structure of your data.
    This is an internal function, not meant to be called directly.",
    inherits(x, c("data.frame", "sf"))
  )

  type <- match.arg(type,
                    names(available_indicators))

  # Calculate number of records for each species by grid cell
  x <- x %>%
    dplyr::summarize(num_occ = sum(obs),
                     .by = c(year, taxonKey)) %>%
    dplyr::arrange(year) %>%
    tidyr::pivot_wider(names_from = year,
                       values_from = num_occ,
                       values_fill = 0) %>%
    tibble::column_to_rownames("taxonKey") %>%
    as.list()

  # Bootstrap evenness values
  bootstraps <- x %>%
    purrr::map(~boot::boot(
      data = .,
      statistic = boot_statistic_evenness,
      R = num_bootstrap,
      type = type))

  # Replace NA values to avoid errors when calculating confidence intervals
  bootstraps <- lapply(bootstraps, ci_error_prevent)

  names(bootstraps) <- unique(indicator$year)

  # Calculate confidence intervals and add to indicator values
  ci <- calc_ci_core(bootstraps, indicator, ci_type, ...)

  return(ci)

}
