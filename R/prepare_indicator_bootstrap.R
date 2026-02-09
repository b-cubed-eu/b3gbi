#' Prepare bootstrap and confidence interval parameters for an indicator
#'
#' This function prepares the argument lists for [dubicube::bootstrap_cube()]
#' and [dubicube::calculate_bootstrap_ci()] based on the indicator definition.
#' Behaviour (grouping, bootstrap method, transformations, bias correction)
#' is fully controlled by a rule book keyed on `indicator$div_type`.
#'
#' No computation is performed; the function only returns parameter lists.
#'
#' @param indicator A list describing the indicator. Must contain at least
#'   \code{div_type} and \code{raw_data}.
#' @param num_bootstrap Integer. Number of bootstrap samples.
#' @param ci_type Character. Type of confidence interval.
#' @param seed Integer. Random seed for bootstrapping.
#' @param trans Optional transformation function applied to the statistic.
#'   Will be overridden if specified by the indicator rule book.
#' @param inv_trans Optional inverse transformation function.
#'   Will be overridden if specified by the indicator rule book.
#' @param confidence_level Numeric between 0 and 1. Confidence level.
#' @param boot_args Named list of additional arguments passed to
#'   \code{bootstrap_cube()}, overriding defaults.
#' @param ci_args Named list of additional arguments passed to
#'   \code{calculate_bootstrap_ci()}, overriding defaults.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{bootstrap_params}{List of parameters for \code{bootstrap_cube()}}
#'   \item{ci_params}{List of parameters for \code{calculate_bootstrap_ci()}}
#' }
#'
#' @importFrom utils modifyList
#'
#' @examples
#' \dontrun{
#' params <- prepare_indicator_bootstrap(
#'   indicator = indicator,
#'   num_bootstrap = 1000,
#'   ci_type = "bca"
#' )
#' }
#'
#' @export
prepare_indicator_bootstrap <- function(
    indicator,
    num_bootstrap,
    ci_type,
    trans = function(t) t,
    inv_trans = function(t) t,
    confidence_level = 0.95,
    seed = NULL,
    boot_args = list(),
    ci_args = list()) {
  ## ------------------------------------------------------------------
  ## Indicator rule book
  ## ------------------------------------------------------------------
  ## Each entry controls:
  ## - whether group-specific bootstrapping is used
  ## - which transformation (if any) is applied
  ## - whether bias correction is disabled (no_bias)
  indicator_rules <- list(
    total_occ = list(
      group_specific = TRUE,
      trans = trans,
      inv_trans = inv_trans,
      no_bias = TRUE
    ),
    pielou_evenness = list(
      group_specific = FALSE,
      trans = logit,
      inv_trans = inv_logit,
      no_bias = FALSE
    ),
    williams_evenness = list(
      group_specific = FALSE,
      trans = logit,
      inv_trans = inv_logit,
      no_bias = FALSE
    ),
    occ_density = list(
      group_specific = FALSE,
      trans = trans,
      inv_trans = inv_trans,
      no_bias = FALSE
    ),
    ab_rarity = list(
      group_specific = FALSE,
      trans = trans,
      inv_trans = inv_trans,
      no_bias = FALSE
    ),
    area_rarity = list(
      group_specific = FALSE,
      trans = trans,
      inv_trans = inv_trans,
      no_bias = FALSE
    ),
    newness = list(
      group_specific = FALSE,
      trans = trans,
      inv_trans = inv_trans,
      no_bias = FALSE
    ),
    spec_occ = list(
      group_specific = TRUE,
      trans = trans,
      inv_trans = inv_trans,
      no_bias = FALSE
    ),
    spec_range = list(
      group_specific = TRUE,
      trans = trans,
      inv_trans = inv_trans,
      no_bias = FALSE
    )
  )

  rule <- indicator_rules[[indicator$div_type]]
  if (is.null(rule)) {
    stop("Unknown indicator$div_type: ", indicator$div_type)
  }
  ## Allow user-supplied arguments to override defaults
  rule <- utils::modifyList(rule, boot_args)
  rule <- utils::modifyList(rule, ci_args)

  ## ------------------------------------------------------------------
  ## Determine grouping variables
  ## ------------------------------------------------------------------
  ## Species-level indicators are grouped by year and taxon;
  ## all others only by year.
  group_cols <- if (indicator$div_type %in% c("spec_occ", "spec_range")) {
    c("year", "taxonKey")
  } else {
    "year"
  }

  ## ------------------------------------------------------------------
  ## Determine bootstrap method
  ## ------------------------------------------------------------------
  boot_method <- if (rule$group_specific) {
    "group_specific"
  } else {
    "whole_cube"
  }

  ## Whole-cube bootstrapping requires the "boot_" prefix
  if (length(group_cols) == 1) {
    boot_method <- paste0("boot_", boot_method)
  }

  ## ------------------------------------------------------------------
  ## Prepare bootstrap_cube parameters
  ## ------------------------------------------------------------------
  bootstrap_params <- list(
    data_cube = indicator$raw_data,
    fun = calc_ts,
    grouping_var = group_cols,
    samples = num_bootstrap,
    processed_cube = FALSE,
    method = boot_method
  )

  # Only add seed if it's actually provided
  if (!is.null(seed)) {
    bootstrap_params$seed <- seed
  }

  ## Allow user-supplied arguments to override defaults
  bootstrap_params <- utils::modifyList(bootstrap_params, boot_args)

  ## ------------------------------------------------------------------
  ## Prepare calculate_bootstrap_ci parameters
  ## ------------------------------------------------------------------
  if ("bca" %in% ci_type & !grepl("^boot", boot_method)) {
    ci_params <- list(
      grouping_var = group_cols,
      type = ci_type,
      h = rule$trans,
      hinv = rule$inv_trans,
      conf = confidence_level,
      data_cube = indicator$raw_data,
      fun = calc_ts,
      no_bias = rule$no_bias
    )
  } else {
    ci_params <- list(
      grouping_var = group_cols,
      type = ci_type,
      h = rule$trans,
      hinv = rule$inv_trans,
      conf = confidence_level,
      no_bias = rule$no_bias
    )
  }

  ## Allow user-supplied arguments to override defaults
  ci_params <- utils::modifyList(ci_params, ci_args)

  ## ------------------------------------------------------------------
  ## Return prepared parameters
  ## ------------------------------------------------------------------
  list(
    bootstrap_params = bootstrap_params,
    ci_params = ci_params
  )
}
