#' Add Confidence Intervals to an Indicator Object
#'
#' @description
#' This function calculates bootstrap confidence intervals for an existing
#' `indicator_ts` object. It supports both cube-level bootstrapping (resampling
#' occurrence records) and indicator-level bootstrapping (resampling the
#' per-year components of the indicator), allowing for advanced
#' transformations during the CI calculation process.
#'
#' @param indicator An object of class `indicator_ts` to which confidence
#'   intervals should be added.
#' @param num_bootstrap (Optional) Number of bootstrap replicates to perform.
#'   (Default: 1000)
#' @param bootstrap_level (Optional) Level at which to perform bootstrapping:
#'   * `auto` (default): Uses `cube` if the \pkg{dubicube} package is
#'     installed, and otherwise falls back to `indicator` (with a message).
#'   * `cube`: Bootstrapping is done by resampling the occurrence records in
#'     the cube, using the \pkg{dubicube} package. This is statistically more
#'     robust as it captures the underlying sampling uncertainty. Requires
#'     \pkg{dubicube}.
#'   * `indicator`: Within each year, the component values of the indicator
#'     (occurrence records, species, or grid-cell values) are resampled with
#'     \pkg{boot}, without recalculating the indicator from a resampled cube.
#'     This is faster for large cubes but less robust, and does not require
#'     \pkg{dubicube}.
#'
#'   The level that was used is stored in the `ci_method` element of the
#'   returned object and shown when it is printed.
#' @param ci_type (Optional) Type of bootstrap confidence intervals to
#'   calculate. (Default: `"perc"`). Supported options are:
#'   * `perc`: Percentile intervals.
#'   * `bca`: Bias-corrected and accelerated intervals.
#'   * `norm`: Normal approximation intervals.
#'   * `basic`: Basic bootstrap intervals.
#'   * `none`: No confidence intervals calculated; the indicator is returned
#'     unchanged (with a message).
#' @param trans (Optional) A function for transforming the indicator values
#'   before calculating confidence intervals (e.g., `log`). At indicator
#'   level it is passed to `boot::boot.ci()` as `h` (not used for Hill
#'   numbers). The `trans`/`inv_trans` arguments are ignored for evenness
#'   indicators at cube level, which always use the logit transformation
#'   unless overridden via `boot_args` or `ci_args`.
#'   (Default: identity function)
#' @param inv_trans (Optional) The inverse of the transformation function
#'   `trans` (e.g., `exp`). Used to back-transform the intervals
#'   to the original scale. At indicator level it is passed to
#'   `boot::boot.ci()` as `hinv`. (Default: identity function)
#' @param confidence_level (Optional) The confidence level for the calculated
#'   intervals (e.g., 0.95 for 95% CIs). Used at both bootstrap levels.
#'   (Default: 0.95)
#' @param overwrite (Optional) Logical. If the indicator already contains
#'   confidence intervals (`ll` and `ul` columns), should they
#'   be replaced? (Default: TRUE)
#' @param seed (Optional) Integer. Random seed for bootstrapping, used at both
#'   bootstrap levels. The random number generator state of the session is
#'   restored afterwards. Use `NA` to not set a seed. (Default: 123)
#' @param boot_args (Optional) Named list of additional arguments passed to
#'   `dubicube::bootstrap_cube()` (cube level only). (Default: `list()`)
#' @param ci_args (Optional) Named list of additional arguments passed to
#'   `dubicube::calculate_bootstrap_ci()` (cube level only).
#'   (Default: `list()`)
#' @param ... (Optional) Additional arguments passed to `calc_ci()`
#'   (indicator level only).
#'
#' @details
#' For cube-level bootstrapping, the function acts as a bridge to the
#' \pkg{dubicube} package (Langeraert et al.), which is developed alongside
#' \pkg{b3gbi} within the B-Cubed project. \pkg{dubicube} is not on CRAN; it
#' can be installed from R-universe with
#' `install.packages("dubicube", repos = c("https://b-cubed-eu.r-universe.dev", "https://cloud.r-project.org"))`.
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
#'   - Group-specific bootstrapping: **no** (whole-cube resampling, with
#'     intervals per year and species)
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
#' group (e.g., year), which is required for indicators that are
#' inherently group-based. This is in contrast to whole-cube bootstrapping,
#' where resampling is performed across the whole dataset; this is applicable
#' to indicators that combine information across groups.
#'
#' Transformations are applied prior to confidence interval calculation and
#' inverted afterwards to return intervals on the original scale.
#'
#' These defaults (grouping, the logit transformation for evenness and the
#' disabled bias correction for `total_occ`) apply to cube-level
#' bootstrapping. At indicator level, `trans` and `inv_trans` are passed to
#' `boot::boot.ci()` for all indicators (including evenness) except Hill
#' numbers.
#'
#' ## Indicators outside scope of this function
#'
#' For certain indicators, confidence intervals cannot be calculated or
#' added post-hoc because they are not statistically relevant, or because 
#' proper uncertainty estimation is not supported. In such cases, a warning 
#' is issued and the original object is returned. The following indicators 
#' cannot have confidence intervals added via `add_ci()`:
#' * `obs_richness`, `spec_richness_density`: Observed species richness and 
#'   richness density are highly sensitive to sampling effort and sample size. 
#'   Furthermore, bootstrapping observed occurrences can never discover 
#'   new/unobserved species, meaning resampled richness estimates are always 
#'   less than or equal to the observed richness. This results in confidence 
#'   intervals that lie entirely at or below the observed value (i.e., the 
#'   upper bound cannot exceed the observed richness). Therefore, post-hoc 
#'   bootstrapping is not a statistically sound way to estimate their 
#'   uncertainty (Hill numbers `hill0`, `hill1`, `hill2` should be used 
#'   instead for proper rarefaction/extrapolation).
#' * `completeness`: Sample completeness (coverage) is calculated as a 
#'   deterministic sample statistic using `iNEXT` methods, and confidence 
#'   interval calculation is not supported.
#' * `relative_occupancy`: Standard bootstrapping is not supported because 
#'   resampling strips spatial attributes (like `total_num_cells` or 
#'   `total_area_sqkm`) necessary for occupancy calculation. These spatial 
#'   attributes cannot be regenerated from the resampled occurrences alone, 
#'   as occurrences only capture cells/regions where species were detected 
#'   and contain no information about empty grid cells (zero occurrences) 
#'   in the study area.
#' * `cum_richness` (Cumulative Species Richness): This is an inherently 
#'   temporal and sequential indicator for which CIs are not generated, as its 
#'   nature as an accumulating count over time is not well-suited to standard 
#'   bootstrapping methods for uncertainty.
#' * `occ_turnover` (Species Turnover): Confidence intervals are not 
#'   calculated for this indicator. The metric's reliance on comparing the 
#'   unique species lists between consecutive time steps means that 
#'   bootstrapping individual observations within each time step would 
#'   introduce artificial variability in these lists. This makes the resulting 
#'   gains and losses highly unstable and would lead to unreliable or biased 
#'   confidence intervals for the turnover metric.
#' * `tax_distinct` (Taxonomic Distinctness): Confidence intervals are not 
#'   calculated for this indicator. Due to its high sensitivity to the exact 
#'   species composition and taxonomic relationships within a sample, 
#'   bootstrapping individual occurrences can introduce significant artificial 
#'   variability, leading to unreliable or biased confidence intervals.
#'
#' *Note:* For Hill numbers (`hill0`, `hill1`, `hill2`), cube-level 
#' bootstrapping is not supported natively, but `add_ci()` will automatically 
#' switch to indicator-level bootstrapping and calculate confidence intervals 
#' internally using the `iNEXT` package.
#'
#' @return The input `indicator_ts` object, with the bootstrap level used
#'   stored in its `ci_method` element (`"cube"` or `"indicator"`) and the
#'   following columns added to its `data`:
#'   * `ll`: Lower limit of the confidence interval (negative lower limits
#'     are set to 0).
#'   * `ul`: Upper limit of the confidence interval.
#'
#'   Except for Hill numbers (whose intervals come from \pkg{iNEXT}), the
#'   following columns are also added:
#'   * `int_type`: The type of interval calculated (e.g., `"perc"`).
#'   * `conf`: The confidence level used.
#'   * `est_boot`: The bootstrap estimate of the indicator value.
#'   * `se_boot`: The bootstrap standard error.
#'   * `bias_boot`: The bootstrap estimate of bias.
#'
#'   At cube level, `est_boot`, `se_boot` and `bias_boot` are only returned
#'   for some indicators (e.g., `total_occ`, `spec_occ` and `spec_range`),
#'   depending on the bootstrap method and bias correction used.
#'
#'   If `ci_type = "none"`, or if confidence intervals cannot be calculated
#'   for the indicator, the input object is returned unchanged.
#'
#' @seealso `dubicube::bootstrap_cube()`, `dubicube::calculate_bootstrap_ci()`
#'
#' @examples
#' \donttest{
#' # Load sample cube data
#' cube_path <- system.file("extdata", "denmark_mammals_cube_eea.csv", package = "b3gbi")
#' cube <- process_cube(cube_path)
#' 
#' # Calculate a time series indicator
#' ts_occ <- total_occ_ts(cube)
#' 
#' # Add bootstrap confidence intervals
#' ts_occ_ci <- add_ci(ts_occ, num_bootstrap = 100)
#' plot(ts_occ_ci)
#' }
#'
#' @export
add_ci <- function(indicator,
                   num_bootstrap = 1000,
                   bootstrap_level = c("auto",
                                       "cube",
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
                   ci_args = list(),
                   seed = 123,
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
                 "spec_richness_density",
                 "completeness",
                 "relative_occupancy")

  # Match ci_type argument
  ci_type <- match.arg(ci_type)
  bootstrap_level <- match.arg(bootstrap_level)

  # Nothing to calculate if no confidence intervals are requested
  if (ci_type == "none") {
    rlang::inform(
      "`ci_type = \"none\"`: returning indicator without confidence intervals."
    )
    return(indicator)
  }

  # If indicator is in noci_list, return indicator without calculating CIs
  if (indicator$div_type %in% noci_list) {
    warning(
      paste0(
        "Cannot calculate sensible confidence intervals for ",
        indicator$div_type, ". Returning indicator without CIs."
      )
    )
    return(indicator)
  }

  # iNEXT calculates CIs internally, so Hill numbers always use the
  # indicator level (silently when the level was chosen automatically)
  if (indicator$div_type %in% c("hill0", "hill1", "hill2") &&
      bootstrap_level == "auto") {
    bootstrap_level <- "indicator"
  }
  if (indicator$div_type %in% c("hill0", "hill1", "hill2") && bootstrap_level == "cube") {
    warning(
      paste0(
        "Cube-level bootstrapping is not supported for ", indicator$div_type, 
        " as confidence intervals are natively handled by the iNEXT package. ",
        "Switching to 'indicator' level bootstrapping."
      )
    )
    bootstrap_level <- "indicator"
  }

  # Resolve the bootstrap level: cube level needs the dubicube package
  if (bootstrap_level == "auto") {
    if (is_package_installed("dubicube")) {
      bootstrap_level <- "cube"
    } else {
      rlang::inform(
        c(paste0("Package 'dubicube' is not installed, so confidence ",
                 "intervals are calculated by indicator-level bootstrapping."),
          i = paste0("For cube-level bootstrapping, install 'dubicube' with ",
                     "install.packages(\"dubicube\", repos = ",
                     "c(\"https://b-cubed-eu.r-universe.dev\", ",
                     "\"https://cloud.r-project.org\"))")),
        .frequency = "once",
        .frequency_id = "b3gbi_add_ci_no_dubicube"
      )
      bootstrap_level <- "indicator"
    }
  } else if (bootstrap_level == "cube") {
    check_dubicube_installed()
  }

  # Extract data from indicator object
  x <- indicator$data
  raw_data <- indicator$raw_data

  # Add appropriate class
  type <- indicator$div_type
  subtype <- paste0(type, "_", attributes(indicator)$type)
  class(raw_data) <- append(type, class(raw_data))
  class(raw_data) <- append(subtype, class(raw_data))

  if (any(c("ll", "ul") %in% names(x)) & !overwrite) {
    warning(
      paste0(
        "Indicator already contains confidence intervals. Returning ",
        "indicator without adding CIs. Use 'overwrite = TRUE' to recalculate ",
        "CIs."
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

    # Send data to calc_ci for indicator level bootstrapping. The confidence
    # level and transformations are passed on to boot::boot.ci() (as `conf`,
    # `h` and `hinv`); arguments supplied via `...` take precedence.
    calc_ci_args <- utils::modifyList(
      list(x = raw_data,
           indicator = x,
           num_bootstrap = num_bootstrap,
           ci_type = ci_type,
           conf = confidence_level,
           h = trans,
           hinv = inv_trans),
      list(...)
    )
    indicator$data <- with_rng_seed(seed, do.call(calc_ci, calc_ci_args))
    indicator$ci_method <- "indicator"
    return(indicator)

  } else if (bootstrap_level == "cube") {
    # Get expected years for consistency in indicator results
    expected_years <- unique(indicator$data$year)

    # Get dubicube function parameters
    params_total  <- prepare_indicator_bootstrap(
      indicator = indicator,
      num_bootstrap = num_bootstrap,
      ci_type = ci_type,
      expected_years = expected_years,
      trans = trans,
      inv_trans = inv_trans,
      confidence_level = confidence_level,
      boot_args = boot_args,
      ci_args = ci_args,
      seed = seed
    )

    # Bootstrap cube data
    bootstrap_results <- do.call(dubicube::bootstrap_cube,
                                 params_total$bootstrap_params)

    # Drop groups (e.g. years) whose bootstrap distribution is undefined or
    # degenerate, such as evenness in a year with fewer than two species, or
    # rarity in a year with a single species. These would otherwise make the
    # CI calculation fail; they simply get NA confidence limits.
    bootstrap_results <- drop_degenerate_bootstraps(
      bootstrap_results,
      params_total$ci_params$grouping_var
    )
    if (length(bootstrap_results) == 0 ||
        (is.data.frame(bootstrap_results) && nrow(bootstrap_results) == 0)) {
      warning(
        paste0(
          "Unable to calculate confidence intervals. There may be ",
          "insufficient data."
        )
      )
      return(indicator)
    }

    # Calculate confidence intervals from bootstrap results
    params_total$ci_params$bootstrap_results <- bootstrap_results
    params_total$ci_params$bootstrap_samples_df <- bootstrap_results
    group_cols <- params_total$ci_params$grouping_var
    ci_df <- do.call(
      dubicube::calculate_bootstrap_ci,
      params_total$ci_params
    ) %>%
      dplyr::select(-dplyr::any_of("est_original"))

    # Join confidence intervals to indicator object
    if (nrow(ci_df) > 0) {

      ## ------------------------------------------------------------------
      ## REVERSE COMPOSITE KEY: Separate group_key back into original cols
      ## ------------------------------------------------------------------
      if ("group_key" %in% names(ci_df)) {
        # We retrieve the original column names from our saved list
        # (Assuming group_cols_original is the vector you had before the unite)
        ci_df <- ci_df %>%
          tidyr::separate_wider_delim(
            cols = "group_key",
            delim = "_",
            names = c("year", "taxonKey"), # Use the original vector here
            cols_remove = TRUE
          )

        # Reset group_cols so the join happens on the original variables
        group_cols <- c("year", "taxonKey")
      }

      # This handles cases where dubicube returns year as character
      if ("year" %in% names(ci_df) && is.numeric(x$year)) {
        ci_df$year <- as.numeric(ci_df$year)
      }
      if ("taxonKey" %in% names(ci_df)) {
        ci_df$taxonKey <- if (is.numeric(x$taxonKey)) as.numeric(ci_df$taxonKey) else as.character(ci_df$taxonKey)
      }
      # Convert negative values to zero as rarity cannot be less than zero
      ci_df$ll <- ifelse(ci_df$ll > 0, ci_df$ll, 0)
      # Join confidence intervals to indicator values by year
      x <- x %>%
        dplyr::full_join(ci_df,
                         by = group_cols)
      indicator$data <- x
      indicator$ci_method <- "cube"
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

#' Evaluate code with a given random seed, restoring the RNG state afterwards
#'
#' The user's random number generator state is left unchanged. If `seed` is
#' `NULL` or `NA`, `code` is evaluated without setting a seed.
#'
#' @param seed Integer random seed, `NULL` or `NA`.
#' @param code Code to evaluate.
#' @noRd
with_rng_seed <- function(seed, code) {
  if (is.null(seed) || is.na(seed)) {
    return(code)
  }
  genv <- globalenv()
  if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = genv, inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = genv), add = TRUE)
  } else {
    on.exit(rm(".Random.seed", envir = genv), add = TRUE)
  }
  set.seed(seed)
  code
}

#' Stop with installation instructions if dubicube is not installed
#' @noRd
check_dubicube_installed <- function() {
  if (!is_package_installed("dubicube")) {
    stop(
      "Cube-level bootstrapping requires the 'dubicube' package, which is ",
      "not installed. Install it with:\n",
      "  install.packages(\"dubicube\", repos = ",
      "c(\"https://b-cubed-eu.r-universe.dev\", ",
      "\"https://cloud.r-project.org\"))\n",
      "or use bootstrap_level = \"indicator\".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Remove bootstrap groups for which confidence intervals are undefined
#'
#' Groups (e.g. years) whose original estimate is not finite, or whose finite
#' bootstrap replicates take fewer than two distinct values, are removed so
#' that the confidence interval calculation does not fail. Works with both a list of
#' 'boot' objects and a data frame of bootstrap replicates.
#'
#' @param bootstrap_results Output of `dubicube::bootstrap_cube()`.
#' @param grouping_var Name(s) of the grouping column(s).
#' @noRd
drop_degenerate_bootstraps <- function(bootstrap_results, grouping_var) {

  est_original <- rep_boot <- NULL

  if (inherits(bootstrap_results, "boot")) {
    bootstrap_results <- list(bootstrap_results)
  }

  if (is.list(bootstrap_results) && !is.data.frame(bootstrap_results)) {
    keep <- vapply(bootstrap_results, function(b) {
      t <- b$t[, 1]
      is.finite(b$t0[1]) && length(unique(t[is.finite(t)])) >= 2
    }, logical(1))
    return(bootstrap_results[keep])
  }

  if (is.data.frame(bootstrap_results) &&
      all(c("est_original", "rep_boot") %in% names(bootstrap_results))) {
    group_cols <- intersect(grouping_var, names(bootstrap_results))
    bootstrap_results <- bootstrap_results %>%
      dplyr::filter(is.finite(est_original) &
                      dplyr::n_distinct(rep_boot[is.finite(rep_boot)]) >= 2,
                    .by = dplyr::all_of(group_cols))
  }

  bootstrap_results
}
