# Add Confidence Intervals to an Indicator Object

This function calculates bootstrap confidence intervals for an existing
`indicator_ts` object. It supports both cube-level bootstrapping
(resampling occurrence records) and indicator-level bootstrapping
(resampling calculated values), allowing for advanced transformations
during the CI calculation process.

## Usage

``` r
add_ci(
  indicator,
  num_bootstrap = 1000,
  bootstrap_level = c("cube", "indicator"),
  ci_type = c("perc", "bca", "norm", "basic", "none"),
  trans = function(t) t,
  inv_trans = function(t) t,
  confidence_level = 0.95,
  overwrite = TRUE,
  boot_args = list(),
  ci_args = list(),
  seed = 123,
  ...
)
```

## Arguments

- indicator:

  An object of class `indicator_ts` to which confidence intervals should
  be added.

- num_bootstrap:

  (Optional) Number of bootstrap replicates to perform. (Default: 1000)

- bootstrap_level:

  (Optional) Level at which to perform bootstrapping:

  - `cube` (default): Bootstrapping is done by resampling the occurrence
    records in the cube. This is mathematically more robust as it
    captures the underlying sampling uncertainty.

  - `indicator`: Bootstrapping is done by resampling indicator values.
    This is faster for large cubes but less robust.

- ci_type:

  (Optional) Type of bootstrap confidence intervals to calculate.
  (Default: `"perc"`). Supported options are:

  - `perc`: Percentile intervals.

  - `bca`: Bias-corrected and accelerated intervals.

  - `norm`: Normal approximation intervals.

  - `basic`: Basic bootstrap intervals.

  - `none`: No confidence intervals calculated.

- trans:

  (Optional) A function for transforming the indicator values before
  calculating confidence intervals (e.g., `log`). (Default: identity
  function)

- inv_trans:

  (Optional) The inverse of the transformation function `trans` (e.g.,
  `exp`). Used to back-transform the intervals to the original scale.
  (Default: identity function)

- confidence_level:

  (Optional) The confidence level for the calculated intervals (e.g.,
  0.95 for 95% CIs). (Default: 0.95)

- overwrite:

  (Optional) Logical. If the indicator already contains confidence
  intervals (`ll` and `ul` columns), should they be replaced? (Default:
  TRUE)

- boot_args:

  (Optional) Named list of additional arguments passed to
  [`dubicube::bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.html).
  (Default: [`list()`](https://rdrr.io/r/base/list.html))

- ci_args:

  (Optional) Named list of additional arguments passed to
  [`dubicube::calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.html).
  (Default: [`list()`](https://rdrr.io/r/base/list.html))

- seed:

  (Optional) Integer. Random seed for bootstrapping. (Default: 123)

- ...:

  (Optional) Additional arguments passed to calc_ci().

## Value

An updated object of class `indicator_ts` containing the original data
with the following additional columns:

- `ll`: Lower limit of the confidence interval.

- `ul`: Upper limit of the confidence interval.

- `est_boot`: The bootstrap estimate of the indicator value.

- `se_boot`: The bootstrap standard error.

- `bias_boot`: The bootstrap estimate of bias.

- `int_type`: The type of interval calculated (e.g., 'perc').

- `conf`: The confidence level used.

## Details

The function acts as a bridge to the dubicube package to calculate
bootstrap confidence intervals.

### Indicator-specific defaults

Depending on the indicator, default settings are internally applied when
calculating bootstrap confidence intervals. These defaults control
whether bootstrapping is performed per group, which transformation is
used, and whether bias correction is disabled.

The following defaults are used unless explicitly overridden via
`trans`, `inv_trans`, `boot_args`, or `ci_args`:

- **`total_occ`**

  - Group-specific bootstrapping: **yes**

  - Transformation: **none (identity)**

  - Bias correction: **disabled** (`no_bias = TRUE`)

- **`spec_occ`, `spec_range`**

  - Group-specific bootstrapping: **yes**

  - Transformation: **none (identity)**

  - Bias correction: enabled

- **`pielou_evenness`, `williams_evenness`**

  - Group-specific bootstrapping: **no**

  - Transformation: **logit**

  - Inverse transformation: **inverse logit**

  - Bias correction: enabled

- **`occ_density`, `ab_rarity`, `area_rarity`, `newness`**

  - Group-specific bootstrapping: **no**

  - Transformation: **none (identity)**

  - Bias correction: enabled

Group-specific bootstrapping means that resampling is performed within
each group (e.g., species or year), which is required for indicators
that are inherently group-based. This in contrast to whole-cube
bootstrapping where resampling is performed across the whole dataset;
applicable for indicators that combine information across groups

Transformations are applied prior to confidence interval calculation and
inverted afterwards to return intervals on the original scale.

### Indicators outside scope of this function

For certain indicators, confidence intervals cannot be calculated or
added post-hoc because they are not statistically relevant, or because
proper uncertainty estimation is not supported. In such cases, a warning
is issued and the original object is returned. The following indicators
cannot have confidence intervals added via `add_ci()`:

- `obs_richness`, `spec_richness_density`: Observed species richness and
  richness density are highly sensitive to sampling effort and sample
  size. Furthermore, bootstrapping observed occurrences can never
  discover new/unobserved species, meaning resampled richness estimates
  are always less than or equal to the observed richness. This results
  in confidence intervals that lie entirely at or below the observed
  value (i.e., the upper bound cannot exceed the observed richness).
  Therefore, post-hoc bootstrapping is not a statistically sound way to
  estimate their uncertainty (Hill numbers `hill0`, `hill1`, `hill2`
  should be used instead for proper rarefaction/extrapolation).

- `completeness`: Sample completeness (coverage) is calculated as a
  deterministic sample statistic using `iNEXT` methods, and confidence
  interval calculation is not supported.

- `relative_occupancy`: Standard bootstrapping is not supported because
  resampling strips spatial attributes (like `total_num_cells` or
  `total_area_sqkm`) necessary for occupancy calculation. These spatial
  attributes cannot be regenerated from the resampled occurrences alone,
  as occurrences only capture cells/regions where species were detected
  and contain no information about empty grid cells (zero occurrences)
  in the study area.

- `cum_richness` (Cumulative Species Richness): This is an inherently
  temporal and sequential indicator for which CIs are not generated, as
  its nature as an accumulating count over time is not well-suited to
  standard bootstrapping methods for uncertainty.

- `occ_turnover` (Species Turnover): Confidence intervals are not
  calculated for this indicator. The metric's reliance on comparing the
  unique species lists between consecutive time steps means that
  bootstrapping individual observations within each time step would
  introduce artificial variability in these lists. This makes the
  resulting gains and losses highly unstable and would lead to
  unreliable or biased confidence intervals for the turnover metric.

- `tax_distinct` (Taxonomic Distinctness): Confidence intervals are not
  calculated for this indicator. Due to its high sensitivity to the
  exact species composition and taxonomic relationships within a sample,
  bootstrapping individual occurrences can introduce significant
  artificial variability, leading to unreliable or biased confidence
  intervals.

*Note:* For Hill numbers (`hill0`, `hill1`, `hill2`), cube-level
bootstrapping is not supported natively, but `add_ci()` will
automatically switch to indicator-level bootstrapping and calculate
confidence intervals internally using the `iNEXT` package.

## See also

[`dubicube::bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.html),
[`dubicube::calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.html)
