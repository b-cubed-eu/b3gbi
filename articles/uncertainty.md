# Uncertainty in Biodiversity Indicators

## Introduction 🛡️

Quantifying uncertainty is crucial for robust biodiversity assessments.
The **b3gbi** package provides a flexible and powerful way to calculate
confidence intervals (CIs) for biodiversity indicators using
bootstrapping methods, leveraging the
[**dubicube**](https://github.com/b-cubed-eu/dubicube) package for
robust cube-level resampling.

To maintain a clean and efficient workflow, uncertainty calculation is
decoupled from the initial indicator calculation. This “two-step”
process allows users to first explore their data and indicators quickly,
and then perform computationally intensive bootstrapping only when
needed.

## The Two-Step Workflow

The standard workflow for adding uncertainty to an indicator is as
follows:

1.  **Calculate the Indicator**: Use a time-series wrapper function
    (e.g.,
    [`total_occ_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/total_occ_map.md),
    [`pielou_evenness_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/pielou_evenness_map.md)).
2.  **Add Confidence Intervals**: Pass the resulting indicator object to
    the
    [`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md)
    function.

### Example: Calculating Richness with Uncertainty

In this example, we calculate total occurrences for mammals in Denmark
and then add 95% confidence intervals using cube-level bootstrapping.

``` r

library(b3gbi)

# 1. Process the cube
denmark_cube <- process_cube(system.file("extdata", 
                                         "denmark_mammals_cube_eqdgc.csv", 
                                         package = "b3gbi"))

# 2. Calculate a time series of total occurrences
# By default, CIs are NOT calculated during this step
occ_ts <- total_occ_ts(denmark_cube)

# 3. Add confidence intervals using add_ci()
# This step uses the dubicube package for robust bootstrapping
occ_ts_with_ci <- add_ci(occ_ts, num_bootstrap = 100) # Using 100 for speed in this example

# 4. Plot the result
plot(occ_ts_with_ci, title = "Total Occurrences with 95% CI")
```

## Bootstrapping Levels

The [`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md)
function supports two levels of bootstrapping, selectable via the
`bootstrap_level` argument:

### 1. Cube-Level Bootstrapping (`bootstrap_level = "cube"`)

This is the **default and recommended method**. It resamples the raw
occurrence records within the data cube. The function automatically
determines whether to use group-specific resampling (for species-level
indicators) or whole-cube resampling (for aggregate indicators) based on
the indicator type.

- **Pros**: Mathematically more robust; captures the underlying sampling
  uncertainty of the original data.
- **Cons**: Computationally more intensive, especially for large cubes.

It leverages the `dubicube` package to ensure that indicators are
recalculated correctly for each bootstrap replicate.

### 2. Indicator-Level Bootstrapping (`bootstrap_level = "indicator"`)

This method resamples the calculated indicator values themselves.

- **Pros**: Very fast, even for massive datasets.
- **Cons**: Less robust; assumes that the calculated indicator values
  are independent and identically distributed, which is often not
  strictly true for biodiversity metrics.

## Automatic Indicator Configuration

The [`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md)
function automatically applies appropriate bootstrapping strategies
based on the indicator type. This ensures statistically correct
confidence intervals without requiring manual configuration:

- **Group-Specific Bootstrapping**: This method isolates resampling
  strictly within each grouping variable. For indicators like Total
  Occurrences (`total_occ`), which calculate a raw count per year,
  resampling only within that year provides mathematically correct
  variance without cross-contamination between years.

- **Whole-Cube Bootstrapping**: This method resamples the entire dataset
  at once. This is automatically applied to aggregate indicators
  (evenness, rarity, density metrics) as well as species-level
  indicators (`spec_occ`, `spec_range`). For species-level indicators,
  whole-cube resampling is computationally optimized to process
  thousands of species simultaneously in a single pass while correctly
  capturing changes in species composition during resampling.

- **Bounded Indicators**: Evenness metrics (`pielou_evenness`,
  `williams_evenness`) automatically use logit transformation to ensure
  confidence intervals remain within valid \[0, 1\] bounds.

These defaults are applied internally, but can be overridden using
`boot_args` if needed.

## Advanced Configuration

The [`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md)
function provides several arguments for fine-tuning the CI calculation:

| Argument | Description | Default |
|----|----|----|
| `num_bootstrap` | Number of bootstrap replicates. | `1000` |
| `ci_type` | Type of bootstrap interval. See [Types of Confidence Intervals](#types-of-confidence-intervals) below. | `"perc"` |
| `confidence_level` | The confidence level (e.g., 0.95). | `0.95` |
| `boot_args` | A list of additional arguments for [`dubicube::bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.html). | [`list()`](https://rdrr.io/r/base/list.html) |
| `ci_args` | A list of additional arguments for [`dubicube::calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.html). | [`list()`](https://rdrr.io/r/base/list.html) |

## Types of Confidence Intervals

The `ci_type` argument allows you to choose between different methods
for calculating bootstrap confidence intervals. These are passed
directly to the `dubicube` package:

- **Percentile (`"perc"`)**: (Default) The simplest and most commonly
  used method. It uses the ordered bootstrap replicates to find the
  intervals (e.g., the 2.5th and 97.5th percentiles for a 95% CI). It is
  transformation-invariant and generally robust for biodiversity
  indicators.
- **Bias-Corrected and Accelerated (`"bca"`)**: An improvement over the
  percentile method that adjusts for both bias and skewness in the
  bootstrap distribution. It is highly recommended for accuracy but
  requires more computation.
- **Normal Approximation (`"norm"`)**: Assumes the bootstrap
  distribution is approximately normal. It uses the standard error of
  the bootstrap replicates to calculate the interval. While fast, it may
  be less accurate for skewed distributions typical of biodiversity
  metrics.
- **Basic (`"basic"`)**: Also known as the “reverse percentile” method.
  It uses the percentiles of the distribution of the difference between
  the bootstrap replicates and the original estimate.

### Customizing the Bootstrap Process

If you need to pass specific parameters to the underlying `dubicube`
functions (e.g., setting a specific seed), you can use the `boot_args`
and `ci_args` parameters:

``` r

occ_ts_custom <- add_ci(occ_ts, 
                        num_bootstrap = 500,
                        boot_args = list(seed = 42),
                        ci_args = list(type = "perc"))
```

### Overriding Indicator Defaults

While
[`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md)
automatically selects appropriate settings, you can override these
defaults using `boot_args` and `ci_args`. This is useful when you need
specialized behavior:

``` r

# Example: Calculate CIs for evenness on raw scale (no logit transformation)
evenness_raw <- add_ci(my_evenness_indicator,
                       num_bootstrap = 1000,
                       boot_args = list(trans = identity, 
                                        inv_trans = identity))

# Example: Force bias correction for total_occ
occ_with_bias <- add_ci(my_occ_indicator,
                        num_bootstrap = 1000,
                        ci_args = list(no_bias = FALSE))
```

Common override parameters include: - `trans` / `inv_trans`:
Transformation functions - `no_bias`: Logical, disable bias correction
(default varies by indicator) - `group_specific`: Logical, force
group-specific vs whole-cube bootstrapping

## Supported Indicators

Confidence intervals can be added post-hoc for the following indicators:

- Total Occurrences (`total_occ`)
- Occurrence Density (`occ_density`)
- Newness (`newness`)
- Williams Evenness (`williams_evenness`)
- Pielou Evenness (`pielou_evenness`)
- Abundance-based Rarity (`ab_rarity`)
- Area-based Rarity (`area_rarity`)
- Species Occurrences (`spec_occ`)
- Species Range (`spec_range`)

### Special Case: Hill Diversity Indicators

While most indicators rely on `dubicube` for calculating confidence
intervals, the Hill diversity indicators (`hill0`, `hill1`, `hill2`)
compute their metrics using the `iNEXT` package, which runs an intensive
coverage-based rarefaction algorithm. `iNEXT` possesses its own highly
optimized, native bootstrapping engine.

When you pass a Hill indicator to
[`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md),
the function will automatically detect it, bypass `dubicube`’s
cube-level resampling to prevent computationally disastrous
double-bootstrapping, and seamlessly delegate to `iNEXT`’s internal
engine (forcing `bootstrap_level = "indicator"`).

### Exclusions

Certain indicators cannot have confidence intervals added via
[`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md):

- **Observed Richness** (`obs_richness`): Bootstrapping observed
  richness is often not statistically sensible; consider using Hill
  numbers for estimated richness instead.
- **Cumulative Richness** (`cum_richness`): The cumulative nature of
  this metric makes standard bootstrapping inappropriate.
- **Occurrence Turnover** (`occ_turnover`): Similar to cumulative
  richness, the temporal dependency requires specialized methods.
- **Taxonomic Distinctness** (`tax_distinct`): Requires specialized
  randomization tests rather than standard bootstrapping.

## Data Exploration with dubicube 🔍

While **b3gbi** focuses on indicator calculation and visualization, the
underlying **dubicube** package offers powerful functions for initial
data exploration and quality assessment of your biodiversity data cubes.

To learn more about the full capabilities of **dubicube**, including
in-depth tutorials and advanced data processing techniques, please visit
the [dubicube
documentation](https://docs.b-cubed.eu/software/dubicube/readme/).

## Summary

The [`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md)
function provides a unified and robust interface for adding uncertainty
to your biodiversity assessments. By leveraging the power of `dubicube`,
**b3gbi** ensures that your results are not just numbers, but
scientifically grounded estimates with clearly defined confidence
boundaries.

With automatic indicator configuration, most users can rely on sensible
defaults while advanced users retain full control over the bootstrapping
process through the `boot_args` and `ci_args` parameters.
