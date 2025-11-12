# Calculate Confidence Intervals for a Biodiversity Indicator

This function calculates bootstrap confidence intervals for a
biodiversity indicator. It is called automatically when calculating a
biodiversity indicator over time unless you choose 'none' for ci_type.

## Usage

``` r
calc_ci(x, indicator, ...)

# Default S3 method
calc_ci(x, indicator, ...)

# S3 method for class 'total_occ'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = ci_type, ...)

# S3 method for class 'occ_density'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = ci_type, ...)

# S3 method for class 'newness'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = ci_type, ...)

# S3 method for class 'williams_evenness'
calc_ci(x, ...)

# S3 method for class 'pielou_evenness'
calc_ci(x, ...)

# S3 method for class 'ab_rarity'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = ci_type, ...)

# S3 method for class 'area_rarity'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = ci_type, ...)

# S3 method for class 'spec_occ'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = ci_type, ...)

# S3 method for class 'spec_range'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = ci_type, ...)
```

## Arguments

- x:

  A data cube object

- indicator:

  An indicator calculated over time, in the form of a data frame.
  \*Note: this should NOT be an 'indicator_ts' object as it is meant to
  be called by the 'compute_indicator_workflow' function.

- ...:

  Additional arguments passed to specific calc_ci functions.

- num_bootstrap:

  (Optional) Set the number of bootstraps to calculate for generating
  confidence intervals. (Default: 1000)

- ci_type:

  (Optional) Type of bootstrap confidence intervals to calculate.
  (Default: "norm". Select "none" to avoid calculating bootstrap CIs.)

## Methods (by class)

- `calc_ci(total_occ)`: Calculate confidence intervals for total
  occurrences

- `calc_ci(occ_density)`: Calculate confidence intervals for occurrence
  density

- `calc_ci(newness)`: Calculate confidence intervals for newness

- `calc_ci(williams_evenness)`: Calculate confidence intervals for
  Williams' evenness

- `calc_ci(pielou_evenness)`: Calculate confidence intervals for
  Pielou's evenness

- `calc_ci(ab_rarity)`: Calculate confidence intervals for
  abundance-based rarity

- `calc_ci(area_rarity)`: Calculate confidence intervals for area-based
  rarity

- `calc_ci(spec_occ)`: Calculate confidence intervals for species
  occurrences

- `calc_ci(spec_range)`: Calculate confidence intervals for species
  range
