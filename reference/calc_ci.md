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
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = "perc", ...)

# S3 method for class 'occ_density'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = "perc", ...)

# S3 method for class 'spec_richness_density'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = "perc", ...)

# S3 method for class 'newness'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = "perc", ...)

# S3 method for class 'williams_evenness'
calc_ci(x, ...)

# S3 method for class 'pielou_evenness'
calc_ci(x, ...)

# S3 method for class 'ab_rarity'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = "perc", ...)

# S3 method for class 'area_rarity'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = "perc", ...)

# S3 method for class 'spec_occ'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = "perc", ...)

# S3 method for class 'spec_range'
calc_ci(x, indicator, num_bootstrap = 1000, ci_type = "perc", ...)

# S3 method for class 'hill0'
calc_ci(x, indicator, num_bootstrap = 1000, ...)

# S3 method for class 'hill1'
calc_ci(x, indicator, num_bootstrap = 1000, ...)

# S3 method for class 'hill2'
calc_ci(x, indicator, num_bootstrap = 1000, ...)
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
  (Default: "perc". Select "none" to avoid calculating bootstrap CIs.)

## Value

A data frame containing indicator values with calculated lower (`ll`)
and upper (`ul`) confidence bounds.

## Methods (by class)

- `calc_ci(total_occ)`: Calculate confidence intervals for total
  occurrences

- `calc_ci(occ_density)`: Calculate confidence intervals for occurrence
  density

- `calc_ci(spec_richness_density)`: Calculate confidence intervals for
  species richness density

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

- `calc_ci(hill0)`: Calculate confidence intervals for Hill0 (Richness)

- `calc_ci(hill1)`: Calculate confidence intervals for Hill1 (Shannon)

- `calc_ci(hill2)`: Calculate confidence intervals for Hill2 (Simpson)

## Examples

``` r
# \donttest{
# calc_ci() is called automatically when confidence intervals are requested
# during indicator calculation (or by add_ci(bootstrap_level = "indicator"))
occ_ts <- total_occ_ts(example_cube_1, first_year = 2000,
                       ci_type = "perc", num_bootstrap = 100)
head(occ_ts$data)
#> # A tibble: 6 × 9
#>    year diversity_val int_type    ll    ul est_boot se_boot bias_boot conf_level
#>   <dbl>         <dbl> <chr>    <dbl> <dbl>    <dbl>   <dbl>     <dbl>      <dbl>
#> 1  2000          2166 percent  1866. 2632.    2189.    186.     23.3        0.95
#> 2  2001          2831 percent  2579. 3098.    2820.    137.    -10.9        0.95
#> 3  2002          3366 percent  2988. 3880.    3364.    225.     -2.31       0.95
#> 4  2003          3114 percent  2629. 3722.    3120.    258.      6.15       0.95
#> 5  2004          2934 percent  2534. 3487.    2997.    227.     63.0        0.95
#> 6  2005          4733 percent  3994. 5378.    4770.    329.     36.6        0.95
# }
```
