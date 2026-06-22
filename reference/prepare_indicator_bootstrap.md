# Prepare bootstrap and confidence interval parameters for an indicator

This function prepares the argument lists for
[`dubicube::bootstrap_cube()`](https://b-cubed-eu.github.io/dubicube/reference/bootstrap_cube.html)
and
[`dubicube::calculate_bootstrap_ci()`](https://b-cubed-eu.github.io/dubicube/reference/calculate_bootstrap_ci.html)
based on the indicator definition. Behaviour (grouping, bootstrap
method, transformations, bias correction) is fully controlled by a rule
book keyed on `indicator$div_type`.

## Usage

``` r
prepare_indicator_bootstrap(
  indicator,
  num_bootstrap,
  ci_type,
  seed = 123,
  trans = function(t) t,
  inv_trans = function(t) t,
  confidence_level = 0.95,
  expected_years = NULL,
  boot_args = list(),
  ci_args = list()
)
```

## Arguments

- indicator:

  A list describing the indicator. Must contain at least `div_type` and
  `raw_data`.

- num_bootstrap:

  Integer. Number of bootstrap samples.

- ci_type:

  Character. Type of confidence interval.

- seed:

  Optional integer. Random seed for bootstrapping. (Default: 123)

- trans:

  Optional transformation function applied to the statistic. Will be
  overridden if specified by the indicator rule book.

- inv_trans:

  Optional inverse transformation function. Will be overridden if
  specified by the indicator rule book.

- confidence_level:

  Numeric between 0 and 1. Confidence level.

- expected_years:

  (Optional) Vector of years expected in the output. Used to ensure
  consistent data frame structure during bootstrapping.

- boot_args:

  Named list of additional arguments passed to `bootstrap_cube()`,
  overriding defaults.

- ci_args:

  Named list of additional arguments passed to
  `calculate_bootstrap_ci()`, overriding defaults.

## Value

A named list with two elements:

- bootstrap_params:

  List of parameters for `bootstrap_cube()`

- ci_params:

  List of parameters for `calculate_bootstrap_ci()`

## Details

No computation is performed; the function only returns parameter lists.

## Examples

``` r
if (FALSE) { # \dontrun{
params <- prepare_indicator_bootstrap(
  indicator = indicator,
  num_bootstrap = 1000,
  ci_type = "bca"
)
} # }
```
