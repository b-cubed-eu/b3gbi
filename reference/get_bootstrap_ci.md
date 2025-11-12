# Calculate confidence intervals for list of `boot` objects

This function calculates confidence intervals for a list of objects of
class `"boot"` per year into a dataframe containing all required
summaries.

## Usage

``` r
get_bootstrap_ci(bootstrap_list, temporal_list_name = "year", ...)
```

## Arguments

- bootstrap_list:

  A list of objects of class `"boot"` per year.

- temporal_list_name:

  (Optional) The temporal list names of `bootstrap_list` (e.g., year,
  month ...) containing time point values. Default `year`.

- ...:

  Additional argument to be passed to the
  [`boot::boot.ci()`](https://rdrr.io/pkg/boot/man/boot.ci.html)
  function.

## Value

The returned value is a dataframe containing the time point, the type of
interval (`int_type`), the lower limit of the confidence interval
(`ll`), the upper limit of the confidence interval (`ul`), and the
confidence level of the intervals (`conf_level`).
