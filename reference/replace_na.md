# Replace NA Values in Indicator Objects

This function takes an `indicator_map` or `indicator_ts` object and
replaces all NA values in the `diversity_val` column with zeros. This is
useful when NA values represent areas or years with no data that should
be treated as zero for mapping or analysis purposes.

## Usage

``` r
replace_na(x)
```

## Arguments

- x:

  An `indicator_map` or `indicator_ts` object containing a
  `diversity_val` column with NA values to replace.

## Value

The input indicator object with NA values in `diversity_val` replaced by
zero.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'result' is an indicator_map or indicator_ts object
result_filled <- replace_na(result)
} # }
```
