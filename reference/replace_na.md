# Replace NA Values in Indicator Objects

This function takes an `indicator_map` or `indicator_ts` object and
replaces all NA values in the `diversity_val` column with zeros. This is
useful when NA values represent areas or years with no data that should
be treated as zero for mapping or analysis purposes. Only use it when
zero is a meaningful value for the indicator: for indicators such as
evenness, NA means the value is undefined (e.g. fewer than two species),
not zero.

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
zero. Stops with an error if there are no NA values.

## Examples

``` r
# Observed richness is NA in cells without occurrences, where zero species
# were observed
or_map_filled <- replace_na(example_indicator_map1)
#> Replaced 477 NA values with zeroes.
```
