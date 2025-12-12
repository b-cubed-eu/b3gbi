# Print an Indicator Time Series Object

Provides a summary representation of an indicator_ts object, designed
for user-friendly display in the console.

## Usage

``` r
# S3 method for class 'indicator_ts'
print(x, n = 10, ...)
```

## Arguments

- x:

  An indicator_ts object.

- n:

  (Optional) Integer specifying the number of rows of data to display.

- ...:

  Additional arguments.

## Examples

``` r
print(example_indicator_ts1)
#> Biodiversity indicator time series
#> 
#> Name of indicator: Observed Species Richness 
#> 
#> Date Range: 1970 - 2024 
#> 
#> Region(s) represented: cube 
#> 
#> Coordinate range represented:
#>  xmin  xmax  ymin  ymax 
#>  3.25 15.75 54.25 58.25 
#> 
#> Number of species represented: 106 
#> Number of families represented: 31 
#> 
#> Kingdoms represented: Animalia 
#> 
#> First 10 rows of data (use n = to show more):
#> 
#> # A tibble: 55 × 2
#>     year diversity_val
#>    <dbl>         <int>
#>  1  1970            24
#>  2  1971             7
#>  3  1972            19
#>  4  1973            12
#>  5  1974            11
#>  6  1975            11
#>  7  1976            12
#>  8  1977            12
#>  9  1978            26
#> 10  1979            15
#> # ℹ 45 more rows
```
