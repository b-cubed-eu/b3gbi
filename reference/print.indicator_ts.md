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
#> Date Range: 1970 - 2023 
#> 
#> Region(s) represented: Denmark 
#> 
#> Coordinate range represented:
#> xmin xmax ymin ymax 
#> 2983 5749 2667 5995 
#> 
#> Number of species represented: 110 
#> Number of families represented: Data not present 
#> 
#> Kingdoms represented: 1 
#> 
#> First 10 rows of data (use n = to show more):
#> 
#> # A tibble: 54 × 2
#>     year diversity_val
#>    <dbl>         <int>
#>  1  1970            14
#>  2  1971             8
#>  3  1972            22
#>  4  1973            21
#>  5  1974            20
#>  6  1975            17
#>  7  1976            18
#>  8  1977            16
#>  9  1978            22
#> 10  1979            22
#> # ℹ 44 more rows
```
