# Print an Indicator Map Object

Provides a summary representation of an indicator_map object, designed
for user-friendly display in the console.

## Usage

``` r
# S3 method for class 'indicator_map'
print(x, n = 10, include_na = FALSE, ...)
```

## Arguments

- x:

  An indicator_map object.

- n:

  Integer specifying the number of rows of data to display.

- include_na:

  Logical. If TRUE, includes rows with NA diversity values in the
  printed output. Default is FALSE.

- ...:

  Additional arguments passed to print().

## Value

Invisibly returns the input object `x`.

## Examples

``` r
print(example_indicator_map1)
#> Gridded biodiversity indicator map
#> 
#> Name of Indicator: Observed Species Richness 
#> 
#> Projected CRS: EPSG:4326 
#> 
#> Coordinate range:
#>  xmin  ymin  xmax  ymax 
#>  3.25 54.25 15.75 58.25 
#> 
#> Grid cell size: 0.25 degrees 
#> Number of cells: 800 
#> 
#> Observation years: 1862 - 2024 
#> Total years with observations: 126 
#> 
#> Number of species represented: 106 
#> Number of families represented: 31 
#> 
#> Kingdoms represented: Animalia 
#> 
#> Map layers:  
#> 
#> First 10 rows of data (use n = to show more):
#> 
#> # A tibble: 323 × 4
#>    cellid   area cellCode  diversity_val
#>     <int> [km^2] <chr>             <int>
#>  1     34   451. E011N54DA             1
#>  2     35   451. E011N54DB             1
#>  3     36   451. E012N54CA             1
#>  4     79   448. E010N54AD             1
#>  5     80   448. E010N54BC            21
#>  6     81   448. E010N54BD             1
#>  7     82   448. E011N54AC             7
#>  8     83   448. E011N54AD            30
#>  9     84   448. E011N54BC            40
#> 10     85   448. E011N54BD            43
#> # ℹ 313 more rows
```
