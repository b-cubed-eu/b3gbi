# Print a Processed Data Cube Object

Provides a summary representation of a processed_cube object, designed
for user-friendly display in the console.

## Usage

``` r
# S3 method for class 'processed_cube'
print(x, n = 10, ...)
```

## Arguments

- x:

  A processed_cube object.

- n:

  (Optional) Integer specifying the number of rows of cube data to
  display.

- ...:

  Additional arguments.

## Examples

``` r
print(example_cube_1)
#> 
#> Processed data cube for calculating biodiversity indicators
#> 
#> Date Range: 1862 - 2023 
#> Single-resolution cube with cell size 0.25degrees 
#> Number of cells: 323 
#> Grid reference system: eqdgc 
#> Coordinate range:
#>   xmin   xmax   ymin   ymax 
#>  3.375 15.625 54.375 58.125 
#> 
#> Total number of observations: 204886 
#> Number of species represented: 106 
#> Number of families represented: 31 
#> 
#> Kingdoms represented: Animalia 
#> 
#> First 10 rows of data (use n = to show more):
#> 
#> # A tibble: 30,902 × 15
#>     year cellCode  kingdomKey kingdom  familyKey family  taxonKey scientificName
#>    <dbl> <chr>          <dbl> <chr>        <dbl> <chr>      <dbl> <chr>         
#>  1  1862 E009N57DD          1 Animalia      5510 Muridae  5219833 Micromys minu…
#>  2  1863 E009N57DD          1 Animalia      5510 Muridae  5219833 Micromys minu…
#>  3  1870 E009N57DD          1 Animalia      9368 Vesper…  2432439 Myotis dauben…
#>  4  1874 E009N57DD          1 Animalia      9368 Vesper…  2432439 Myotis dauben…
#>  5  1879 E009N57DB          1 Animalia      9368 Vesper…  2432439 Myotis dauben…
#>  6  1881 E008N56AB          1 Animalia      9435 Dipodi…  2439449 Sicista betul…
#>  7  1881 E015N55CC          1 Animalia      9701 Canidae  5219303 Vulpes lagopus
#>  8  1884 E008N56AB          1 Animalia      9435 Dipodi…  2439449 Sicista betul…
#>  9  1884 E014N55DD          1 Animalia      5510 Muridae  2437756 Apodemus flav…
#> 10  1884 E014N55DD          1 Animalia      5510 Muridae  2437760 Apodemus sylv…
#> # ℹ 30,892 more rows
#> # ℹ 7 more variables: obs <dbl>, minCoordinateUncertaintyInMeters <dbl>,
#> #   minTemporalUncertainty <dbl>, familyCount <dbl>, xcoord <dbl>,
#> #   ycoord <dbl>, resolution <chr>
```
