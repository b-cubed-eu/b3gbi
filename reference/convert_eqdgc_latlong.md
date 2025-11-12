# Convert equal-area quarter-degree grid cell codes to latitude and longitude

This is a helper function that takes a vector of EQDGC (Equal-Area
Quarter-Degree Grid Cell) codes and converts them to their central
latitude and longitude coordinates. The function parses the base
coordinates and the nested sub-grid codes to precisely locate the center
of each grid cell.

## Usage

``` r
convert_eqdgc_latlong(cellCode)
```

## Arguments

- cellCode:

  A character vector of EQDGC cell codes, which must follow the
  "direction-base_coord" format (e.g., "W10N34" or "E15S20"). The code
  can also contain additional letter characters for sub-grids (e.g.,
  "W10N34A").

## Value

A matrix with two columns, `lat` and `long`, representing the central
coordinates of each input grid cell.

## Details

The function works by first extracting the base longitude and latitude
coordinates from the cell code, accounting for direction (East/West and
North/South). It then iteratively processes any sub-grid codes (e.g.,
`A`, `B`, `C`, `D`) to refine the coordinates. The final coordinates are
the center point of the most specific grid cell.

## Examples

``` r
# A simple quarter-degree cell
convert_eqdgc_latlong("E10N10")
#>       lat long
#> [1,] 10.5 10.5

# A sub-grid cell
convert_eqdgc_latlong("W-10S34ABCD")
#>            lat      long
#> [1,] -34.21875 -10.65625

# Multiple cell codes
convert_eqdgc_latlong(c("E10N10", "W-10S34DDBA"))
#>            lat      long
#> [1,]  10.50000  10.50000
#> [2,] -34.78125 -10.09375
```
