# Create an sf object from UTM coordinates, handling multiple zones correctly.

This function takes a data frame with UTM coordinates (xcoord, ycoord)
and a utmzone column, and creates an sf object with the correct CRS for
each zone.

## Usage

``` r
create_sf_from_utm(df, output_crs = NULL)
```

## Arguments

- df:

  A data frame with columns: xcoord, ycoord, and utmzone.

- output_crs:

  (Optional) The EPSG code or CRS string for the desired output CRS. If
  NULL, the CRS of the first UTM zone will be used.

## Value

An sf object with the geometry correctly defined for each UTM zone.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  xcoord = c(500000, 501000),
  ycoord = c(5600000, 5601000),
  utmzone = c(32, 32),
  hemisphere = c("North", "North")
)
sf_obj <- create_sf_from_utm(df, output_crs = "EPSG:4326")
} # }
```
