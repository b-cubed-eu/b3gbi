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

  A data frame with numeric columns `xcoord` (easting), `ycoord`
  (northing) and `utmzone`, and a character column `hemisphere` (`"S"`
  for the southern hemisphere; any other value is treated as northern).

- output_crs:

  (Optional) The EPSG code or CRS string for the desired output CRS. If
  NULL, the CRS of the first UTM zone will be used.

## Value

A single sf object, with the points of each zone transformed to
`output_crs`.

## Examples

``` r
# \donttest{
df <- data.frame(
  xcoord = c(500000, 501000),
  ycoord = c(5600000, 5601000),
  utmzone = c(32, 32),
  hemisphere = c("North", "North")
)
sf_obj <- create_sf_from_utm(df, output_crs = "EPSG:4326")
# }
```
