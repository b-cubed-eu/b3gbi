# Create a Longitude/Latitude Bounding Box from MGRS Data

Converts UTM coordinates of MGRS cells, which may span several UTM
zones, to WGS 84 and returns their combined bounding box.

## Usage

``` r
mgrs_to_latlong_bbox(df)
```

## Arguments

- df:

  A data frame with at least three columns: `cellCode` (MGRS codes; the
  first three characters give the UTM zone and latitude band), and
  `xcoord`/`ycoord` (UTM easting and northing).

## Value

An
[`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
object in EPSG:4326 (longitude/latitude).

## Examples

``` r
# \donttest{
df <- data.frame(
  cellCode = c("32UUC", "32UUD"),
  xcoord = c(500000, 501000),
  ycoord = c(5600000, 5601000)
)
bbox <- mgrs_to_latlong_bbox(df)
# }
```
