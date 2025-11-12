# Create a Single Bounding Box from MGRS Data in a Projected CRS

Converts MGRS coordinates to a single sf bounding box in a suitable
projected CRS, handling data that spans multiple UTM zones.

## Usage

``` r
mgrs_to_latlong_bbox(df)
```

## Arguments

- df:

  A data frame with at least three columns: `cellCode` (containing MGRS
  strings), and `xcoord` and `ycoord` for easting and northing.

## Value

An sf bounding box
([`sf::st_bbox`](https://r-spatial.github.io/sf/reference/st_bbox.html))
in a single projected CRS (e.g., Albers).
