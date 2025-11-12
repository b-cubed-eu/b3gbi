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
