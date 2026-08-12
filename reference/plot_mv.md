# Plot Interactive Biodiversity Indicator Map

Creates an interactive map of a calculated multi-species biodiversity
indicator. Requires an indicator_map object as input.

## Usage

``` r
plot_mv(
  x,
  legend_title = NULL,
  transparency = 0.8,
  filter_NA = TRUE,
  basemap_list = c("OpenStreetMap", "OpenTopoMap", "CartoDB.Positron",
    "Esri.WorldImagery"),
  ...
)
```

## Arguments

- x:

  An 'indicator_map' object containing multi-species indicator values
  associated with map grid cells. This is a required parameter with no
  default.

- legend_title:

  (Optional) Legend title. Add your own custom legend title if you don't
  like the automatically generated one. If set to the default of NULL,
  the function will provide a legend title based on the indicator being
  plotted.

- transparency:

  (Optional) Change the transparency of the grid fill. Default is 0.8. A
  value of 0 is full transparency, 1 is solid fill.

- filter_NA:

  (Optional) Filter out NA values so NA grid cells do not appear.
  Default is TRUE.

- basemap_list:

  (Optional) A list of basemaps you want mapview to display as options
  to select from. The first one in the list will be shown when the map
  loads. Default is c("OpenStreetMap", "OpenTopoMap",
  "CartoDB.Positron", "Esri.WorldImagery").

- ...:

  (Optional) Pass additional parameters to the mapview package.

## Value

An interactive mapview plot.

## Examples

``` r
if (FALSE) { # \dontrun{
cube_path <- system.file("extdata", "denmark_mammals_cube_eea.csv", package = "b3gbi")
cube <- process_cube(cube_path)
map_rich <- obs_richness_map(cube, level = "country", region = "Denmark")
plot_mv(map_rich)
} # }
```
