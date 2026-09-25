# Plot Biodiversity Indicator Map

Creates a map visualization of a calculated multi-species biodiversity
indicator. Requires an indicator_map object as input. To plot
single-species indicators, use the
[`plot_species_map()`](https://b-cubed-eu.github.io/b3gbi/reference/plot_species_map.md)
function instead.

## Usage

``` r
plot_map(
  x,
  title = "auto",
  auto_title = NULL,
  leg_label_default = NULL,
  xlims = NULL,
  ylims = NULL,
  trans = NULL,
  bcpower = NULL,
  breaks = NULL,
  labels = NULL,
  output_crs = NULL,
  crop_to_grid = NULL,
  crop_by_region = FALSE,
  ocean_fill_colour = NULL,
  land_fill_colour = NULL,
  grid_fill_colour = NULL,
  grid_line_colour = NULL,
  grid_line_width = NULL,
  grid_fill_transparency = NULL,
  grid_line_transparency = NULL,
  legend_title = NULL,
  legend_limits = NULL,
  legend_title_wrap_length = 10,
  title_wrap_length = 60,
  visible_gridlines = TRUE,
  visible_panel_gridlines = FALSE,
  map_expansion_factor = 0.1,
  layers = NULL,
  layer_colours = NULL,
  layer_fill_colours = NULL,
  scale = c("medium", "small", "large"),
  filter_outliers = FALSE
)
```

## Arguments

- x:

  An 'indicator_map' object containing multi-species indicator values
  associated with map grid cells. This is a required parameter with no
  default.

- title:

  (Optional) Plot title. Replace "auto" with your own title if you want
  a custom title or if calling the function manually.

- auto_title:

  (Optional) Text for automatic title generation, provided by an
  appropriate S3 method (if calling the function manually, leave as
  NULL).

- leg_label_default:

  (Optional) Default label for the legend, provided by an appropriate S3
  method (if calling the function manually, leave as NULL).

- xlims:

  (Optional) Custom longitude limits in decimal degrees (WGS84), as
  c(min, max). Should be supplied together with ylims. If only xlims is
  supplied, the values are instead interpreted in the units of the
  indicator_map's coordinate reference system and combined with the
  map's own y limits.

- ylims:

  (Optional) Custom latitude limits in decimal degrees (WGS84), as
  c(min, max). Should be supplied together with xlims. If only ylims is
  supplied, the values are instead interpreted in the units of the
  indicator_map's coordinate reference system and combined with the
  map's own x limits.

- trans:

  (Optional) Scale transformation for the fill gradient. Can be any
  transformation accepted by
  [`ggplot2::scale_fill_gradient()`](https://ggplot2.tidyverse.org/reference/scale_gradient.html)
  (e.g., 'log', 'log10' or 'sqrt'), or one of the special values
  'boxcox', 'modulus' or 'yj' (Yeo-Johnson), which use the power
  parameter given in bcpower.

- bcpower:

  (Optional) Power parameter for the Box-Cox, modulus, or Yeo-Johnson
  transformations (used only when trans is 'boxcox', 'modulus' or 'yj').

- breaks:

  (Optional) Break points for the legend scale.

- labels:

  (Optional) Labels for legend scale break points.

- output_crs:

  (Optional) Coordinate Reference System (CRS) for the output map. Can
  be specified as an EPSG code (e.g., 4326) or a proj4string. If NULL
  (default), the original CRS of the indicator_map object will be used.

- crop_to_grid:

  (Optional) If TRUE, the grid will determine the edges of the map. If
  FALSE, a buffer will be added around the grid. If NULL (default), will
  be set to TRUE if map_level is "cube", otherwise FALSE.

- crop_by_region:

  (Optional) If TRUE, the map extent is set to the bounding box of the
  region that was specified when calculating the indicator_map (e.g. the
  country or continent), instead of the extent of the grid. This
  requires that a region was specified, i.e. that the indicator_map was
  not calculated with level = "cube" or level = "world". Default is
  FALSE.

- ocean_fill_colour:

  (Optional) Colour for the ocean (plot background) outside of the grid.
  Default is "#92c5f0" (light blue).

- land_fill_colour:

  (Optional) Colour for the land area outside of the grid. Default is
  "grey85".

- grid_fill_colour:

  (Optional) Colour for empty grid cells (non-empty grid cells will be
  coloured according to their indicator value). Default is
  "transparent".

- grid_line_colour:

  (Optional) Colour for the grid lines. Default is "black". If
  visible_gridlines is set to FALSE, this setting will have no effect.

- grid_line_width:

  (Optional) Width of the grid lines. If NULL (default), 0.5 for ISEA3H
  grids and 0.1 otherwise.

- grid_fill_transparency:

  (Optional) Transparency of the grid fill colour for empty grid cells
  (0 = fully transparent, 1 = fully opaque). If visible_gridlines is set
  to TRUE, default is 0.2. Otherwise, default is 0. Note that this
  setting does NOT apply to grid cells with indicator values, and has no
  visible effect while grid_fill_colour is "transparent".

- grid_line_transparency:

  (Optional) Transparency of the grid line colour (0 = fully
  transparent, 1 = fully opaque). Default is 0.5. If visible_gridlines
  is set to FALSE, this setting will have no effect. \*Note that this
  setting does NOT apply to the grid outline!

- legend_title:

  (Optional) Title for the plot legend.

- legend_limits:

  (Optional) Limits for the legend scale.

- legend_title_wrap_length:

  (Optional) Maximum legend title length before wrapping to a new line.

- title_wrap_length:

  (Optional) Maximum title length before wrapping to a new line.

- visible_gridlines:

  (Optional) Show gridlines between cells. Default is TRUE.

- visible_panel_gridlines:

  (Optional) Show ggplot panel gridlines. Default is FALSE.

- map_expansion_factor:

  (Optional) Factor to expand the map limits beyond the grid limits.
  This does NOT expand the boundaries of the plot, it only affects where
  the crop is applied. If this value is too small, some land may be
  visibly cut off due to map distortion caused by projections. A larger
  value will extend the bounding box for cropping to prevent this. Must
  be a positive number. Default is 0.1. This should be enough for most
  projections, but you can increase this value if you are using an
  extreme projection and find that some land is visibly cut off.

- layers:

  (Optional) Additional rnaturalearth layers to plot, e.g. c("reefs",
  "playas").

- layer_colours:

  (Optional) Outline colours for the additional layers, given in the
  same order as 'layers' (one colour per layer; must be the same length
  as 'layers'). If NULL (default), all layer outlines are black.

- layer_fill_colours:

  (Optional) Fill colours for the additional layers, given in the same
  order as 'layers' (must be the same length as 'layers'). If NULL
  (default), layers are unfilled, except "ocean" and "lakes", which are
  filled light blue.

- scale:

  (Optional) Scale of Natural Earth data ("small", "medium", or
  "large"). Default is 'medium'.

- filter_outliers:

  (Optional) If TRUE, removes geographical outliers from the plot extent
  using an Interquartile Range (IQR) method based on the centroid
  coordinates of the indicator_map cells. This is particularly useful
  for discrete global grids like ISEA3H where stray data points from
  coordinate errors can force the map bounds to expand globally. Default
  is FALSE.

## Value

A ggplot object representing the biodiversity indicator map. Can be
customized using ggplot2 functions.

## Examples

``` r
# \donttest{
evenness_map <- pielou_evenness_map(example_cube_1,
  level = "country",
  region = "Denmark"
)
plot_map(
  x = evenness_map,
  title = "Map of Species Evenness in Denmark",
  legend_title = "Evenness"
)

# }
```
