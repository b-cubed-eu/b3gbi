# Calculate Occupancy Turnover

This function calculates occupancy turnover as a time series (see
'Details' for more information).

## Usage

``` r
occ_turnover_ts(data, ...)
```

## Arguments

- data:

  A data cube object (class 'processed_cube').

- ...:

  Arguments passed on to
  [`compute_indicator_workflow`](https://b-cubed-eu.github.io/b3gbi/reference/compute_indicator_workflow.md)

  `ci_type`

  :   (Optional) Type of bootstrap confidence intervals to calculate.
      (Default: "norm"). Select "none" to avoid calculating bootstrap
      CIs.

  `cell_size`

  :   (Optional) Length of grid cell sides, in km or degrees. If set to
      "grid" (default), this will use the existing grid size of your
      cube. If set to "auto", this will be automatically determined
      according to the geographical level selected. This is 100 km or 1
      degree for 'continent' or 'world', 10 km or (for a degree-based
      CRS) the native resolution of the cube for 'country',
      'sovereignty' or 'geounit'. If level is set to 'cube', cell size
      will be the native resolution of the cube for a degree-based CRS,
      or for a km-based CRS, the cell size will be determined by the
      area of the cube: 100 km for cubes larger than 1 million sq km, 10
      km for cubes between 10 thousand and 1 million sq km, 1 km for
      cubes between 100 and 10 thousand sq km, and 0.1 km for cubes
      smaller than 100 sq km. Alternatively, the user can manually
      select the grid cell size (in km or degrees). Note that the cell
      size must be a whole number multiple of the cube's resolution.

  `level`

  :   (Optional) Spatial level: 'cube', 'continent', 'country', 'world',
      'sovereignty', or 'geounit'. (Default: 'cube')

  `region`

  :   (Optional) The region of interest (e.g., "Europe"). This parameter
      is ignored if level is set to 'cube' or 'world'. (Default: NULL)

  `ne_type`

  :   (Optional) The type of Natural Earth data to download:
      'countries', 'map_units', 'sovereignty', or 'tiny_countries'. This
      parameter is ignored if level is set to 'cube' or 'world'.
      (Default: "countries")

  `ne_scale`

  :   (Optional) The scale of Natural Earth data to download: 'small' -
      110m, 'medium' - 50m, or 'large' - 10m. (Default: "medium")

  `output_crs`

  :   (Optional) The CRS you want for your calculated indicator. (Leave
      blank to let the function choose a default based on grid reference
      system.)

  `first_year`

  :   (Optional) Exclude data before this year. (Uses all data in the
      cube by default.)

  `last_year`

  :   (Optional) Exclude data after this year. (Uses all data in the
      cube by default.)

  `spherical_geometry`

  :   (Optional) If set to FALSE, will temporarily disable spherical
      geometry while the function runs. Should only be used to solve
      specific issues. (Default is TRUE).

  `make_valid`

  :   (Optional) Calls st_make_valid() from the sf package after
      creating the grid. Increases processing time but may help if you
      are getting polygon errors. (Default is FALSE).

  `num_bootstrap`

  :   (Optional) Set the number of bootstraps to calculate for
      generating confidence intervals. (Default: 100)

  `shapefile_path`

  :   (optional) Path of an external shapefile to merge into the
      workflow. For example, if you want to calculate your indicator
      particular features such as protected areas or wetlands.

  `shapefile_crs`

  :   (Optional) CRS of a .wkt shapefile. If your shapefile is .wkt and
      you do NOT use this parameter, the CRS will be assumed to be
      EPSG:4326 and the coordinates will be read in as lat/long. If your
      shape is NOT a .wkt the CRS will be determined automatically.

  `invert`

  :   (optional) Calculate an indicator over the inverse of the
      shapefile (e.g. if you have a protected areas shapefile this would
      calculate an indicator over all non protected areas within your
      cube). Default is FALSE.

  `include_land`

  :   (Optional) Include occurrences which fall within the land area.
      Default is TRUE. \*Note that this purely a geographic filter, and
      does not filter based on whether the occurrence is actually
      terrestrial. Grid cells which fall partially on land and partially
      on ocean will be included even if include_land is FALSE. To
      exclude terrestrial and/or freshwater taxa, you must manually
      filter your data cube before calculating your indicator.

  `include_ocean`

  :   (Optional) Include occurrences which fall outside the land area.
      Default is TRUE. Set as "buffered_coast" to include a set buffer
      size around the land area rather than the entire ocean area.
      \*Note that this is purely a geographic filter, and does not
      filter based on whether the occurrence is actually marine. Grid
      cells which fall partially on land and partially on ocean will be
      included even if include_ocean is FALSE. To exclude marine taxa,
      you must manually filter your data cube before calculating your
      indicator.

  `buffer_dist_km`

  :   (Optional) The distance to buffer around the land if include_ocean
      is set to "buffered_coast". Default is 50 km.

  `force_grid`

  :   (Optional) Forces the calculation of a grid even if this would not
      normally be part of the pipeline, e.g. for time series. This
      setting is required for the calculation of rarity or Hill
      diversity, and is forced on by indicators that require it.
      (Default: FALSE)

## Value

An S3 object with the classes 'indicator_ts' and 'occ_turnover'
containing the calculated indicator values and metadata.

## Details

Occupancy turnover measures the change in species composition over time,
reflecting the rate at which species appear or disappear from a given
area. It provides insights into the dynamic nature of ecological
communities, highlighting shifts in species distributions and potential
environmental changes. High turnover rates may indicate rapid community
restructuring, potentially driven by factors such as habitat alteration,
climate change, or invasive species. Analyzing occupancy turnover can be
crucial for understanding ecosystem stability, identifying areas of
conservation concern, and assessing the effectiveness of management
strategies.

Occupancy turnover can be calculated in different ways, but here we use
the Jaccard dissimilarity index (Jaccard, 1901) to measure the
similarity between two sets of species occurrences. The Jaccard index is
calculated as:

\$\$ J = (b + c) / (a + b + c) \$\$

where a is the number of species present in both time periods, b is the
number of species present only in the first time period, and c is the
number of species present only in the second time period. The index
ranges from 0 (no turnover) to 1 (complete turnover).

## References

Jaccard, P. (1901). Étude de la distribution florale dans une portion
des Alpes et du Jura. *Bulletin de la Société Vaudoise des* *Science
Naturelles*, *37*(142), 547-579.

## See also

compute_indicator_workflow

## Examples

``` r
ot_ts <- occ_turnover_ts(example_cube_1, first_year = 1985)
#> Warning: Bootstrapped confidence intervals cannot be calculated for the chosen indicator.
plot(ot_ts)
#> Warning: Removed 1 row containing non-finite outside the scale range (`stat_smooth()`).
#> Warning: Removed 1 row containing missing values or values outside the scale range
#> (`geom_point()`).

```
