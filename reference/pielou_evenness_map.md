# Calculate Evenness Over Time or Space

Calculate evenness over a gridded map or as a time series (see 'Details'
for more information).

## Usage

``` r
pielou_evenness_map(data, ...)

pielou_evenness_ts(data, ...)

williams_evenness_map(data, ...)

williams_evenness_ts(data, ...)
```

## Arguments

- data:

  A data cube object (class 'processed_cube').

- ...:

  Arguments passed on to
  [`compute_indicator_workflow`](https://b-cubed-eu.github.io/b3gbi/reference/compute_indicator_workflow.md)

  `cell_size`

  :   (Optional) Length of grid cell sides, in km or degrees. Only used
      for maps and for time series that require a grid.

      - `"grid"` (default): use the native resolution of the cube. If
        this would produce more than about 1 million grid cells over the
        study area (for degree-based cubes: if the resolution is finer
        than 1 degree for 'world' or 'continent', or finer than 0.1
        degrees otherwise), you are asked to confirm in an interactive
        session, and the function stops with an error in a
        non-interactive session.

      - `"auto"`: determined automatically. For km-based grids it
        depends on the area of the study region: 100 km for areas of at
        least 1 million sq km, 10 km for at least 10,000 sq km, 1 km for
        at least 100 sq km, and 0.1 km for smaller areas. For
        degree-based grids it is 1 degree for 'world' or 'continent' and
        0.1 degrees otherwise. The automatic size is never smaller than
        the cube's resolution.

      - A number (in the units of the cube's resolution, i.e. km or
        degrees), or for km-based grids a string such as `"10km"` or
        `"500m"`.

      A manually selected cell size must be a whole number multiple of
      the cube's resolution.

  `level`

  :   (Optional) Spatial level: 'cube', 'continent', 'country', 'world',
      'sovereignty', or 'geounit'. (Default: 'cube')

  `region`

  :   (Optional) The region of interest (e.g., "Denmark"). Ignored if
      level is 'cube' or 'world'. (Default: "Europe")

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

  `shapefile_path`

  :   (optional) Path of an external shapefile to merge into the
      workflow. For example, if you want to calculate your indicator for
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
      Default is TRUE. Note that this is purely a geographic filter, and
      does not filter based on whether the occurrence is actually
      terrestrial. Grid cells which fall partially on land and partially
      on ocean will be included even if include_land is FALSE. To
      exclude terrestrial and/or freshwater taxa, you must manually
      filter your data cube before calculating your indicator.

  `include_ocean`

  :   (Optional) Include occurrences which fall outside the land area.
      Default is TRUE. Set as "buffered_coast" to include a set buffer
      size around the land area rather than the entire ocean area. Note
      that this is purely a geographic filter, and does not filter based
      on whether the occurrence is actually marine. Grid cells which
      fall partially on land and partially on ocean will be included
      even if include_ocean is FALSE. To exclude marine taxa, you must
      manually filter your data cube before calculating your indicator.

  `buffer_dist_km`

  :   (Optional) The distance to buffer around the land if include_ocean
      is set to "buffered_coast". Default is 50 km.

  `force_grid`

  :   (Optional) Forces the calculation of a grid even if this would not
      normally be part of the pipeline, i.e. for time series. A grid is
      needed for time series of area-based rarity, Hill diversity and
      relative occupancy (and for completeness with
      `gridded_average = TRUE`). This is switched on automatically for
      these indicators: the wrappers
      [`area_rarity_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/area_rarity_map.md),
      [`hill0_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/hill0_map.md),
      [`hill1_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/hill0_map.md)
      and
      [`hill2_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/hill0_map.md)
      already set `force_grid = TRUE`, so do not pass it to them.
      (Default: FALSE)

## Value

An S3 object with the classes 'indicator_map' or 'indicator_ts' and
'pielou_evenness' or 'williams_evenness' containing the calculated
indicator values and metadata.

## Details

### Evenness

Species evenness is a commonly used indicator that measures how
uniformly individuals are distributed across species in a region or over
time. It provides a complement to richness by taking relative abundance
into account. Although GBIF provides information about abundances as
individual counts, the majority of entries lack this information. Hence,
evenness can only be calculated using the proportions of observations
rather than proportions of individuals. Strictly speaking, the evenness
measures therefore indicate how uniformly species are represented in the
respective data set rather than the true evenness of the ecological
community.

### Pielou's evenness

Pielou's evenness (1966) is a well-known and commonly used evenness
measure. It is calculated as:

\$\$ E = \frac{-\sum\_{i=1}^{S} p_i \ln(p_i)}{\ln(S)} \$\$ where S is
the number of species observed in the grid cell (maps) or year (time
series) and pi is the proportion of occurrences represented by species
i. Species absent from a cell or year do not count towards S, and
evenness is NA when fewer than two species are present.

### Williams' evenness

An analysis of evenness properties by Kvålseth (2015) showed that an
evenness index introduced by Williams in 1977 in an unpublished
manuscript has two important properties which Pielou's does not. The
properties in question are complex mathematical properties known as the
Schur-Concavity and value validity, but we attempt to describe them here
more simply. If a measure of evenness is Schur-concave, it means that
when the distribution of individuals becomes more evenly spread across
species, the measure of evenness will stay the same or increase, but
never decrease. Value validity means that an evenness index should
provide sensible and meaningful values across its range for any given
distribution of species abundances. Kvålseth referred to this evenness
measure as E9 but we refer to it as Williams' evenness.

Williams' evenness is calculated as:

\$\$ 1 - \sqrt{\frac{S\sum\_{i=1}^{S} p_i^2 - 1}{S - 1}} \$\$

where S is the number of species observed in the grid cell (maps) or
year (time series) and pi is the proportion of occurrences represented
by species i. Species absent from a cell or year do not count towards S,
and evenness is NA when fewer than two species are present.

## Functions

- `pielou_evenness_map()`:

- `pielou_evenness_ts()`:

- `williams_evenness_map()`:

- `williams_evenness_ts()`:

## References

Pielou, E. C. (1966). The measurement of diversity in different types of
biological collections. *Journal of theoretical biology*, *13*, 131-144.

Kvålseth, T. O. (2015). Evenness indices once again: critical analysis
of properties. *SpringerPlus*, *4*, 1-12.

## See also

[`compute_indicator_workflow()`](https://b-cubed-eu.github.io/b3gbi/reference/compute_indicator_workflow.md),
[`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md)

## Examples

``` r
# \donttest{
pe_map <- pielou_evenness_map(example_cube_1,
  level = "country",
  region = "Denmark"
)
plot(pe_map)

# }
# \donttest{
pe_ts <- pielou_evenness_ts(example_cube_1, first_year = 1985)
plot(pe_ts)

# }
# \donttest{
we_map <- williams_evenness_map(example_cube_1,
  level = "country",
  region = "Denmark"
)
plot(we_map)

# }
# \donttest{
we_ts <- williams_evenness_ts(example_cube_1, first_year = 1985)
plot(we_ts)

# }
```
