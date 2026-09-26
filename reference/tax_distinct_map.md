# Calculate Taxonomic Distinctness Over Space or Time

This function calculates the taxonomic distinctness index (TDI) over a
gridded map or as a time series (see 'Details' for more information).

## Usage

``` r
tax_distinct_map(data, rows = 1, ...)

tax_distinct_ts(data, rows = 1, ...)
```

## Arguments

- data:

  A data cube object (class 'processed_cube').

- rows:

  Deprecated and ignored. Taxa are now looked up by their GBIF taxon
  key, so there is no ambiguity to resolve.

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
'tax_distinct' containing the calculated indicator values and metadata.

## Details

Taxonomic distinctness is an essential biodiversity variable (EBV) that
measures the taxonomic relatedness between species, providing a measure
of biodiversity that accounts for evolutionary relationships. It is
calculated as the Taxonomic Distinctness Index (TDI; Clarke & Warwick,
1999; presence-absence form): \$\$ \frac{\sum\sum\_{i\<j}
\frac{\omega\_{ij}}{L}}{\frac{S(S-1)}{2}} \$\$ where S is the number of
species, \\\omega\_{ij}\\ is the taxonomic distance between species i
and j (the number of taxonomic levels below the lowest rank they share:
1 for species in the same genus, 2 for the same family, and so on up to
7 for species in different kingdoms), and L = 7 is the number of
taxonomic levels used (kingdom, phylum, class, order, family, genus,
species). The TDI ranges from 0 to 1, with higher values indicating
greater taxonomic distinctness. It is calculated for grid cells or years
with at least three classified species.

The classification of each taxon is retrieved from GBIF with the rgbif
package (Chamberlain et al.) using the taxon keys in the cube: the GBIF
Backbone Taxonomy for numeric keys, and the Catalogue of Life eXtended
Release (COL XR) for alphanumeric keys. Taxa are looked up by key in one
batched request per checklist; taxa not found by key are retried by
scientific name, and taxa still unclassified are excluded (with a
warning). Results are cached for the rest of the R session. Requires
rgbif (\>= 3.7.0; \>= 3.8.4 for COL XR keys) and an internet connection.

## Functions

- `tax_distinct_map()`:

- `tax_distinct_ts()`:

## References

Chamberlain, S., Barve, V., Mcglinn, D., Oldoni, D., Desmet, P.,
Geffert, L., & Ram, K. rgbif: Interface to the Global Biodiversity
Information Facility API. R package.
https://CRAN.R-project.org/package=rgbif

Clarke, K. R., & Warwick, R. M. (1999). The taxonomic distinctness
measure of biodiversity: weighting of step lengths between hierarchical
levels. Marine Ecology Progress Series, 184, 21-29.

## See also

[`compute_indicator_workflow()`](https://b-cubed-eu.github.io/b3gbi/reference/compute_indicator_workflow.md),
[`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md)

## Examples

``` r
# \donttest{
# Requires the rgbif package and an internet connection (GBIF API)
if (requireNamespace("rgbif", quietly = TRUE)) {
  td_map <- tryCatch(
    tax_distinct_map(example_cube_1,
      level = "country",
      region = "Denmark"
    ),
    error = function(e) {
      message("GBIF could not be reached: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(td_map)) plot(td_map)
}
#> Retrieving the taxonomic classification of 104 taxa from GBIF (results are cached for the rest of this session).
#> Assuming first column is 'scientificName' column.

# }

# \donttest{
# Requires the rgbif package and an internet connection (GBIF API)
if (requireNamespace("rgbif", quietly = TRUE)) {
  td_ts <- tryCatch(
    tax_distinct_ts(example_cube_1,
      level = "country",
      region = "Denmark"
    ),
    error = function(e) {
      message("GBIF could not be reached: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(td_ts)) plot(td_ts)
}

# }
```
