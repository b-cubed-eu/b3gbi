# Calculate Biodiversity Indicators Over Space or Time

This function provides a flexible framework for calculating various
biodiversity indicators on a spatial grid or as a time series. It
prepares the data, creates a grid, calculates indicators, and formats
the output into an appropriate S3 object ('indicator_map' or
'indicator_ts').

## Usage

``` r
compute_indicator_workflow(
  data,
  type,
  dim_type = c("map", "ts"),
  cell_size = "grid",
  level = c("cube", "continent", "country", "world", "sovereignty", "geounit"),
  region = "Europe",
  ne_type = c("countries", "map_units", "sovereignty", "tiny_countries"),
  ne_scale = c("medium", "small", "large"),
  output_crs = NULL,
  first_year = NULL,
  last_year = NULL,
  spherical_geometry = TRUE,
  make_valid = FALSE,
  shapefile_path = NULL,
  shapefile_crs = NULL,
  invert = FALSE,
  include_land = TRUE,
  include_ocean = TRUE,
  buffer_dist_km = 50,
  force_grid = FALSE,
  ...
)
```

## Arguments

- data:

  A data cube object (class 'processed_cube', 'processed_cube_dsinfo' or
  'sim_cube').

- type:

  The indicator to calculate. Supported options include:

  - 'obs_richness': Observed species richness.

  - 'total_occ': Total number of occurrences.

  - 'newness': Mean year of occurrence.

  - 'occ_density': Density of occurrences.

  - 'spec_richness_density': Species richness density (richness / area).

  - 'williams_evenness', 'pielou_evenness': Evenness measures.

  - 'ab_rarity', 'area_rarity': Abundance-based and area-based rarity
    scores.

  - 'cum_richness': Cumulative species richness.

  - 'occ_turnover': Occupancy turnover.

  - 'spec_range': Species range size.

  - 'spec_occ': Species occurrences.

  - 'relative_occupancy': Species relative occupancy.

  - 'tax_distinct': Taxonomic distinctness.

  - 'hill0': Species richness (estimated by coverage-based rarefaction).

  - 'hill1': Hill-Shannon diversity (estimated by coverage-based
    rarefaction).

  - 'hill2': Hill-Simpson diversity (estimated by coverage-based
    rarefaction).

  - 'completeness': Sample completeness (Sample Coverage).

- dim_type:

  (Optional) Dimension to calculate indicator over time: 'ts', or space:
  'map'. (Default: 'map')

- cell_size:

  (Optional) Length of grid cell sides, in km or degrees. Only used for
  maps and for time series that require a grid.

  - `"grid"` (default): use the native resolution of the cube. If this
    would produce more than about 1 million grid cells over the study
    area (for degree-based cubes: if the resolution is finer than 1
    degree for 'world' or 'continent', or finer than 0.1 degrees
    otherwise), you are asked to confirm in an interactive session, and
    the function stops with an error in a non-interactive session.

  - `"auto"`: determined automatically. For km-based grids it depends on
    the area of the study region: 100 km for areas of at least 1 million
    sq km, 10 km for at least 10,000 sq km, 1 km for at least 100 sq km,
    and 0.1 km for smaller areas. For degree-based grids it is 1 degree
    for 'world' or 'continent' and 0.1 degrees otherwise. The automatic
    size is never smaller than the cube's resolution.

  - A number (in the units of the cube's resolution, i.e. km or
    degrees), or for km-based grids a string such as `"10km"` or
    `"500m"`.

  A manually selected cell size must be a whole number multiple of the
  cube's resolution.

- level:

  (Optional) Spatial level: 'cube', 'continent', 'country', 'world',
  'sovereignty', or 'geounit'. (Default: 'cube')

- region:

  (Optional) The region of interest (e.g., "Denmark"). Ignored if level
  is 'cube' or 'world'. (Default: "Europe")

- ne_type:

  (Optional) The type of Natural Earth data to download: 'countries',
  'map_units', 'sovereignty', or 'tiny_countries'. This parameter is
  ignored if level is set to 'cube' or 'world'. (Default: "countries")

- ne_scale:

  (Optional) The scale of Natural Earth data to download: 'small' -
  110m, 'medium' - 50m, or 'large' - 10m. (Default: "medium")

- output_crs:

  (Optional) The CRS you want for your calculated indicator. (Leave
  blank to let the function choose a default based on grid reference
  system.)

- first_year:

  (Optional) Exclude data before this year. (Uses all data in the cube
  by default.)

- last_year:

  (Optional) Exclude data after this year. (Uses all data in the cube by
  default.)

- spherical_geometry:

  (Optional) If set to FALSE, will temporarily disable spherical
  geometry while the function runs. Should only be used to solve
  specific issues. (Default is TRUE).

- make_valid:

  (Optional) Calls st_make_valid() from the sf package after creating
  the grid. Increases processing time but may help if you are getting
  polygon errors. (Default is FALSE).

- shapefile_path:

  (optional) Path of an external shapefile to merge into the workflow.
  For example, if you want to calculate your indicator for particular
  features such as protected areas or wetlands.

- shapefile_crs:

  (Optional) CRS of a .wkt shapefile. If your shapefile is .wkt and you
  do NOT use this parameter, the CRS will be assumed to be EPSG:4326 and
  the coordinates will be read in as lat/long. If your shape is NOT a
  .wkt the CRS will be determined automatically.

- invert:

  (optional) Calculate an indicator over the inverse of the shapefile
  (e.g. if you have a protected areas shapefile this would calculate an
  indicator over all non protected areas within your cube). Default is
  FALSE.

- include_land:

  (Optional) Include occurrences which fall within the land area.
  Default is TRUE. Note that this is purely a geographic filter, and
  does not filter based on whether the occurrence is actually
  terrestrial. Grid cells which fall partially on land and partially on
  ocean will be included even if include_land is FALSE. To exclude
  terrestrial and/or freshwater taxa, you must manually filter your data
  cube before calculating your indicator.

- include_ocean:

  (Optional) Include occurrences which fall outside the land area.
  Default is TRUE. Set as "buffered_coast" to include a set buffer size
  around the land area rather than the entire ocean area. Note that this
  is purely a geographic filter, and does not filter based on whether
  the occurrence is actually marine. Grid cells which fall partially on
  land and partially on ocean will be included even if include_ocean is
  FALSE. To exclude marine taxa, you must manually filter your data cube
  before calculating your indicator.

- buffer_dist_km:

  (Optional) The distance to buffer around the land if include_ocean is
  set to "buffered_coast". Default is 50 km.

- force_grid:

  (Optional) Forces the calculation of a grid even if this would not
  normally be part of the pipeline, i.e. for time series. A grid is
  needed for time series of area-based rarity, Hill diversity and
  relative occupancy (and for completeness with
  `gridded_average = TRUE`). This is switched on automatically for these
  indicators: the wrappers
  [`area_rarity_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/area_rarity_map.md),
  [`hill0_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/hill0_map.md),
  [`hill1_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/hill0_map.md)
  and
  [`hill2_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/hill0_map.md)
  already set `force_grid = TRUE`, so do not pass it to them. (Default:
  FALSE)

- ...:

  Additional arguments passed to specific indicator calculation
  functions. For time series, `ci_type` (default `"none"`) and
  `num_bootstrap` (default 0) can be used to request bootstrapped
  confidence intervals directly (alternatively, use
  [`add_ci()`](https://b-cubed-eu.github.io/b3gbi/reference/add_ci.md)
  afterwards). Other examples are `newness_min_year` for
  [`newness_map()`](https://b-cubed-eu.github.io/b3gbi/reference/newness_map.md)
  and `occ_type` for
  [`relative_occupancy_map()`](https://b-cubed-eu.github.io/b3gbi/reference/relative_occupancy_map.md)
  and
  [`relative_occupancy_ts()`](https://b-cubed-eu.github.io/b3gbi/reference/relative_occupancy_map.md).

## Value

An object of class "indicator_map" (dim_type = "map") or "indicator_ts"
(dim_type = "ts") containing the calculated indicator values and
metadata.

## Examples

``` r
# \donttest{
diversity_map <- compute_indicator_workflow(example_cube_1,
  type = "obs_richness",
  dim_type = "map",
  level = "country",
  region = "Denmark"
)
diversity_map
#> Gridded biodiversity indicator map
#> 
#> Name of Indicator: Observed Species Richness 
#> 
#> Map of Denmark 
#> 
#> Projected CRS: EPSG:4326 
#> 
#> Coordinate range:
#>      xmin      ymin      xmax      ymax 
#>  8.040572 54.750000 15.188939 57.736925 
#> 
#> Grid cell size: 0.25 degrees 
#> Number of cells: 200 
#> 
#> Observation years: 1862 - 2024 
#> Total years with observations: 126 
#> 
#> Number of species represented: 106 
#> Number of families represented: 31 
#> 
#> Kingdoms represented: Animalia 
#> 
#> Map layers:  
#> 
#> First 10 rows of data (use n = to show more):
#> 
#> # A tibble: 200 × 4
#>    cellCode  cellid   area diversity_val
#>    <chr>      <int> [km^2]         <int>
#>  1 E008N54BA    178   344.            20
#>  2 E008N55AA    126   261.            21
#>  3 E008N55AB    110   434.            35
#>  4 E008N55AC     58   293.            39
#>  5 E008N55AD     59   436.            37
#>  6 E008N55BA    185   434.            31
#>  7 E008N55BB     60   434.            25
#>  8 E008N55BC    141   436.            30
#>  9 E008N55BD     61   436.            27
#> 10 E008N55CA    127   324.             2
#> # ℹ 190 more rows
# }
```
