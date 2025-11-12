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
  ci_type = c("norm", "basic", "perc", "bca", "none"),
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
  num_bootstrap = 100,
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

  A data cube object (class 'processed_cube').

- type:

  The indicator to calculate. Supported options include:

  - 'obs_richness': Observed species richness.

  - 'total_occ': Total number of occurrences.

  - 'newness': Mean year of occurrence.

  - 'occ_density': Density of occurrences.

  - 'williams_evenness', 'pielou_evenness': Evenness measures.

  - 'ab_rarity', 'area_rarity': Abundance-based and area-based rarity
    scores.

  - 'cum_richness': Cumulative species richness.

  - 'occ_turnover': Occupancy turnover.

  - 'spec_range': Species range size.

  - 'spec_occ': Species occurrences.

  - 'tax_distinct': Taxonomic distinctness.

  - 'hill0': Species richness (estimated by coverage-based rarefaction).

  - 'hill1': Hill-Shannon diversity (estimated by coverage-based
    rarefaction).

  - 'hill2': Hill-Simpson diversity (estimated by coverage-based
    rarefaction).

- dim_type:

  (Optional) Dimension to calculate indicator over time: 'ts', or space:
  'map'. (Default: 'map')

- ci_type:

  (Optional) Type of bootstrap confidence intervals to calculate.
  (Default: "norm"). Select "none" to avoid calculating bootstrap CIs.

- cell_size:

  (Optional) Length of grid cell sides, in km or degrees. If set to
  "grid" (default), this will use the existing grid size of your cube.
  If set to "auto", this will be automatically determined according to
  the geographical level selected. This is 100 km or 1 degree for
  'continent' or 'world', 10 km or (for a degree-based CRS) the native
  resolution of the cube for 'country', 'sovereignty' or 'geounit'. If
  level is set to 'cube', cell size will be the native resolution of the
  cube for a degree-based CRS, or for a km-based CRS, the cell size will
  be determined by the area of the cube: 100 km for cubes larger than 1
  million sq km, 10 km for cubes between 10 thousand and 1 million sq
  km, 1 km for cubes between 100 and 10 thousand sq km, and 0.1 km for
  cubes smaller than 100 sq km. Alternatively, the user can manually
  select the grid cell size (in km or degrees). Note that the cell size
  must be a whole number multiple of the cube's resolution.

- level:

  (Optional) Spatial level: 'cube', 'continent', 'country', 'world',
  'sovereignty', or 'geounit'. (Default: 'cube')

- region:

  (Optional) The region of interest (e.g., "Europe"). This parameter is
  ignored if level is set to 'cube' or 'world'. (Default: NULL)

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

- num_bootstrap:

  (Optional) Set the number of bootstraps to calculate for generating
  confidence intervals. (Default: 100)

- shapefile_path:

  (optional) Path of an external shapefile to merge into the workflow.
  For example, if you want to calculate your indicator particular
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
  Default is TRUE. \*Note that this purely a geographic filter, and does
  not filter based on whether the occurrence is actually terrestrial.
  Grid cells which fall partially on land and partially on ocean will be
  included even if include_land is FALSE. To exclude terrestrial and/or
  freshwater taxa, you must manually filter your data cube before
  calculating your indicator.

- include_ocean:

  (Optional) Include occurrences which fall outside the land area.
  Default is TRUE. Set as "buffered_coast" to include a set buffer size
  around the land area rather than the entire ocean area. \*Note that
  this is purely a geographic filter, and does not filter based on
  whether the occurrence is actually marine. Grid cells which fall
  partially on land and partially on ocean will be included even if
  include_ocean is FALSE. To exclude marine taxa, you must manually
  filter your data cube before calculating your indicator.

- buffer_dist_km:

  (Optional) The distance to buffer around the land if include_ocean is
  set to "buffered_coast". Default is 50 km.

- force_grid:

  (Optional) Forces the calculation of a grid even if this would not
  normally be part of the pipeline, e.g. for time series. This setting
  is required for the calculation of rarity or Hill diversity, and is
  forced on by indicators that require it. (Default: FALSE)

- ...:

  Additional arguments passed to specific indicator calculation
  functions.

## Value

An S3 object containing the calculated indicator values and metadata.

## Examples

``` r
diversity_map <- compute_indicator_workflow(example_cube_1,
                                            type = "obs_richness",
                                            dim_type = "map",
                                            level = "country",
                                            region = "Denmark")
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
#>  8.016236 54.628857 15.272695 57.736914 
#> 
#> Grid cell size: 0.25 degrees 
#> Number of cells: 299 
#> 
#> Observation years: 1862 - 2023 
#> Total years with observations: 125 
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
#> # A tibble: 299 × 3
#>    cellid   area diversity_val
#>     <int> [km^2]         <int>
#>  1     68   447.            NA
#>  2     69   447.            NA
#>  3     70   447.            20
#>  4     71   447.            NA
#>  5     72   447.            NA
#>  6     73   447.            34
#>  7     74   447.            NA
#>  8     75   447.            40
#>  9     76   447.            37
#> 10     77   447.            21
#> # ℹ 289 more rows
```
