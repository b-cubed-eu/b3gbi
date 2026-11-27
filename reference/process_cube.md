# Process GBIF Data Cubes

Processes a GBIF occurrence cube (a CSV or parquet file, or a data
frame) into a `processed_cube` object. Cubes produced by the GBIF cube
API can have user-specified column names, so check that your column
names match the Darwin Core names expected by this function; if not,
supply them with the `cols_*` arguments. The function stops with an
error if it cannot find all required columns.

## Usage

``` r
process_cube(
  cube_name,
  grid_type = c("automatic", "eea", "mgrs", "eqdgc", "isea3h", "custom", "none"),
  first_year = NULL,
  last_year = NULL,
  force_gridcode = FALSE,
  cols_year = NULL,
  cols_yearMonth = NULL,
  cols_yearMonthDay = NULL,
  cols_cellCode = NULL,
  cols_occurrences = NULL,
  cols_scientificName = NULL,
  cols_minCoordinateUncertaintyInMeters = NULL,
  cols_minTemporalUncertainty = NULL,
  cols_kingdom = NULL,
  cols_family = NULL,
  cols_species = NULL,
  cols_kingdomKey = NULL,
  cols_familyKey = NULL,
  cols_speciesKey = NULL,
  cols_familyCount = NULL,
  cols_sex = NULL,
  cols_lifeStage = NULL,
  separator = NULL,
  grid = NULL,
  grid_id = NULL,
  grid_crs = NULL
)
```

## Arguments

- cube_name:

  Either the path to a data cube CSV or parquet file (e.g.
  `system.file("extdata", "denmark_mammals_cube_eqdgc.csv", package = "b3gbi")`)
  or a data frame containing the cube. Reading parquet files requires
  the arrow package.

- grid_type:

  (Optional) The grid reference system your cube uses. One of
  `"automatic"` (default), `"eea"`, `"mgrs"`, `"eqdgc"`, `"isea3h"`,
  `"custom"` or `"none"`. With `"automatic"` the function attempts to
  detect the grid from the cell codes and returns an error if it fails.
  If you want to perform analysis on a cube with custom grid codes (e.g.
  output from the gcube package) or a cube without grid codes, select
  `"custom"` or `"none"`, respectively. Cubes with custom grid codes can
  only be used for time series, unless the cell geometries are given
  with `grid`; supplying `grid` sets the grid type to `"custom"`.

- first_year:

  (Optional) The first year of occurrences to include. If not specified,
  uses the earliest year present in the cube.

- last_year:

  (Optional) The final year of occurrences to include. If not specified,
  uses the latest year present in the cube.

- force_gridcode:

  (Optional) Logical. If `TRUE`, skips the check that cell codes match
  the expected format of `grid_type`. Not recommended; invalid codes may
  cause downstream errors. Default `FALSE`.

- cols_year:

  (Optional) The name of the column containing the year of occurrence
  (if something other than 'year'). This column is required unless you
  have a yearMonth column.

- cols_yearMonth:

  (Optional) The name of the column containing the year and month of
  occurrence (if present and if other than 'yearMonth'). Use this only
  if you do not have a year column. The b3gbi package does not use month
  data, so the function will convert your yearMonth column to a year
  column.

- cols_yearMonthDay:

  (Optional) The name of the column containing the year, month and day
  of occurrence (if present and if other than 'yearMonthDay'). Use this
  only if you do not have year or yearMonth columns. The b3gbi package
  does not use day or month data, so the function will convert your
  yearMonthDay column to a year column.

- cols_cellCode:

  (Optional) The name of the column containing the grid reference codes
  (if other than 'cellCode'). This column is required.

- cols_occurrences:

  (Optional) The name of the column containing the number of occurrences
  (if other than 'occurrences'). This column is required.

- cols_scientificName:

  (Optional) The name of the column containing the scientific name of
  the species (if other than 'scientificName'). Note that it is not
  necessary to have both a species column and a scientificName column.
  One or the other is sufficient.

- cols_minCoordinateUncertaintyInMeters:

  (Optional) The name of the column containing the minimum coordinate
  uncertainty of the occurrences (if other than
  'minCoordinateUncertaintyInMeters').

- cols_minTemporalUncertainty:

  (Optional) The name of the column containing the minimum temporal
  uncertainty of the occurrences (if other than
  'minTemporalUncertainty').

- cols_kingdom:

  (Optional) The name of the column containing the kingdom the occurring
  species belongs to (if other than 'kingdom').

- cols_family:

  (Optional) The name of the column containing the family the occurring
  species belongs to (if other than 'family').

- cols_species:

  (Optional) The name of the column containing the name of the occurring
  species (if other than 'species'). Note that it is not necessary to
  have both a species column and a scientificName column. One or the
  other is sufficient.

- cols_kingdomKey:

  (Optional) The name of the column containing the kingdom key of the
  occurring species (if other than 'kingdomKey').

- cols_familyKey:

  (Optional) The name of the column containing the family key of the
  occurring species (if other than 'familyKey').

- cols_speciesKey:

  (Optional) The name of the column containing the species key of the
  occurring species (if other than 'speciesKey'). The column is
  required, but note that if you have a 'taxonKey' column you can
  provide it as the speciesKey.

- cols_familyCount:

  (Optional) The name of the column containing the occurrence count by
  family.

- cols_sex:

  (Optional) The name of the column containing the sex of the observed
  individuals.

- cols_lifeStage:

  (Optional) The name of the column containing the life stage of the
  observed individuals.

- separator:

  (Optional) The column-separating character in your csv file. This
  should be automatically recognized, so only specify this if you are
  having trouble.

- grid:

  (Optional) The geometries of the grid cells, for cubes that use a grid
  other than the supported ones (e.g. cubes from the BMD cubing engine,
  whose cells are identified by an ID and described in a separate
  dimension table). Either the path to a file (GeoParquet, GeoPackage,
  shapefile, GeoJSON, or a CSV with a WKT geometry column), an sf
  object, or a data frame with a WKT geometry column. Each grid cell
  must be a polygon with a unique ID, which is matched to the cube's
  grid cell column. With a grid, all indicators (maps and time series)
  can be calculated. Reading parquet files requires the arrow and
  jsonlite packages.

- grid_id:

  (Optional) The name of the ID column in `grid`. If not given, the
  column with the same name as `cols_cellCode` is used, or else the one
  column name shared by the cube and the grid (e.g. `grid_idx`).

- grid_crs:

  (Optional) The coordinate reference system of `grid` (e.g.
  `"EPSG:3035"`), needed only if the grid file does not contain one
  (e.g. a CSV with WKT geometries).

## Value

An object of class `processed_cube` (or `sim_cube` when `grid_type` is
`"none"`, or `"custom"` without `grid`): a list of metadata (years,
number of species, grid type, resolution, ...) plus the processed
occurrences in the `data` element. With a custom `grid`, the grid cells
are stored in the `grid` element.

## Examples

``` r
# \donttest{
cube_name <- system.file("extdata", "denmark_mammals_cube_eqdgc.csv",
                         package = "b3gbi")
denmark_example_cube <- process_cube(cube_name)
denmark_example_cube
#> 
#> Processed data cube for calculating biodiversity indicators
#> 
#> Date Range: 1901 - 2024 
#> Single-resolution cube with cell size 0.25degrees 
#> Number of cells: 186 
#> Grid reference system: eqdgc 
#> Coordinate range:
#>  xmin  xmax  ymin  ymax 
#>  3.75 15.25 54.50 58.25 
#> 
#> Total number of observations: 6099 
#> Number of species represented: 54 
#> Number of families represented: 19 
#> 
#> Kingdoms represented: Animalia 
#> 
#> First 10 rows of data (use n = to show more):
#> 
#> # A tibble: 989 × 15
#>     year cellCode  kingdomKey kingdom  familyKey family  taxonKey scientificName
#>    <dbl> <chr>     <chr>      <chr>    <chr>     <chr>   <chr>    <chr>         
#>  1  1901 E010N55CD 1          Animalia 9456      Sciuri… 8211070  Sciurus vulga…
#>  2  1930 E014N55DD 1          Animalia 9456      Sciuri… 8211070  Sciurus vulga…
#>  3  1940 E012N55BA 1          Animalia 9614      Bovidae 2441022  Bos taurus    
#>  4  1943 E011N54BA 1          Animalia 5510      Muridae 2437756  Apodemus flav…
#>  5  1946 E008N56BB 1          Animalia 5510      Muridae 5219833  Micromys minu…
#>  6  1952 E010N57CB 1          Animalia 5510      Muridae 2439261  Rattus norveg…
#>  7  1960 E014N55DD 1          Animalia 5534      Sorici… 8316400  Sorex araneus 
#>  8  1963 E008N57DC 1          Animalia 5534      Sorici… 7571319  Sorex minutus 
#>  9  1968 E014N55DB 1          Animalia 5510      Muridae 2437756  Apodemus flav…
#> 10  1969 E012N55BA 1          Animalia 9701      Canidae 5219243  Vulpes vulpes 
#> # ℹ 979 more rows
#> # ℹ 7 more variables: obs <dbl>, minCoordinateUncertaintyInMeters <dbl>,
#> #   minTemporalUncertainty <dbl>, familyCount <dbl>, xcoord <dbl>,
#> #   ycoord <dbl>, resolution <chr>
# }

# A cube whose grid cells are defined in a separate file (e.g. a cube and
# dimension table from the BMD cubing engine)
montenegro_cube <- process_cube(
  system.file("extdata", "montenegro_bryophytes_cube_custom.csv",
              package = "b3gbi"),
  grid = system.file("extdata", "montenegro_bryophytes_grid.gpkg",
                     package = "b3gbi")
)
montenegro_cube
#> 
#> Processed data cube for calculating biodiversity indicators
#> 
#> Date Range: 2003 - 2025 
#> Single-resolution cube with cell size 1km ^2 
#> Number of cells: 124 
#> Grid reference system: custom 
#> Coordinate range:
#>    xmin    xmax    ymin    ymax 
#> 5018000 5149000 2135000 2274000 
#> 
#> Total number of observations: 244 
#> Number of species represented: 91 
#> Number of families represented: 34 
#> 
#> Kingdoms represented: Plantae 
#> 
#> First 10 rows of data (use n = to show more):
#> 
#> # A tibble: 225 × 28
#>    cellCode kingdom kingdomKey phylum    phylumkey class classkey order orderkey
#>    <chr>    <chr>   <chr>      <chr>     <chr>     <chr> <chr>    <chr> <chr>   
#>  1 1009     Plantae 6          Bryophyta 35        Bryo… 327      Timm… 12191514
#>  2 1020     Plantae 6          Bryophyta 35        Bryo… 327      Orth… 1341    
#>  3 1021     Plantae 6          Bryophyta 35        Spha… 190      Spha… 623     
#>  4 1014     Plantae 6          Bryophyta 35        Bryo… 327      Timm… 12191514
#>  5 1020     Plantae 6          Bryophyta 35        Bryo… 327      Timm… 12191514
#>  6 1021     Plantae 6          Bryophyta 35        Bryo… 327      Hypn… 617     
#>  7 1001     Plantae 6          Bryophyta 35        Bryo… 327      Orth… 1341    
#>  8 1088     Plantae 6          Bryophyta 35        Bryo… 327      Hypn… 617     
#>  9 1091     Plantae 6          Bryophyta 35        Bryo… 327      Hypn… 617     
#> 10 1090     Plantae 6          Bryophyta 35        Bryo… 327      Hypn… 617     
#> # ℹ 215 more rows
#> # ℹ 19 more variables: family <chr>, familyKey <chr>, genus <chr>,
#> #   genuskey <chr>, scientificName <chr>, taxonKey <chr>, year <dbl>,
#> #   kingdomcount <chr>, phylumcount <chr>, classcount <chr>, ordercount <chr>,
#> #   familyCount <dbl>, genuscount <chr>, obs <dbl>,
#> #   minTemporalUncertainty <dbl>, minCoordinateUncertaintyInMeters <dbl>,
#> #   xcoord <dbl>, ycoord <dbl>, resolution <chr>
```
