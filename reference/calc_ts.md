# Calculate Biodiversity Indicators Over Time

This function provides a flexible framework for calculating various
biodiversity indicators over time. It prepares the data, creates a grid,
calculates indicators, and formats the output into an appropriate S3
object ('indicator_ts'). Specific implementations for different
indicator types are provided using the appropriate wrappers.

## Usage

``` r
# Default S3 method
calc_ts(x, ...)

# S3 method for class 'completeness'
calc_ts(x, ...)

# S3 method for class 'hill0'
calc_ts(x, ...)

# S3 method for class 'hill1'
calc_ts(x, ...)

# S3 method for class 'hill2'
calc_ts(x, ...)

# S3 method for class 'obs_richness'
calc_ts(x, ...)

# S3 method for class 'cum_richness'
calc_ts(x, ...)

# S3 method for class 'total_occ'
calc_ts(x, ...)

# S3 method for class 'spec_richness_density'
calc_ts(x, ...)

# S3 method for class 'occ_density'
calc_ts(x, ...)

# S3 method for class 'newness'
calc_ts(x, ...)

# S3 method for class 'williams_evenness'
calc_ts(x, ...)

# S3 method for class 'pielou_evenness'
calc_ts(x, ...)

# S3 method for class 'ab_rarity'
calc_ts(x, ...)

# S3 method for class 'area_rarity'
calc_ts(x, ...)

# S3 method for class 'spec_occ'
calc_ts(x, ...)

# S3 method for class 'spec_range'
calc_ts(x, ...)

# S3 method for class 'relative_occupancy'
calc_ts(x, occ_type = 0, ...)

# S3 method for class 'tax_distinct'
calc_ts(x, set_rows = 1, ...)

# S3 method for class 'occ_turnover'
calc_ts(x, ...)

calc_ts(x, ...)
```

## Arguments

- x:

  A data cube object ('processed_cube').

- ...:

  Additional arguments passed to specific indicator calculation
  functions.

- occ_type:

  Integer controlling the occupancy denominator (default `0`):

  - `0` — **Total-area occupancy**: number of cells occupied by species
    / total number of grid cells in the study region (including
    unsampled cells). This is the most conservative measure.
    *Presence-only caveat*: empty cells cannot be assumed to be truly
    unoccupied; they may simply lack sampling effort.

  - `1` — **Relative-to-ever-occupied occupancy**: number of cells
    occupied by species / number of cells with *at least one occurrence
    (any species) anywhere in the time window*. This conditions on cells
    where some sampling effort is documented but the denominator is
    still constant across years.

  - `2` — **Annual occupancy**: for each year, number of cells occupied
    by species / number of cells with at least one occurrence (any
    species) *in that year*. The denominator therefore varies by year,
    reflecting changes in sampling footprint over time.

  **Note on presence-only data**: All three types are computed from
  presence-only occurrence data. A cell with no records cannot be
  assumed to be truly unoccupied; it may be unsampled or under-surveyed.
  Types 1 and 2 partially address this by restricting the denominator to
  cells with documented occurrences, but they still reflect *recording
  effort* rather than true absence. Interpret all types with care.

  **Note on cell aggregation**: When a coarser `cell_size` is chosen,
  data are aggregated to coarser grid cells before this calculation. The
  denominator counts post-aggregation cells (`cellid`), not original
  cube cells (`cellCode`).

- set_rows:

  Automatically select which taxonomic information to keep when there
  are multiple options. Default value of 1 keeps the first option, which
  is usually the best.

## Value

An S3 object of the class 'indicator_ts' containing the calculated
indicator values and metadata.

## Examples

``` r
total_occurrences_trend <- total_occ_ts(example_cube_1)
#> [1] "All values of t are equal to  2 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  2 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  3 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  2 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  2 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  2 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  4 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  3 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  2 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  2 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  5 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  1 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  4 \n Cannot calculate confidence intervals"
#> [1] "All values of t are equal to  3 \n Cannot calculate confidence intervals"
plot(total_occurrences_trend, min_year=1980)

```
