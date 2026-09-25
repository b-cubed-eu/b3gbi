# Map of Observed Species Richness for Mammals in Denmark

Example indicator containing a map of observed species richness for
mammal occurrences in Denmark (occurrences from GBIF: 16.03.2024).

## Usage

``` r
example_indicator_map1
```

## Format

An 'indicator_map' object: a list of metadata plus, in its `data`
element, an sf data frame with 800 rows and 5 variables

- cellid:

  id of a map cell the indicator was calculated for

- area:

  area of the map cell in square kilometers

- cellCode:

  code containing the cell coordinates in Extended Quarter Degree
  (eqdgc) grid format

- diversity_val:

  calculated richness value for the cell

- geometry:

  geometry of the map cell

## Source

[doi:10.15468/dl.5mb887](https://doi.org/10.15468/dl.5mb887)
