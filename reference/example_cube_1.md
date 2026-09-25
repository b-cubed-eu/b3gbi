# Cube of GBIF Mammal Occurrences in Denmark

Small example cube containing the mammal occurrences in Denmark
available on GBIF as of 16.03.2024.

## Usage

``` r
example_cube_1
```

## Format

A 'processed_cube' object: a list of metadata plus, in its `data`
element, a tibble with 31,632 rows and 15 variables

- year:

  year occurrence was recorded

- cellCode:

  code containing the cell resolution and coordinates on the Extended
  Quarter Degree (eqdgc) grid

- kingdomKey:

  kingdom key associated with the species on GBIF

- kingdom:

  kingdom name

- familyKey:

  family key associated with the species on GBIF

- family:

  family name

- taxonKey:

  taxonomic key associated with the species on GBIF

- scientificName:

  scientific species name

- obs:

  number of occurrences (records)

- minCoordinateUncertaintyInMeters:

  minimum coordinate uncertainty in meters

- minTemporalUncertainty:

  minimum temporal uncertainty in seconds

- familyCount:

  number of occurrences of the family the species belongs to

- xcoord:

  longitude of the cell centre (degrees)

- ycoord:

  latitude of the cell centre (degrees)

- resolution:

  grid cell size (e.g. "0.25degrees")

## Source

[doi:10.15468/dl.5mb887](https://doi.org/10.15468/dl.5mb887)

## Examples

``` r
# \donttest{
denmark_mammals_or_map <- obs_richness_map(example_cube_1,
                                           level = "country",
                                           region = "Denmark")
plot(denmark_mammals_or_map,
     title = "Mammals in Denmark (1862-2024): Observed Species Richness")

# }
```
