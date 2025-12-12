# Cube of GBIF Mammal Occurrences in Denmark

Small example cube containing the mammal occurrences in Denmark
available on GBIF as of 16.03.2024.

## Usage

``` r
example_cube_1
```

## Format

A 'processed_cube' object containing a tibble with 31,632 rows and 15
variables, as well as metadata

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

  number of individuals observed

- minCoordinateUncertaintyInMeters:

  minimum coordinate uncertainty in meters

- minTemporalUncertainty:

  minimum temporal uncertainty in seconds

- xcoord:

  East-West coordinate on the eqdgc grid

- ycoord:

  North-South coordinate on the eqdgc grid

- resolution:

  grid cell size

## Source

<https://doi.org/10.15468/dl.5mb887>

## Examples

``` r
{
  if (FALSE) { # \dontrun{
    # Basic example of how to use the dataset
    denmark_mammals_or_map <- obs_richness_map(example_cube_1,
                                               level = "country",
                                               region = "Denmark")
    plot(denmark_mammals_or_map,
         title = "Mammals in Denmark (1862-2024): Observed Species Richness")
  } # }
}
```
