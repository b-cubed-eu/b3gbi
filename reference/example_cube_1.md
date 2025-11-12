# Cube of GBIF Mammal Occurrences in Denmark

Small example cube containing the mammal occurrences in Denmark
available on GBIF as of 16.03.2024.

## Usage

``` r
example_cube_1
```

## Format

A 'processed_cube' object containing a tibble with 276,950 rows and 10
variables, as well as metadata

- year:

  year occurrence was recorded

- eea_cell_code:

  code containing the cell resolution and coordinates on the EEA grid

- taxonKey:

  taxonomic key associated with the species on GBIF

- obs:

  number of individuals observed

- scientificName:

  scientific species name

- rank:

  taxonomic rank

- kingdom:

  kingdom

- xcoord:

  East-West coordinate on the EEA grid

- ycoord:

  North-South coordinate on the EEA grid

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
         title = "Mammals in Denmark (1751-2023): Observed Species Richness")
  } # }
}
```
