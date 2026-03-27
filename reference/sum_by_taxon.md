# Sum Observations by Taxonomic Rank

This function takes a `processed_cube` object and aggregates the
observation counts by a specified taxonomic rank.

## Usage

``` r
sum_by_taxon(object, rank)
```

## Arguments

- object:

  A `processed_cube` object containing taxonomic information in
  `object$data`.

- rank:

  A character string specifying the taxonomic rank to group by (e.g.,
  "family", "kingdom"). This column must exist in `object$data`.

## Value

A tibble with columns `total_observations` and the specified `rank`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'cube' is a processed_cube object
family_sums <- sum_by_taxon(cube, "family")
} # }
```
