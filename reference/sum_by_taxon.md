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
family_sums <- sum_by_taxon(example_cube_1, "family")
head(family_sums)
#> # A tibble: 6 × 2
#>   total_observations family         
#>                <dbl> <chr>          
#> 1                 99 Balaenopteridae
#> 2                 94 Bovidae        
#> 3               9281 Canidae        
#> 4                200 Castoridae     
#> 5              34839 Cervidae       
#> 6               1947 Cricetidae     
```
