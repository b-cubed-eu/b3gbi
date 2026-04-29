# Extract Species Names.

Retrieves a standardized list of species names from a biodiversity data
object ('processed_cube', 'indicator_map' or 'indicator_ts').

## Usage

``` r
list_species(object)
```

## Arguments

- object:

  A biodiversity data object containing species names (either in a
  `data` slot or a `species_names` vector).

## Value

A tibble with a single column:

- `scientificName`: The unique scientific names found in the object.

## Examples

``` r
list_species(example_cube_1)
#> # A tibble: 106 × 1
#>    scientificName            
#>    <chr>                     
#>  1 Alces alces               
#>  2 Apodemus agrarius         
#>  3 Apodemus flavicollis      
#>  4 Apodemus sylvaticus       
#>  5 Arvicola amphibius        
#>  6 Balaenoptera acutorostrata
#>  7 Balaenoptera borealis     
#>  8 Balaenoptera physalus     
#>  9 Barbastella barbastellus  
#> 10 Bison bonasus             
#> # ℹ 96 more rows
```
