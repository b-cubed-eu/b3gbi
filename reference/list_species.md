# Extract Species Names.

Retrieves a list of species names from a biodiversity data object
('processed_cube', 'indicator_map' or 'indicator_ts').

## Usage

``` r
list_species(object)
```

## Arguments

- object:

  A biodiversity data object containing species names, either as a
  separate vector called species_names or as a column called
  scientificName.

## Value

Either a character vector of species names (if directly available) or a
data frame with columns:

- `taxonKey`: A unique identifier for each species.

- `scientificName`: The scientific name of each species.

## Examples

``` r
list_species(example_cube_1)
#> # A tibble: 106 × 2
#>    taxonKey scientificName      
#>       <dbl> <chr>               
#>  1  5219833 Micromys minutus    
#>  2  2432439 Myotis daubentonii  
#>  3  2439449 Sicista betulina    
#>  4  5219303 Vulpes lagopus      
#>  5  2437756 Apodemus flavicollis
#>  6  2437760 Apodemus sylvaticus 
#>  7  2437761 Apodemus agrarius   
#>  8  7429082 Mus musculus        
#>  9  5219243 Vulpes vulpes       
#> 10  8211070 Sciurus vulgaris    
#> # ℹ 96 more rows
```
