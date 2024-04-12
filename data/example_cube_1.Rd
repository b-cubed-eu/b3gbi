\name{example_cube_1}
\alias{example_cube_1}
\title{Cube of GBIF Mammal Occurrences in Denmark}
\description{
    This is a small example cube containing the mammal occurrences in Denmark available on GBIF as of 16.03.2024.
}
\format{
    Contains a tibble with the columns 'year', 'eea_cell_code', 'taxonKey', 'obs', 'scientificName', 'rank', 'kingdom', 'xcoord', 'ycoord', and 'resolution'. This can be directly accessed using example_cube_1$data. Also contains metadata, including the data range, resolution, number of grid cells, coordinate range, total number of observations, number of species represented, and kingdoms represented. Simply type example_cube_1 to see the metadata.
}
\source{
    The occurrences were downloaded from GBIF: https://doi.org/10.15468/dl.5mb887
}
\examples{
    \dontrun{
        # Basic example of how to use the dataset
        denmark_mammals_or_map <- obs_richness_map(example_cube_1, level = "country", region = "Denmark")
        plot(denmark_mammals_or_map, title = "Mammals in Denmark (1751-2023): Observed Species Richness")
    }
}
