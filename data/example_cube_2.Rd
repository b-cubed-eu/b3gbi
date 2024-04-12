\name{example_cube_2}
\alias{example_cube_2}
\title{Cube of GBIF Insect Occurrences in Europe}
\description{
    This is a small example cube containing the insect occurrences in Europe available on GBIF as of 16.03.2024.
}
\format{
    Contains a tibble with the columns 'year', 'eea_cell_code', 'taxonKey', 'obs', 'scientificName', 'rank', 'kingdom', 'xcoord', 'ycoord', and 'resolution'. This can be directly accessed using example_cube_1$data. Also contains metadata, including the data range, resolution, number of grid cells, coordinate range, total number of observations, number of species represented, and kingdoms represented. Simply type example_cube_2 to see the metadata.
}
\source{
    The occurrences were downloaded from GBIF: https://doi.org/10.15468/dl.qk4f2z
\examples{
    \dontrun{
        # Basic example of how to use the dataset
        europe_insects_or_map <- obs_richness_map(example_cube_2, level = "continent", region = "Europe")
        plot(europe_insects_or_map, title = "Insects in Europe (1751-2023): Observed Species Richness")
    }
}
