
<!-- README.md is generated from README.Rmd. Please edit that file -->

# b3gbi: B-Cubed General Biodiversity Indicators

Analyze biodiversity trends and spatial patterns from GBIF data cubes,
using flexible indicators like richness, evenness, and more.

<!-- badges: start -->

[![Development Status:: In
Development](https://img.shields.io/badge/Status-In%20Development-yellow.svg)](https://github.com/your_github_username/b3gbi)

<!-- badges: end -->

## Motivation

Biodiversity researchers need robust and standardized tools to analyze
the vast amounts of data available on platforms like GBIF. The b3gbi
package leverages the power of data cubes to streamline biodiversity
assessments. It helps researchers gain insights into:

- **Changes Over Time:** How biodiversity metrics shift throughout the
  years.
- **Spatial Variations:** Differences in biodiversity across regions,
  identifying hotspots or areas of concern.
- **The Impact of Factors:** How different environmental variables or
  human activities might affect biodiversity patterns.

## Key Features

b3gbi empowers biodiversity analysis with:

- **Standardized Workflows:** Simplify the process of calculating common
  biodiversity indicators from GBIF data cubes.
- **Flexibility:** Calculate richness, evenness, rarity, taxonomic
  distinctness, Shannon-Hill diversity, Simpson-Hill diversity, and
  more.
- **Analysis Options:** Explore temporal trends or create spatial maps.
- **Visualization Tools:** Generate publication-ready plots of your
  biodiversity metrics.

## Installation

You can install the development version of b3gbi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
# devtools::install_github("shawndove/b3gbi")
```

## Example

This is a basic example which shows you how to calculate and plot a map
of species richness for a data cube containing GBIF occurrence data on
amphibians in Europe:

``` r
# Load necessary libraries
library(b3gbi)
library(data.table)
library(ggplot2)

# Load GBIF data cube
cube_name <- "inst/extdata/europe_amphibians_cube.csv"

# Load taxonomic info for cube
tax_info <- "inst/extdata/europe_amphibians_info.csv"

# Prepare cube
amphib_data <- process_cube(cube_name, tax_info)

# Calculate diversity metric
map_obs_rich_amphib <- calculate_indicator(amphib_data, type = "obs_rich")

# Plot diversity metric
plot(map_obs_rich_amphib, title = "Observed Species Richness: Amphibians in Europe") 
```

<img src="man/figures/README-example-1.png" width="100%" />
