
<!-- README.md is generated from README.Rmd. Please edit that file -->

# b3gbi <a href="https://b-cubed-eu.github.io/b3gbi/"><img src="man/figures/logo.png" align="right" height="120" alt="b3gbi website"/></a>

<!-- badges: start -->

[![repo
status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#wip)
[![Release](https://img.shields.io/github/release/b-cubed-eu/b3gbi.svg?include_prereleases)](https://github.com/b-cubed-eu/b3gbi/releases)
[![b3gbi status
badge](https://b-cubed-eu.r-universe.dev/b3gbi/badges/version)](https://b-cubed-eu.r-universe.dev/b3gbi)
[![R-CMD-check](https://github.com/b-cubed-eu/b3gbi/actions/workflows/R-CMD-check.yaml/badge.svg?branch=main)](https://github.com/b-cubed-eu/b3gbi/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/b-cubed-eu/b3gbi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/b-cubed-eu/b3gbi/)
[![name status
badge](https://b-cubed-eu.r-universe.dev/badges/:name?color=6CDDB4)](https://b-cubed-eu.r-universe.dev/)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.20763709-blue.svg)](https://doi.org/10.5281/zenodo.20763709)
[![status](https://joss.theoj.org/papers/892c9da161bfc0658e2edca125a3ba10/status.svg)](https://joss.theoj.org/papers/892c9da161bfc0658e2edca125a3ba10)

<!-- badges: end -->

Analyze biodiversity trends and spatial patterns from GBIF data cubes,
using flexible indicators like richness, evenness, and more.

## Overview

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
  distinctness, Shannon-Hill diversity, Simpson-Hill diversity,
  completeness, and more.
- **Analysis Options:** Explore temporal trends or create spatial maps.
- **Visualization Tools:** Generate publication-ready plots of your
  biodiversity metrics.

## Installation

Install **b3gbi** in R:

``` r
install.packages("b3gbi", repos = c("https://b-cubed-eu.r-universe.dev", "https://cloud.r-project.org"))
```

## Example: Three-Step Workflow

This basic example demonstrates the core workflow: preparing the data
cube, calculating an indicator, and plotting the result as a spatial map
of species richness for bryophytes in Montenegro.

``` r
# Load package
library(b3gbi)

# 1. Load and prepare the GBIF data cube
cube_name <- system.file("extdata", "montenegro_bryophytes_cube_eea.csv", package = "b3gbi")
bryophyte_data <- process_cube(cube_name)

# 2. Calculate a map of observed richness
map_rich_bryo <- obs_richness_map(bryophyte_data, level = "country", region = "Montenegro", ne_scale = "medium", cell_size = 10)

# 3. Plot the indicator map
plot(map_rich_bryo, title = "Observed Species Richness: Bryophytes in Montenegro")
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

For a more in-depth introduction, see the tutorial:
<https://b-cubed-eu.github.io/b3gbi/articles/b3gbi.html>.
