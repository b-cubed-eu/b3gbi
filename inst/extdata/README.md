# Example Datasets (`inst/extdata`)

This directory contains example biodiversity data cubes used in unit tests, vignettes, and examples. To comply with CRAN package size limitations (< 5 MB), some of these datasets have been systematically downsampled.

## Denmark Mammals Data Cubes
* **Files**:
  - `denmark_mammals_cube_eea.csv`
  - `denmark_mammals_cube_eqdgc.csv`
  - `denmark_mammals_cube_mgrs.csv`
* **Source**: GBIF occurrence download (16 March 2024)
* **Original DOI**: [https://doi.org/10.15468/dl.5mb887](https://doi.org/10.15468/dl.5mb887)
* **Note on Downsampling**: These files have been systematically downsampled (taking every $N$-th row) to exactly ~1,000 rows each to reduce package size. Because of this, they are representative subsets of the full data, and do not exactly match the checksums or complete row counts of the original downloads linked to the GBIF DOI.

## Montenegro Bryophytes Data Cube
* **File**: `montenegro_bryophytes_cube_eea.csv`
* **Source**: GBIF occurrence download of bryophytes in Montenegro.

## Montenegro Bryophytes Cube with Its Own Grid
* **Files**:
  - `montenegro_bryophytes_cube_custom.csv`
  - `montenegro_bryophytes_grid.gpkg`
* **Source**: Derived from `montenegro_bryophytes_cube_eea.csv` with `data-raw/make_custom_grid_example.R`. The EEA cell codes are replaced by an arbitrary cell ID (`grid_idx`), and the cell polygons (EPSG:3035) are stored separately in the GeoPackage, as delivered by cube-producing tools such as the B-Cubed BMD cubing engine. Used to demonstrate `process_cube(grid = ...)`.
