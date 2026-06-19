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

## Hungary Amphibians Data Cube
* **File**: `hungary_amphibians_cube_mgrs.csv`
* **Source**: GBIF occurrence download of amphibians in Hungary.

## Montenegro Bryophytes Data Cube
* **File**: `montenegro_bryophytes_cube_eea.csv`
* **Source**: GBIF occurrence download of bryophytes in Montenegro.
