## Resubmission

This is a resubmission (1.1.3). Changes since the previous submission:

* 1.1.3: The 1.1.2 upload failed the incoming check on Debian in one test
  (`test-hex-grids.R`, an ISEA3H hexagonal-grid integration test whose result
  depends on the system PROJ/GEOS). It passed on Windows and on the first
  submission, and the code it tests was unchanged. The test is now skipped on
  CRAN.
* 1.1.2: Removed the internal (unexported, unused) functions that wrote to the
  installed package directory to edit the indicator registry, as requested.
  They are now developer scripts in `data-raw/`, which is excluded from the
  package build. No function, example, vignette or test writes to the user's
  home filespace, the package directory or the working directory; tests write
  only to `tempfile()`/`tempdir()`, and Natural Earth downloads use
  `rnaturalearth`'s default `destdir = tempdir()`.

## Submission

This is the first CRAN submission of b3gbi. The package has been published in
the Journal of Open Source Software (JOSS).

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* Possibly misspelled words in DESCRIPTION: EEA, EQDGC, ISEA, MGRS. These are
  the standard abbreviations of the supported grid systems (European
  Environment Agency reference grid, Extended Quarter Degree Grid Cells,
  Icosahedral Snyder Equal Area aperture 3 hexagonal grid, Military Grid
  Reference System).

## Notes for the CRAN team

* **Suggested packages not on CRAN.** `dubicube` (cube-level bootstrapping,
  developed alongside b3gbi within the EU-funded B-Cubed project) and
  `rnaturalearthhires` (high-resolution Natural Earth maps) are in Suggests
  and are available from the repositories listed in `Additional_repositories`
  (https://b-cubed-eu.r-universe.dev and https://ropensci.r-universe.dev).
  Both are used only conditionally: without `dubicube`, `add_ci()` falls back
  to indicator-level bootstrapping; without `rnaturalearthhires`, large-scale
  maps give an informative error. Tests, examples and vignettes run without
  them.

* **Examples.** No examples use `\dontrun{}`. Examples that take more than a
  few seconds (map calculations that clip to country borders) are in
  `\donttest{}`. The two taxonomic distinctness examples
  (`tax_distinct_map()`, `tax_distinct_ts()`) query the GBIF web API; they are
  in `\donttest{}`, run only if `rgbif` is installed, and print a message
  instead of failing if GBIF cannot be reached.

* **Internet access.** Only taxonomic distinctness (via the Suggested package
  `rgbif`) and optional Natural Earth downloads use the internet. Tests that
  need the internet are skipped on CRAN and when offline. Functions that use
  internet resources stop with an informative message if the resource is
  unavailable.

## Test environments

* local: Linux (Ubuntu 24.04), R 4.3.3
* local: Windows, R release (devtools::check(remote = TRUE, manual = TRUE))
* win-builder: R-devel
* mac-builder: R release
* R-hub: Linux (R-devel) and Windows (R-devel), 0 errors | 0 warnings | 0 notes
