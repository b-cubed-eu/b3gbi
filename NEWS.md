# b3gbi 0.5.2 - Minor update:

* Fixed species occurrence and species range plots when selecting only one species (previously it was not possible to add additional ggplot code when only one species was plotted due to a bug in the plotting code).

# b3gbi 0.5.1 - Minor update:

* Equations in pkgdown help website now display correctly.
* Fixed bad example in spec_occ_ts() documentation.

# b3gbi 0.5.0 - Major update:

* Conversion between input and output coordinate reference systems is now handled correctly even if they have different units (e.g. degrees vs km).
* Conversion between CRSs with different units (e.g. degrees vs km) must now be forced by the user, and a warning is given.
* More extensive documentation is now provided for indicator functions, including formulas and background information.
* Users can now provide external shapefiles to limit indicator calculations to feature boundaries.
* Removed compatibility with previous generation cubes (created using TriAs project code).

# b3gbi 0.4.4 - Minor update:

* Area is no longer calculated for projections that use degrees.
* Occurrence density now refuses to calculate for projections that use degrees.
* Object metadata now displays 'degrees' instead of 'degrees^2'.

# b3gbi 0.4.3

04.03.2025 - Minor update:

* Add power parameter for map legend scale transformations. This enables the use of Box-Cox, modulus, and Yeo-Johnson transformations.

# b3gbi 0.4.2

20.02.2025 - Minor update:

* Bug fix: Coordinates now display correctly after using process_cube() with custom grid codes

# b3gbi 0.4.1

19.02.2025 - Minor update:

* All R CMD checks are now passing without any errors, warnings or notes
* The 'taxize' package is no longer a dependency, but must be installed if you want to calculate taxonomic distinctness

# b3gbi 0.4

14.02.2025 - Major update:

* Better integration with rnaturalearth
* Fully supports b3gbi GUI functions
* Hill diversity functions are now exported and can be used

# b3gbi 0.3.1

12.02.2025 - Minor update:

* Get dependencies from recognised repositories
* Join the [R-universe](https://b-cubed-eu.r-universe.dev/)!

# b3gbi 0.3

05.12.2024 - Major update:

* Added bootstrapped confidence intervals to most indicators.
* Added many additional customization parameters for time series plots. See plot_ts() documentation.
* Defaults for some visualization parameters have changed.
* Fixed bug that caused errors about incorrect cell sizes when they were actually correct.

# b3gbi 0.2.3.1

20.11.2024 - Minor update:

* User-supplied resolutions for 'km'-based grid systems are now properly handled

# b3gbi 0.2.3

07.11.2024 - Minor update:

Compatible with gcube output:

* added class sim_cube for objects without grid codes
* added 'none' to grid_type options

Bug fixes:

* No longer returns an error when you correctly specify a cell size when calculating indicators with mgrs grid

# b3gbi 0.2.2

16.07.2024 - Minor update:

* Cubes processed using process_cube_old() now have a column called minCoordinateUncertaintyInMeters, containing the spatial uncertainty from the cube. Previously the spatial uncertainty column was made available when using process_cube() but was ignored by process_cube_old().

# b3gbi 0.2.1

27.06.2024 - Minor update:

* Added options to improve control of plot output. The argument land_fill_colour allows you to change the colour of the land area outside of the grid.The argument crop_to_grid will snap the plot edges to the gridded area. This will improve the way the plot looks if e.g. your data is sparse and does not extend to the edges of a country or continent. 
* Fixed a bug in process_cube_old that caused it to fail.
* Fixed a bug that caused calculated indicator objects to retain too many data columns.

# b3gbi 0.2.0

24.06.2024 - Major update:

* Added occupancy turnover to available indicators.
* All indicators in the package now working (you can get a list by typing 'available_indicators').
* The process_cube function now expects a current generation cube (created using the GBIF API) by default. Previous generation cubes (created using the TriAS project code) can still be used but must now be processed using a separate function, process_cube_old.
* Added support for non-standard column names (you can manually input column names when processing your cube, and the process_cube function will rename them to the Darwin Core standard expected by the rest of the package).
* Added support for Quarter Degree and Military grids (note that the Military grids are translated by an external package that is not on CRAN. It is hoped to replace this with native code in a later update).
* Process_cube function can now automatically find the column containing the grid cell codes and detect the type of grid (there are options to manually input the column name and grid cell type but in most cases it is better to leave it on automatic to avoid potential downstream problems).
* You can now select the output CRS you want to use for your calculated indicator.


# b3gbi 0.1.0


