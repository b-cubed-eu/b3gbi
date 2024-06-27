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


