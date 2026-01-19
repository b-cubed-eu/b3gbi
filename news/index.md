# Changelog

## b3gbi 0.8.13

- Fixed issues with spatial intersection for Australia and USA maps by
  adding defensive checks in get_ne_data.
- Improved wrong_class and stopifnot_error to handle NA logical values
  more robustly.
- Added unit tests for NA handling in utility functions.

## b3gbi 0.8.12 - Minor update:

- The cellCode column from the cube is now retained in indicator map
  outputs. This allows users to trace back grid cells to their original
  codes in the cube.

## b3gbi 0.8.11 - Minor update:

- Increased unit test coverage for better reliability.
- Improved error handling in several functions.
- Added some missing information to DESCRIPTION file.

## b3gbi 0.8.10 - Minor update:

- Fixed a bug that caused occurrences to be wrongly assigned in EEA
  grids as the coordinates were offset incorrectly.

## b3gbi 0.8.9 - Minor update:

- Fixed an important bug that caused observations to be assigned to
  multiple grid cells when level was set to ‘cube’ (no specific region
  was selected).
- Fixed a bug that caused the occ_density_ts() function to give an error
  about missing ‘cellid’. It should now work as expected.
- Fixed a bug that that caused grid sizes other than 1km to fail for the
  EEA grid type (this was due to an inflexible coordinate extraction
  process that assumed all EEA grids were 1km).
- Changed the default grid cell size to match the grid of the provided
  cube (previously the default was to recalculate based on either the
  level or the area of the cube, unless the user specificied otherwise,
  which could lead to unexpected results). Now the user can still
  manually select the size, or set it to ‘auto’ to have the package
  calculate the size. Also, the user is prompted if the grid size of the
  provided cube is likely to lead to a large number of grid cells and
  long calculation times.

## b3gbi 0.8.8 - Minor update:

- Fixed bug that caused some cubes to be read incorrectly if the
  delimiting character (‘separator’ paramter in process_cube()) was not
  manually specified, without informing the user.

## b3gbi 0.8.7 - Minor update:

- The bug fix for the Hill diversity maps in version 0.8.5 was
  incomplete, as some datasets were still failing when data_type =
  “abundance” or assume_freq = TRUE. The fix now applies to all
  parameter options.

## b3gbi 0.8.6 - Minor update:

- Added support for plotting interactive maps using the mapview package.
  Use plot_mv() to call mapview on an indicator_map object.

## b3gbi 0.8.5 - Minor update:

- Fixed a small bug in Hill diversity map code that was causing some
  datasets to fail.

## b3gbi 0.8.4 - Minor update:

- Fixed a bug that caused a grid cell naming mix up during Hill
  diversity map calculation and resulted in incorrect results.
- The new data_type parameter for Hill diversity maps allows the user to
  treat their species occurrences as either incidence data
  (presence/absence with years as sampling units) or abundance data
  (number of occurrences as a proxy for abundance).
- The new assume_freq option for Hill diversity maps allows the user to
  force the treatment of occurrence counts for each species as incidence
  frequencies (number of sites at which a species was present). This is
  only valid if data_type is set to ‘incidence’ and should remain at the
  default FALSE setting unless the user is certain of the data
  structure.

## b3gbi 0.8.3 - Minor update:

- Improved and updated the introductory tutorial

## b3gbi 0.8.2 - Minor update:

- Fixed the way Hill diversity is calculated for maps. It now uses years
  as sampling units instead of aggregating across years, and estimates
  from incidence data (presence/absence) instead of using observations
  as an abundance proxy.

## b3gbi 0.8.1 - Minor update:

- Fixed multiple small bugs that led to incorrect grid cell calculation
  and assignment for quarter-degree and eea grids.

## b3gbi 0.8.0 - Major update:

- The grid is now intersected with the shapefile polygon if one is
  provided, so the grid is clipped to the shape polygon instead of the
  bounding box.
- The user can now choose to exclude occurrences on land, whereas
  previously only oceanic occurrences could be excluded. This can be
  changed using the parameter include_land = FALSE. By default, both
  ocean and land occurrences are included. Please note: this are
  geographical filters, they do not discriminate by taxa. Furthermore,
  since cubes contain pre-aggregated data, occurrences within grid cells
  that overlap both land and ocean cannot be excluded, regardless of
  whether the occurrences themselves are on land or ocean.
- The user can now select the output CRS when plotting, independently of
  the CRS of the indicator object. This can be done using the parameter
  output_crs. By default, output_crs is NULL and will use the CRS of the
  indicator object.
- Grid line width and colour, and the fill colour of empty grid cells
  can now be customized using the parameters grid_line_width,
  grid_line_colour, and grid_fill_colour.
- The width and colour of the grid outline can also be customized using
  the parameters grid_outline_width and grid_outline_colour.
- The grid outline is turned off by default, but can be turned on using
  the parameter visible_grid_outline = TRUE.
- The grid outline can be plotted as a complete rectangle even when the
  grid is not, using complete_grid_outline. Set this to ‘original’ to
  use an outline that is generally rectangular but follows the
  distortions of the grid caused by the chosen projection. Use
  ‘transformed’ to force a proper rectangular shape regardless of the
  projection. By default, complete_grid_outline is set to FALSE.
- It is now possible to crop a map according to the boundaries of the
  geographical regions you selected when calculating the indicator, even
  if they are different from the bounding box of the cube. This can be
  done using the parameter crop_by_region = TRUE. Note that this only
  works if you specified one or more region(s) when calculating the
  indicator, not if you used level = ‘cube’. By default, crop_by_region
  is set to FALSE.
- Colours and fill colours for each rnaturalearth layer that the user
  specifies when plotting a map can now be customized using the
  parameters layer_colours and layer_fill_colours. The user must provide
  a vector of colours that matches the order and number of layers
  specified in the layers parameter.
- Panel grid lines can now be turned on using the parameter
  visible_panel_gridlines. By default, panel grid lines are turned off
  (visible_panel_gridlines = FALSE).
- The parameter panel_bg has been renamed to ocean_fill_colour to better
  reflect its function.
- The parameter ne_type, which specifies the type of rnaturalearth data
  used, is now retained in the metadata of the calculated indicator (the
  level and region are still retained as well).
- Area-based automatic cell size determination now has more categories.
- Plotting code has been restructured and simplified to improve
  efficiency and readability.
- Documentation has been updated and improved.

## b3gbi 0.7.6 - Minor update:

- Fixed a bug that caused rnaturalearth to keep downloading the same
  layer data. It now downloads the data to a cache folder and loads it
  from there the next time it needs it.

## b3gbi 0.7.5 - Minor update:

- Fixed a bug that caused maps to sometimes fail to plot with an error
  about wrong object class.
- Fixed errors in the way Pielou’s evenness, Williams’ evenness, and
  taxonomic distinctness were calculated.
- Some code restructuring to reduce the size of the codebase and improve
  readability.

## b3gbi 0.7.4 - Minor update:

- Refactored code in main plotting functions to improve efficiency and
  readability.
- Fixed many style inconsistencies across the package.
- This update does not significantly affect functionality of the
  package. It is mostly about cleaner, more efficient, more readable
  code.

## b3gbi 0.7.3 - Minor update:

- Hill diversity maps now calculate without error.
- Hill diversity maps now use observations as an abundance proxy instead
  of attempting to use matrices of raw incidence data.
- Hill diversity maps no longer calculate confidence intervals as
  visualization is not supported at this time.

## b3gbi 0.7.2 - Minor update:

- Hill diversity time series indicators now calculate without error.
- The number of bootstrap confidence intervals for Hill diversity time
  series can now be set by the user, and confidence intervals can be
  turned off.
- The confidence level for Hill diversity time series can now be set by
  the user (default is 0.95).
- Taxonomic distinctness no longer tries to calculate confidence
  intervals
- If no shapefile is selected and no grid cells are required for
  calculation, time series now bypass all geometric operations and are
  thus extremely fast.
- The x and y axis expansions for plot_ts and plot_species_ts now
  default to 0.05 instead of 0.This makes time series plots look less
  cramped.

## b3gbi 0.7.1 - Minor update:

- Reorganized code in some indicator calculations to improve efficiency.

- Fixed area_rarity_ts() that was broken by the previous update.

- Note that the Hill functions are not working properly. This will
  hopefully be fixed in the next update.

## b3gbi 0.7.0 - Major update:

- Ocean occurrences are now included by default when calculating
  indicators (previously they were ignored entirely). They can be turned
  off using include_ocean = FALSE. Alternatively, you can set
  include_ocean = “buffered_coast” to include only occurrences within a
  set distance from the coast, measured in km (e.g., buffer_dist = 50).
- Shape files can now be .wkt format. The CRS cannot be automatically
  determined from .wkt files, so it is assumed to be EPSG:4326 unless
  specified using the parameter shapefile_crs.
- All geometry processing within the workflow is now done using a
  projected CRS, and conversion to the user-specified output_crs happens
  at the end. This allows for a simplified workflow and ensures more
  accurate calculations.
- The user now has the option to add any rnaturalearth layer(s) to map
  plots using the layers parameter.
- Automatic cell_size determination now takes total cube area into
  account instead of just the level (‘country’, ‘continent’, etc). Note,
  however, that the user still has the ability to manually set
  cell_size.
- Warnings from the sf package should no longer appear due to
  improvements in the way geometry operations are handled by b3gbi.
- The scale (resolution) of rnaturalearth data is now user-selectable
  when plotting (using the ‘scale’ parameter). The default is ‘medium’.
- Indicators are now calculated much faster as geometric operations have
  been simplified and reduced and code has been reorganized and
  simplified to improve efficiency.
- Grid offsets are now correct.
- Additional error checking included.
- Many minor bug fixes.

## b3gbi 0.6.3 - Minor update:

- Fixed a bug that caused quarter-degree grid codes to be translated
  incorrectly for some geographic locations. They should now work
  correctly in all cases.

## b3gbi 0.6.2 - Minor update:

- Fixed a bug related to MGRS grids that caused southern hemisphere MGRS
  codes to be associated northern hemisphere EPSG codes.

## b3gbi 0.6.1 - Minor update:

- Fixed a bug that caused an error when calculating time series from
  MGRS cube.

## b3gbi 0.6.0 - Major update:

- Fixed the way that MGRS grid codes are processed. Previously they were
  converted to lat/long and a global CRS was used. The package instead
  now converts to UTM codes and chooses the most appropriate local CRS.

## b3gbi 0.5.6 - Minor update:

- Fixed a bug in that caused quarter-degree grid codes to be translated
  incorrectly.
- Removed minimum size limitation for grid cells. Very high resolution
  grids now just give a message warning the user that it could take a
  while and suggesting a larger cell size.

## b3gbi 0.5.5 - Minor update:

- Fixed bugs in ab_rarity_map() and area_rarity_map() that caused
  incorrect calculations.
- Added more unit tests.
- Improved error handling in some functions.

## b3gbi 0.5.4 - Minor update:

- Fixed an error where spherical geometry was being turned off without a
  good reason. Spherical geometry is now only turned off to retry failed
  st_intersection() or st_difference() operations or when the user
  explicitly sets it to FALSE.
- Additional error handling was added to many functions.
- process_cube() now checks for contamination in the grid code column
  and throws an error if any false grid codes are found.
- Some code was cleaned up and made more efficient.
- Unit tests were added to the package for some functions to prevent
  breakage in future updates.

## b3gbi 0.5.3 - Minor update:

- Updated old and non-working example in process_cube() documentation.

## b3gbi 0.5.2 - Minor update:

- Fixed species occurrence and species range plots when selecting only
  one species (previously it was not possible to add additional ggplot
  code when only one species was plotted due to a bug in the plotting
  code).

## b3gbi 0.5.1 - Minor update:

- Equations in pkgdown help website now display correctly.
- Fixed bad example in spec_occ_ts() documentation.

## b3gbi 0.5.0 - Major update:

- Conversion between input and output coordinate reference systems is
  now handled correctly even if they have different units (e.g. degrees
  vs km).
- Conversion between CRSs with different units (e.g. degrees vs km) must
  now be forced by the user, and a warning is given.
- More extensive documentation is now provided for indicator functions,
  including formulas and background information.
- Users can now provide external shapefiles to limit indicator
  calculations to feature boundaries.
- Removed compatibility with previous generation cubes (created using
  TriAs project code).

## b3gbi 0.4.4 - Minor update:

- Area is no longer calculated for projections that use degrees.
- Occurrence density now refuses to calculate for projections that use
  degrees.
- Object metadata now displays ‘degrees’ instead of ‘degrees^2’.

## b3gbi 0.4.3

04.03.2025 - Minor update:

- Add power parameter for map legend scale transformations. This enables
  the use of Box-Cox, modulus, and Yeo-Johnson transformations.

## b3gbi 0.4.2

20.02.2025 - Minor update:

- Bug fix: Coordinates now display correctly after using process_cube()
  with custom grid codes

## b3gbi 0.4.1

19.02.2025 - Minor update:

- All R CMD checks are now passing without any errors, warnings or notes
- The ‘taxize’ package is no longer a dependency, but must be installed
  if you want to calculate taxonomic distinctness

## b3gbi 0.4

14.02.2025 - Major update:

- Better integration with rnaturalearth
- Fully supports b3gbi GUI functions
- Hill diversity functions are now exported and can be used

## b3gbi 0.3.1

12.02.2025 - Minor update:

- Get dependencies from recognised repositories
- Join the [R-universe](https://b-cubed-eu.r-universe.dev/)!

## b3gbi 0.3

05.12.2024 - Major update:

- Added bootstrapped confidence intervals to most indicators.
- Added many additional customization parameters for time series plots.
  See plot_ts() documentation.
- Defaults for some visualization parameters have changed.
- Fixed bug that caused errors about incorrect cell sizes when they were
  actually correct.

## b3gbi 0.2.3.1

20.11.2024 - Minor update:

- User-supplied resolutions for ‘km’-based grid systems are now properly
  handled

## b3gbi 0.2.3

07.11.2024 - Minor update:

Compatible with gcube output:

- added class sim_cube for objects without grid codes
- added ‘none’ to grid_type options

Bug fixes:

- No longer returns an error when you correctly specify a cell size when
  calculating indicators with mgrs grid

## b3gbi 0.2.2

16.07.2024 - Minor update:

- Cubes processed using process_cube_old() now have a column called
  minCoordinateUncertaintyInMeters, containing the spatial uncertainty
  from the cube. Previously the spatial uncertainty column was made
  available when using process_cube() but was ignored by
  process_cube_old().

## b3gbi 0.2.1

27.06.2024 - Minor update:

- Added options to improve control of plot output. The argument
  land_fill_colour allows you to change the colour of the land area
  outside of the grid.The argument crop_to_grid will snap the plot edges
  to the gridded area. This will improve the way the plot looks if
  e.g. your data is sparse and does not extend to the edges of a country
  or continent.
- Fixed a bug in process_cube_old that caused it to fail.
- Fixed a bug that caused calculated indicator objects to retain too
  many data columns.

## b3gbi 0.2.0

24.06.2024 - Major update:

- Added occupancy turnover to available indicators.
- All indicators in the package now working (you can get a list by
  typing ‘available_indicators’).
- The process_cube function now expects a current generation cube
  (created using the GBIF API) by default. Previous generation cubes
  (created using the TriAS project code) can still be used but must now
  be processed using a separate function, process_cube_old.
- Added support for non-standard column names (you can manually input
  column names when processing your cube, and the process_cube function
  will rename them to the Darwin Core standard expected by the rest of
  the package).
- Added support for Quarter Degree and Military grids (note that the
  Military grids are translated by an external package that is not on
  CRAN. It is hoped to replace this with native code in a later update).
- Process_cube function can now automatically find the column containing
  the grid cell codes and detect the type of grid (there are options to
  manually input the column name and grid cell type but in most cases it
  is better to leave it on automatic to avoid potential downstream
  problems).
- You can now select the output CRS you want to use for your calculated
  indicator.

## b3gbi 0.1.0
