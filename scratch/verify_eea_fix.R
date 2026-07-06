
# Reproduction Script for EEA Grid Issue
# This script simulates the duplication of grid cells that causes opaque map colors.

library(sf)
library(dplyr)

# 1. Setup a single grid cell
message("Setting up test data...")
cell <- st_polygon(list(matrix(c(0,0, 100,0, 100,100, 0,100, 0,0), ncol=2, byrow=TRUE)))
grid <- st_sf(cellid = 1, cellCode = "CELL1", geometry = st_sfc(cell))

# 2. Setup a target that will cause a split into two fragments
# Even with st_union, some map data can have slivers or overlaps that result 
# in multiple geometries after intersection. 
# We simulate the case where st_intersection returns 2 rows for 1 cell.
target1 <- st_polygon(list(matrix(c(-10,-10, 40,-10, 40,110, -10,110, -10,-10), ncol=2, byrow=TRUE)))
target2 <- st_polygon(list(matrix(c(60,-10, 110,-10, 110,110, 60,110, 60,-10), ncol=2, byrow=TRUE)))
target_union <- st_union(st_sfc(target1, target2))

# This simulates the raw st_intersection used for EEA in compute_indicator_workflow.R
message("Simulating raw st_intersection...")
clipped_grid <- st_intersection(grid, target_union)

# Currently, st_intersection of a Polygon and a MultiPolygon usually returns 1 row.
# BUT, if the target was NOT unioned (or union failed), it returns multiple rows.
# In the workflow, it uses st_union(intersection_target).
# Let's see if we can force multiple rows by using a feature collection.
target_fc <- st_sf(id = 1:2, geometry = st_sfc(target1, target2))
clipped_grid_fc <- st_intersection(grid, target_fc)

message("Number of rows for one cell after raw intersection: ", nrow(clipped_grid_fc))

# 3. Simulate the data join
# This is where the duplication happens!
indicator_data <- data.frame(cellCode = "CELL1", diversity_val = 1.0)
final_data <- left_join(clipped_grid_fc, indicator_data, by = "cellCode")

message("Number of rows in final data (should be 1): ", nrow(final_data))

if (nrow(final_data) > 1) {
  message("\n[RESULT] BUG REPRODUCED: Data duplication detected.")
  message("This explains the opaque colors (multiple overlapping polygons).")
} else {
  message("\n[RESULT] SUCCESS: No duplication detected.")
}

# 4. Check the 0.25km parsing issue too
message("\nChecking 0.25km parsing logic...")
res_text <- "0.25km"
res_val <- as.numeric(stringr::str_extract(res_text, "^\\d+"))
message("Parsed resolution value from '0.25km': ", res_val)

if (is.na(res_val) || res_val == 0) {
  message("[RESULT] BUG REPRODUCED: Decimal resolution parsed incorrectly as 0.")
} else {
  message("[RESULT] SUCCESS: Decimal resolution parsed correctly.")
}
