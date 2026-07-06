
library(sf)
library(dplyr)

# Create a single large grid cell (100km)
poly <- st_polygon(list(matrix(c(0,0, 100,0, 100,100, 0,100, 0,0), ncol=2, byrow=TRUE)))
grid <- st_sf(cellid = 1, cellCode = "CELL1", geometry = st_sfc(poly))

# Create a target with two separate components
target1 <- st_polygon(list(matrix(c(-10,-10, 40,-10, 40,110, -10,110, -10,-10), ncol=2, byrow=TRUE)))
target2 <- st_polygon(list(matrix(c(60,-10, 110,-10, 110,110, 60,110, 60,-10), ncol=2, byrow=TRUE)))
target <- st_sf(id = 1:2, geometry = st_sfc(target1, target2))

# Case 1: Intersect with unioned target (as in the code)
message("Case 1: st_intersection(grid, st_union(target))")
unioned <- st_union(target)
intersected1 <- st_intersection(grid, unioned)
message("Rows: ", nrow(intersected1))
print(intersected1)

# Case 2: Intersect with non-unioned target
message("\nCase 2: st_intersection(grid, target)")
intersected2 <- st_intersection(grid, target)
message("Rows: ", nrow(intersected2))
print(intersected2)

# Now, why would Case 1 return multiple rows?
# Maybe if the result is split into separate polygons and the user expects one?
# But sf usually returns a MULTIPOLYGON if it's one row.

# What if st_union fails to truly merge them? (e.g. if they are already disjoint)
# st_union on disjoint polygons returns a MULTIPOLYGON.
# st_intersection(Polygon, MultiPolygon) should return one row.

# WAIT! What if 'grid' has multiple rows for the same cell?
# Or what if 'intersection_target' is NOT unioned in some cases?
