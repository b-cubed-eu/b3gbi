
library(sf)
library(dplyr)
devtools::load_all(".")

# Check the coordinates of the EEA origin cell
cell <- "100kmE43N32"
coords <- eea_code_to_coords(cell)
message("Decoded coordinates for ", cell, ":")
print(coords)

# Create a point and transform to WGS84
pt <- st_sfc(st_point(c(coords$xcoord + 50000, coords$ycoord + 50000)), crs = "EPSG:3035")
pt_wgs84 <- st_transform(pt, "EPSG:4326")
message("WGS84 center of ", cell, ":")
print(st_coordinates(pt_wgs84))

# Check the Northing of 40N, 10E
ref_pt <- st_sfc(st_point(c(10, 40)), crs = "EPSG:4326")
ref_pt_3035 <- st_transform(ref_pt, "EPSG:3035")
message("EPSG:3035 coordinates of 40N, 10E:")
print(st_coordinates(ref_pt_3035))
# Northing should be around 3210000 - (12 degrees * 111km) approx 1.9M.
# So it should be N19 or N20.

# Check my natural_reproduction grid
grid_cells <- expand.grid(xcoord = 43, ycoord = 15:55) %>%
  mutate(cellCode = paste0("100kmE", xcoord, "N", ycoord))
res <- eea_code_to_coords(grid_cells$cellCode)
# No need to multiply, the function does it.

# Let's check where N20 is
n20_pt <- st_sfc(st_point(c(4350000, 2050000)), crs = "EPSG:3035")
n20_wgs84 <- st_transform(n20_pt, "EPSG:4326")
message("WGS84 center of 100kmE43N20:")
print(st_coordinates(n20_wgs84))
