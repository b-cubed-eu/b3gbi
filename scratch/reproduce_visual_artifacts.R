
library(b3gbi)
library(sf)
library(dplyr)
library(ggplot2)

# 1. Create a grid of 100km cells for Europe (simplified)
# Center of Europe is around 4.3M, 3.2M in EPSG:3035
grid_cells <- expand.grid(
  x = seq(30, 60, by = 1),
  y = seq(15, 45, by = 1)
) %>%
  mutate(
    cellCode = paste0("100kmE", x, "N", y),
    xcoord = x * 100000,
    ycoord = y * 100000,
    cellid = row_number()
  )

# Create polygons
polys <- mapply(function(x, y) {
  st_polygon(list(matrix(c(x,y, x+100000,y, x+100000,y+100000, x,y+100000, x,y), ncol=2, byrow=TRUE)))
}, grid_cells$xcoord, grid_cells$ycoord, SIMPLIFY = FALSE)

grid_sf <- st_sf(grid_cells, geometry = st_sfc(polys, crs = "EPSG:3035"))

# 2. Simulate "Opaque below a line"
# We'll pick a line at y=32 (mid Germany)
# Below y=32, we'll duplicate the data 50 times per cell
# Above y=32, we'll have 1 row per cell
data_normal <- grid_cells %>% filter(y >= 32) %>% mutate(diversity_val = 0.5)
data_messy <- grid_cells %>% filter(y < 32) %>% 
  slice(rep(1:n(), each = 50)) %>% # 50 layers
  mutate(diversity_val = 0.8)

data_indicator <- bind_rows(data_normal, data_messy)

# 3. Create an indicator_map object
ind_map <- list(
  data = left_join(grid_sf, data_indicator, by = "cellCode") %>% select(cellid.x, cellCode, diversity_val),
  div_type = "obs_richness",
  projection = "EPSG:3035",
  map_level = "cube",
  coord_range = st_bbox(grid_sf),
  grid_type = "eea"
)
names(ind_map$data)[1] <- "cellid"
class(ind_map) <- "indicator_map"

# 4. Plot it
message("Creating messed up map plot...")
p <- plot_map(ind_map, title = "Simulation of Opaque Artifacts")

# Save the plot
ggsave("scratch/messed_up_map.png", p, width = 8, height = 8)
message("Plot saved to scratch/messed_up_map.png")

# 5. Reproduce the 'Line through Germany' caused by coordinate error
# We'll add one cell with a huge coordinate error
huge_error_cell <- data.frame(
  cellid = 9999,
  cellCode = "100kmE4321N3210",
  diversity_val = 1.0,
  geometry = st_sfc(st_polygon(list(matrix(c(432100000, 321000000, 432200000, 321000000, 432200000, 321100000, 432100000, 321100000, 432100000, 321000000), ncol=2, byrow=TRUE))), crs = "EPSG:3035")
) %>% st_as_sf()

ind_map_error <- ind_map
ind_map_error$data <- bind_rows(ind_map$data, huge_error_cell)

p_error <- plot_map(ind_map_error, title = "Simulation of Line Artifact")
ggsave("scratch/line_error_map.png", p_error, width = 8, height = 8)
message("Plot saved to scratch/line_error_map.png")
