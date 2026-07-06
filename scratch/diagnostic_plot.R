
library(sf)
library(dplyr)
library(ggplot2)
devtools::load_all(".")

# 1. Create a minimal cube
grid_cells <- expand.grid(
  xcoord = seq(25, 55, by = 1),
  ycoord = seq(15, 55, by = 1)
) %>%
  mutate(
    cellCode = paste0("100kmE", xcoord, "N", ycoord),
    xcoord = xcoord * 100000,
    ycoord = ycoord * 100000,
    resolution = "100km",
    obs = 1,
    scientificName = "Species 1"
  )

cube <- list(
  data = grid_cells,
  grid_type = "eea",
  first_year = 2020,
  last_year = 2020,
  num_species = 1,
  resolutions = "100km"
)
class(cube) <- "processed_cube"

# 2. Run workflow with clipping bypassed (as it is now)
res <- compute_indicator_workflow(
  cube, 
  type = "total_occ", 
  dim_type = "map",
  level = "continent",
  region = "europe"
)

# 3. Plot ONLY THE GRID without any background layers
p <- ggplot(res$data) +
  geom_sf(aes(fill = diversity_val), color = "black", alpha = 0.5) +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme_void() +
  labs(title = "DIAGNOSTIC: Grid ONLY (No background)")

ggsave("scratch/diagnostic_grid_only.png", p, width = 10, height = 10)
message("[SUCCESS] Diagnostic map saved to scratch/diagnostic_grid_only.png")
