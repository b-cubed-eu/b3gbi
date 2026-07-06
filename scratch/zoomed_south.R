
library(sf)
library(dplyr)
library(ggplot2)
devtools::load_all(".")

set.seed(42)
grid_cells <- expand.grid(
  xcoord = seq(25, 55, by = 1),
  ycoord = seq(15, 55, by = 1)
) %>%
  mutate(
    cellCode = paste0("100kmE", xcoord, "N", ycoord),
    xcoord = xcoord * 100000,
    ycoord = ycoord * 100000,
    scientificName = "Species 1",
    obs = sample(1:50, n(), replace = TRUE),
    year = 2020,
    resolution = "100km"
  )

cube <- list(
  data = grid_cells,
  grid_type = "eea",
  first_year = 2020,
  last_year = 2020,
  num_species = 1,
  resolutions = "100km",
  coord_range = list(xmin=2500000, xmax=5500000, ymin=1500000, ymax=5500000)
)
class(cube) <- "processed_cube"

res <- compute_indicator_workflow(cube, type = "total_occ", dim_type = "map",
                                  level = "continent", region = "europe")

# Plot with ZOOMED view into southern Europe (30N-55N)
p <- plot(res, title = "Zoomed: Southern Europe Border Test")

ggsave("scratch/zoomed_south.png", p, width = 12, height = 12, dpi = 150)
message("[SUCCESS] Zoomed southern Europe map saved")
