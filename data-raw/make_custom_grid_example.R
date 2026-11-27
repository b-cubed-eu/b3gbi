# Build the example cube with its own grid geometries (inst/extdata):
#   montenegro_bryophytes_cube_custom.csv  - cube whose cells are identified
#                                            by an arbitrary ID (grid_idx)
#   montenegro_bryophytes_grid.gpkg        - the cell polygons ("dimension
#                                            table"), EPSG:3035
# Both are derived from montenegro_bryophytes_cube_eea.csv, mimicking the
# output of cube-producing tools that deliver cell geometries separately.

cube <- read.delim("inst/extdata/montenegro_bryophytes_cube_eea.csv",
                   colClasses = "character")
cells <- sort(unique(cube$eeacellcode))

# EEA cell code "1kmE5088N2259": lower-left corner at 5088 km E, 2259 km N
x0 <- as.numeric(sub("^1kmE(\\d+)N\\d+$", "\\1", cells)) * 1000
y0 <- as.numeric(sub("^1kmE\\d+N(\\d+)$", "\\1", cells)) * 1000
polys <- lapply(seq_along(cells), function(i) {
  sf::st_polygon(list(rbind(
    c(x0[i], y0[i]), c(x0[i] + 1000, y0[i]), c(x0[i] + 1000, y0[i] + 1000),
    c(x0[i], y0[i] + 1000), c(x0[i], y0[i])
  )))
})
grid <- sf::st_sf(
  grid_idx = 1000L + seq_along(cells),
  geometry = sf::st_sfc(polys, crs = 3035)
)

cube$grid_idx <- grid$grid_idx[match(cube$eeacellcode, cells)]
cube$eeacellcode <- NULL
cube <- cube[, c("grid_idx", setdiff(names(cube), "grid_idx"))]

utils::write.table(cube, "inst/extdata/montenegro_bryophytes_cube_custom.csv",
                   sep = "\t", row.names = FALSE, quote = FALSE)
f <- "inst/extdata/montenegro_bryophytes_grid.gpkg"
if (file.exists(f)) file.remove(f)
sf::st_write(grid, f, layer = "grid", quiet = TRUE)
