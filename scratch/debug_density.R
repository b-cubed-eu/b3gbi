
devtools::load_all("c:/R_projects/b3gbi")
data(example_cube_1, package = "b3gbi")

# Replicate the mock from the test
mock_get_ne_data <- function(projected_crs, ...) {
  geom <- sf::st_sfc(sf::st_polygon(list(matrix(
    c(5, 50, 15, 50, 15, 60, 5, 60, 5, 50), ncol = 2, byrow = TRUE
  ))), crs = 4326)
  
  map_data_combined <- sf::st_sf(geometry = sf::st_transform(geom, crs = projected_crs))
  map_data_save <- sf::st_sf(geometry = sf::st_transform(geom, crs = projected_crs))
  
  list(
    combined = map_data_combined,
    saved = map_data_save
  )
}

# Monkey-patch get_ne_data
environment(mock_get_ne_data) <- asNamespace("b3gbi")
assignInNamespace("get_ne_data", mock_get_ne_data, ns = "b3gbi")

res <- spec_richness_density_map(example_cube_1, level = "country", region = "Denmark")
cat("Has non-NA:", any(!is.na(res$data$diversity_val)), "\n")
cat("nrow data:", nrow(res$data), "\n")
cat("diversity_val summary:\n")
print(summary(res$data$diversity_val))
cat("area summary:\n")
print(summary(res$data$area))
