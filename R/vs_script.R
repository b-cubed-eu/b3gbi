# Create raster stack from worldclim data
Worldclimbio <- geodata::worldclim_global(var = "bio", res = 10, path = "inst/extdata") %>%
  raster::stack()
Worldclimelev <- geodata::elevation_global(res = 10, path = "inst/extdata") %>%
  raster::stack()
WorldclimS <- raster::stack(Worldclimbio$wc2.1_10m_bio_1,
                            Worldclimbio$wc2.1_10m_bio_4,
                            Worldclimbio$wc2.1_10m_bio_7,
                            Worldclimbio$wc2.1_10m_bio_12,
                            Worldclimbio$wc2.1_10m_bio_15,
                            Worldclimelev$wc2.1_10m_elev)

# Make simpler names
names(WorldclimS) <- c("bio1", "bio4", "bio7", "bio12", "bio15", "elev")

# Create virtual species
test_vs <- createvs_custom(WorldclimS, 300, start_val = 347)

saveRDS(test_vs, file = "test_vs_300spec_17.01.2024.RData")

# Build virtual cube
test_vc <- createvirtualcube(test_vs)

# Convert to processed_cube class
test_vc <- processed_cube(as_tibble(test_vc))

# Calculate species occurrences
test_map <- calc_map(test_vc, type = "spec_occ", level = "continent", region = "Europe")

plot(test_map, species = c(1,2,3,4))

# Calculate observed richness
test_map2 <- calc_map(test_vc, type = "obs_richness", level = "continent", region = "Europe")

plot(test_map2)

# Calculate density
test_map3 <- calc_map(test_vc, type = "density", level = "continent", region = "Europe")

plot(test_map3)

test_map4 <- calc_map(test_vc, type = "total_occ", level = "continent", region = "Europe")
plot(test_map4, trans = "log")

test_map5 <- calc_map(test_vc, type = "pielou_evenness", level = "continent", region = "Europe")
plot(test_map5)

test_map6 <- calc_map(test_vc, type = "area_rarity", level = "continent", region = "Europe")
plot(test_map6)
