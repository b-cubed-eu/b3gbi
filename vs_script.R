# packages installation ---------------------------------------------------
list.of.packages <- c("raster", "tidyverse", "virtualspecies", "ggplot2", "tictoc", "snow", "doSNOW", "foreach",
                      "terra", "sf", "patchwork", "geodata", "sn", "philentropy", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

remove(list.of.packages, new.packages)

######################
# Download and prepare worldclim data using raster package
######################

# Worldclimbio <-
#   geodata::worldclim_global(var = "bio",
#                             res = 0.5,
#                             path = "inst/extdata") %>%
#   raster::stack()
# Worldclimelev <-
#   geodata::elevation_global(res = 0.5,
#                             path = "inst/extdata") %>%
#   raster::stack()
# WorldclimS <- raster::stack(c(Worldclimbio$wc2.1_30s_bio_1,
#                             Worldclimbio$wc2.1_30s_bio_4,
#                             Worldclimbio$wc2.1_30s_bio_7,
#                             Worldclimbio$wc2.1_30s_bio_12,
#                             Worldclimbio$wc2.1_30s_bio_15,
#                             Worldclimelev$wc2.1_30s_elev)) %>%
#   `names<-`(c("bio1", "bio4", "bio7", "bio12", "bio15", "elev"))
#
# temp_bbox_rast <- raster::extent(c(-25, 65, 35, 72)) %>%
#   sf::st_bbox()
#
# Worldclim <-
#   WorldclimS %>%
#   raster::crop(temp_bbox_rast)
#
# WorldclimP <-
#   Worldclim %>%
#   raster::projectRaster(crs="EPSG:3035")

######################
# Prepare and save worldclim data using terra package
######################

temp_rast <- terra::rast(nlyrs = 6,
                         res = c(547, 920),
                         xmin = 2600000,
                         xmax = 7000000,
                         ymin = 1500000,
                         ymax = 6000000,
                         crs = "epsg:3035")

wc <- terra::rast(c("inst/extdata/wc2.1_30s/wc2.1_30s_bio_1.tif",
                    "inst/extdata/wc2.1_30s/wc2.1_30s_bio_4.tif",
                    "inst/extdata/wc2.1_30s/wc2.1_30s_bio_7.tif",
                    "inst/extdata/wc2.1_30s/wc2.1_30s_bio_12.tif",
                    "inst/extdata/wc2.1_30s/wc2.1_30s_bio_15.tif",
                    "inst/extdata/wc2.1_30s/wc2.1_30s_elev.tif")) %>%
  `names<-`(c("bio1", "bio4", "bio7", "bio12", "bio15", "elev")) %>%
  terra::crop(c(-25,65,35,72)) %>%
  terra::project(temp_rast)

f <- paste0("inst/extdata/wc2.1_30s/wc2.1_30s_Eurocrop_LAEAproj_", names(wc), ".tif")

terra::writeRaster(wc, filename = f, overwrite=TRUE, gdal=c("COMPRESS=LZW"))

remove(temp_rast)

#######################
# Open prepared worldclim data using terra
#######################

wc <- terra::rast(c(f))

#######################
# Create and save virtual species using virtualspecies package
#######################

#test_vs <- createvs_custom(WorldclimP, n = 1, start_val = 43, dir = "F:/VSDB_HR_LAEA/")
test_ <- createvs_custom(wc, n = 1000, start_val = 1001, dir = "F:/VSDB_HR_LAEA/", show_occ_plot=FALSE)

files_list <- list.files("F:/VSDB_HR_LAEA/",
                         pattern = "Virtual_species_pp_",
                         full.names = TRUE)

test_vs_1 <- retrieve_samples(files_list, parallel = TRUE, num_cores = 8)

remove(files_list)

#saveRDS(test_vs, file = "F:/VSDB_HR_LAEA/test_vs_spec_HR_LAEA_26.01.2024.RData")

#test_vs <- readRDS(file = "F:/VSDB_HR/test_vs_50spec_highres_22.01.2024.RData")

# files_list <- list.files("F:/VSDB_HR_LAEA/",
#                          pattern = "Virtual_species_pa_",
#                          full.names = TRUE)
#
# test_vs <- sample_species(files_list, 5000000)

saveRDS(test_vs_1, file = "F:/VSDB_HR_LAEA/test_vs_spec_HR_LAEA_01.02.2024.RData")

# test_vs2 <- list()
# counter <- 1
# for (i in 1:length(test_vs)) {
#   if (!is.null(test_vs[[i]])) {
#     test_vs2[[counter]] <- test_vs[[i]]
#     counter <- counter + 1
#   } else { next}
# }

# Build virtual cube
test_vc <- createvirtualcube(test_vs_1)

remove(test_vs_1)

# Aggregate occurrences
test_vc2 <- aggregatevirtualcube(test_vc)

remove(test_vc)

# Convert to processed_cube class
#test_vc2 <- processed_cube(as_tibble(test_vc2))

test_vc3 <- virtual_cube(test_vc2)

remove(test_vc2)

sample_cube <- function(vc, samp_size, samp_type = "size") {

 # data <- vc$data
  data <- vc

  # Calculate total occurrences in the dataset
  total_obs <- sum(data$obs)

  if (samp_type=="proportion") {

    # if (!is.numeric(samp_size)) {
    #   stop("Sample size must be numeric. Did you include a % symbol? \n
    #        If so, please try again without the symbol.")
    # }
    if (samp_size>=1) {
      stop(paste0("Sample size must be input as a proportion
                  and must be less than 1."))
    }

    # Set all occurrence sizes to 1 before sampling
   # data$obs <- 1

    # Randomly sample a percentage of total occurrences from the cube
    samp_cube <- slice_sample(data,
                              n = round(samp_size * total_obs),
                              replace = FALSE)

  } else if (samp_type=="size") {

    if (samp_size>=total_obs) {
      stop(paste0("Sample size must be less than
                  the total number of occurrences in the cube."))
    }

    # Set all occurrence sizes to 1 before sampling
  #  data$obs <- 1

    # Randomly sample n times from cube
    samp_cube <- slice_sample(data,
                              n = samp_size,
                              replace = FALSE)

  }

  # Aggregate occurrences in grid cells
  samp_cube <-
    samp_cube %>%
    dplyr::mutate(n = sum(obs), .by = c(year, taxonKey, eea_cell_code), .keep = "all") %>%
    dplyr::distinct(year, taxonKey, eea_cell_code, .keep_all = TRUE) %>%
    dplyr::select(-obs) %>%
    dplyr::rename(obs = n)

  samp_cube <- processed_cube(as_tibble(samp_cube))

  return(samp_cube)

}

compare_ind <- function(ind1, ind2) {

  ind1 <- ind1$data$diversity_val
  ind2 <- ind2$data$diversity_val

  ind1 <- ifelse(is.na(ind1), 0, ind1)
  ind2 <- ifelse(is.na(ind2), 0, ind2)

  jval <- philentropy::jaccard(ind1, ind2, testNA = TRUE)

  return(jval)

}


compare_cubes <- function(cube, indicator, samp_sizes, proc_cube = NULL, proc_map = NULL, plot_samp_map=FALSE, ...) {

  list_results <- list()

  if (is.null(proc_cube)) {

    aggr_cube <- aggregatevirtualcube(cube)

    proc_cube <- processed_cube(as_tibble(aggr_cube))

  }

  if (is.null(proc_map)) {

    ind <- calc_map(proc_cube, type = indicator, ...)

  } else {

    if (proc_map$div_type!=indicator) {

      stop("Provided indicator map does not match desired indicator type.")

    }

    ind <- proc_map

  }

  for (i in 1:length(samp_sizes)) {

    print(paste0("Randomly sampling cube for sample ", i))

    samp_cube <- sample_cube(cube, samp_sizes[i])

    print(paste0("Calculating indicator map for sample ", i))

    samp_ind <- calc_map(samp_cube, type = indicator, ...)

    print(paste0("Comparing indicator maps for sample ", i))

    ind_result <- compare_ind(samp_ind, ind)

    list_results[[i]] <- ind_result

    names(list_results[[i]]) <- samp_sizes[i]

    f_samp <- paste0("samp_plot_", i, "_size_", samp_sizes[i])

    if (plot_samp_map == TRUE) {

      print(paste0("Plotting indicator map of sample ", i))

      samp_plot <- plot(samp_ind)

      print(paste0("Saving indicator map of sample ", i))

      ggsave(f_samp, samp_plot, device = jpeg, width = 4000, height = 8000, units = "px")

    }

    print(paste0("Completed sample ", i))

  }

  return(list_results)

}

sampled_cube1 <- sample_cube(test_vc4, 100000, samp_type = "size")
sampled_cube2 <- sample_cube(test_vc4, 0.02, samp_type = "proportion")

# Calculate observed richness
test_map2 <- calc_map(test_vc2, type = "obs_richness", level = "continent", region = "Europe", cs1 = 10, cs2 = 10)
plot(test_map2)
ggsave("test_map.jpg", last_plot(), device = jpeg, width = 4000, height = 8000, units = "px")

test_map3 <- calc_map(test_vc2, type = "obs_richness", level = "country", region = "Germany", cs1 = 1, cs2 = 1)
plot(test_map3)
ggsave("test_map2.jpg", last_plot(), device = jpeg, width = 4000, height = 8000, units = "px")

# test_sampled_map <- calc_map(sampled_cube1, type = "obs_richness",  level = "continent", region = "Europe")
# plot(test_sampled_map)

# Compare sampled and unsampled observed richness
or_dist1 <- compare_ind(test_map2, test_sampled_map)

or_dist <- compare_cubes(test_vc, "obs_richness", c(100000, 200000, 500000, 1000000), proc_cube = test_vc2, proc_map = test_map3, plot_samp_map = TRUE, cs1 = 1, cs2 = 1, level = "country", region = "Germany")

-------------

# Calculate species occurrences
test_map <- calc_map(test_vc5, type = "spec_occ", level = "continent", region = "Europe")

plot(test_map, species = c(1,2,3,4))



# Calculate density
test_map3 <- calc_map(test_vc5, type = "density", level = "continent", region = "Europe")

plot(test_map3)

test_map4 <- calc_map(test_vc5, type = "total_occ", level = "continent", region = "Europe")
plot(test_map4, trans = "log")

test_map5 <- calc_map(test_vc5, type = "pielou_evenness", level = "continent", region = "Europe")
plot(test_map5)

test_map6 <- calc_map(test_vc5, type = "area_rarity", level = "continent", region = "Europe")
plot(test_map6)
