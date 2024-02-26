#' @noRd
sample_species <- function(files_list, max_mean = 1000000, plot = FALSE) {

  species_data <- list()

  counter <- 1

  total_points <- 0

  for (i in 1:length(files_list)) {

    filename <- files_list[[i]]

    pres_abs <- readRDS(filename)

    max_pts <- nrow(pres_abs$suitab.raster[pres_abs$suitab.raster > pres_abs$suitab.raster@pnt[["range_min"]]+0.05])

    spec_prevalence <- as.numeric(pres_abs$PA.conversion[["species.prevalence"]])

    sampsize_dist <- round(rweibull(100, shape = 0.5, scale = spec_prevalence * max_mean))

    sampsize_dist <- sampsize_dist[sampsize_dist < max_pts]

    nsamp <- sample(sampsize_dist, 1)

    # Sample occurrences from the presence-absence distribution
    set.seed(NULL)
    possibleError <- tryCatch(
      presence_points <- sampleOccurrences_custom(pres_abs,
                                                n = nsamp,
                                                plot = plot,
                                                sampling.area = "Europe",
                                                replacement = TRUE,
                                                type = "presence only",
                                                projection = "epsg:3035"),
      error = function(e) print(e))

    if(inherits(possibleError, "error")) {

      print(paste0("Species ", i, " had an error. Skipping."))
      next

    }

    total_points <- total_points + nrow(presence_points$sample.points)

    Presence <- presence_points$sample.points[, c( "x", "y",  "Observed")]

    species_data[[counter]] <- Presence

    cat("\rSpecies sampled:", counter, "    Total points sampled:", total_points)

    counter <- counter + 1

  }

  return(species_data)

}
