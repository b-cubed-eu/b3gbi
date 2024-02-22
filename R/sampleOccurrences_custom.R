sampleOccurrences_custom <- function (x, n, type = "presence only", extract.probability = FALSE,
                                   sampling.area = NULL, detection.probability = 1, correct.by.suitability = FALSE,
                                   error.probability = 0, bias = "no.bias", bias.strength = 50,
                                   bias.area = NULL, weights = NULL, sample.prevalence = NULL,
                                   replacement = FALSE, plot = TRUE, projection = NULL)
{
  results <- list(type = type, detection.probability = list(detection.probability = detection.probability,
                                                            correct.by.suitability = correct.by.suitability), error.probability = error.probability,
                  bias = NULL, replacement = replacement, original.distribution.raster = NULL,
                  sample.plot = NULL)
  if (is.null(.Random.seed)) {
    stats::runif(1)
  }
  attr(results, "RNGkind") <- RNGkind()
  attr(results, "seed") <- .Random.seed
  if (inherits(x, "virtualspecies")) {
    if (inherits(x$occupied.area, "SpatRaster")) {
      sp.raster <- x$occupied.area
    }
    else if (inherits(x$pa.raster, "SpatRaster")) {
      sp.raster <- x$pa.raster
    }
    else stop("x must be:\n- a SpatRaster object\nor\n- the output list",
              " from functions generateRandomSp(), convertToPA() or ",
              "limitDistribution()")
  }
  else if (inherits(x, "RasterLayer")) {
    sp.raster <- rast(x)
    if (extract.probability) {
      stop("Cannot extract probability when x is not a virtualspecies object.",
           " Set extract.probability = FALSE")
    }
  }
  else if (inherits(x, "SpatRaster")) {
    sp.raster <- x
    if (extract.probability) {
      stop("Cannot extract probability when x is not a virtualspecies object.",
           " Set extract.probability = FALSE")
    }
  }
  else stop("x must be:\n- a SpatRaster object\nor\n- the output list",
            " from functions generateRandomSp(), convertToPA() or ",
            "limitDistribution()")
  if (global(sp.raster, max, na.rm = TRUE)[1, 1] > 1 | global(sp.raster,
                                                              min, na.rm = TRUE)[1, 1] < 0) {
    stop("There are values above 1 or below 0 in your presence/absence raster.",
         "Please make sure that the provided raster is a correct P/A raster",
         " and not a suitability raster.")
  }
  original.raster <- sp.raster
  results$original.distribution.raster <- wrap(original.raster)
  if (!is.null(sample.prevalence)) {
    if (sample.prevalence < 0 | sample.prevalence > 1) {
      stop("Sample prevalence must be a numeric between 0 and 1")
    }
  }
  if (!is.null(sampling.area)) {
    if (is.character(sampling.area)) {
      if (!("rnaturalearth" %in% rownames(utils::installed.packages()))) {
        stop("You need to install the package \"rnaturalearth\".")
      }
      worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
      if (!is.null(projection)) {
        worldmap <- sf::st_transform(worldmap, crs = projection)
      }
      if (any(!(sampling.area %in% c(unique(worldmap$sovereignt),
                                     unique(worldmap$region_un), unique(worldmap$continent))))) {
        stop("The choosen sampling.area is incorrectly spelled.\n Type",
             " 'unique(rnaturalearth::ne_countries(returnclass =",
             "'sf')$sovereignt)', ", "'unique(rnaturalearth::ne_countries(returnclass =",
             "'sf')$region_un)'", " & unique(rnaturalearth::ne_countries(returnclass =",
             "'sf')$continent) to obtain valid names.")
      }
      sampling.area <- worldmap[which(worldmap$sovereignt %in%
                                        sampling.area | worldmap$region_un %in% sampling.area |
                                        worldmap$continent %in% sampling.area), ]
    }
    else if (!(inherits(sampling.area, c("SpatVector", "sf",
                                         "SpatExtent")))) {
      stop("Please provide to sampling.area either \n",
           "- the names of countries, region and/or continents in which",
           " to sample\n", "- a SpatialPolygons or SpatialPolygonsDataFrame\n",
           "- an extent\n ", "in which the sampling will take place")
    }
    if (inherits(sampling.area, "SpatExtent")) {
      sampling.area <- vect(sampling.area)
    }
    sample.area.raster1 <- terra::rasterize(sampling.area,
                                            sp.raster, field = 1, background = NA, silent = TRUE)
    sp.raster <- sp.raster * sample.area.raster1
  }
  if (correct.by.suitability) {
    if (!(inherits(x, "virtualspecies")) | !("suitab.raster" %in%
                                             names(x))) {
      stop("If you choose to weight the probability of detection by the",
           " suitability of the species (i.e., correct.by.suitability = TRUE),",
           " then you need to provide an appropriate virtual species ",
           "containing a suitability raster to x.")
    }
  }
  if (!is.numeric(detection.probability) | detection.probability >
      1 | detection.probability < 0) {
    stop("detection.probability must be a numeric value between 0 and 1")
  }
  if (!is.numeric(error.probability) | error.probability >
      1 | error.probability < 0) {
    stop("error.probability must be a numeric value between 0 and 1")
  }
  if (length(bias) > 1) {
    stop("Only one bias can be applied at a time")
  }
  if (!(bias %in% c("no.bias", "country", "region", "continent",
                    "extent", "polygon", "manual"))) {
    stop("Argument bias must be one of : \"no.bias\", \"country\", \"region\", \n         \"continent\", \"extent\", \"polygon\", \"manual\"")
  }
  if (!is.numeric(bias.strength) & bias != "no.bias") {
    stop("Please provide a numeric value for bias.strength")
  }
  if (bias %in% c("country", "region", "continent")) {
    if (!("rnaturalearth" %in% rownames(utils::installed.packages()))) {
      stop("You need to install the package \"rnaturalearth\" in order to use",
           "bias = \"region\" or bias = \"country\"")
    }
    worldmap <- rnaturalearth::ne_countries(returnclass = "sf")
    if (bias == "country") {
      if (any(!(bias.area %in% worldmap$sovereignt))) {
        stop("country name(s) must be correctly spelled.",
             "Type unique(rnaturalearth::ne_countries(returnclass =",
             "'sf')$sovereignt) to obtain valid names.")
      }
      results$bias <- list(bias = bias, bias.strength = bias.strength,
                           bias.area = bias.area)
    }
    else if (bias == "region") {
      if (any(!(bias.area %in% worldmap$region_un))) {
        stop(paste("region name(s) must be correctly spelled, according to",
                   " one of the following : ", paste(unique(worldmap$region_un),
                                                     collapse = ", "), sep = "\n"))
      }
      results$bias <- list(bias = bias, bias.strength = bias.strength,
                           bias.area = bias.area)
    }
    else if (bias == "continent") {
      if (any(!(bias.area %in% worldmap$continent))) {
        stop(paste("region name(s) must be correctly spelled,",
                   "according to one of the following : ", paste(unique(worldmap$continent),
                                                                 collapse = ", "), sep = "\n"))
      }
      results$bias <- list(bias = bias, bias.strength = bias.strength,
                           bias.area = bias.area)
    }
  }
  if (bias == "polygon") {
    if (is.null(bias.area)) {
      message("No object of class SpatVector provided. A window with a map ",
              "will open, click on the map to draw the polygon of the area",
              " sampled with a bias.\n Once finished, press ",
              "escape to close the polygon.")
      if ("RStudioGD" %in% names(grDevices::dev.list())) {
        grDevices::dev.new(noRStudioGD = TRUE)
      }
      plot(sp.raster)
      bias.area <- draw(x = "polygon")
    }
    else if (!(inherits(bias.area, c("sf", "SpatVector")))) {
      stop("If you choose bias = 'polygon', please provide a polygon of class ",
           "sf or SpatVector to argument bias.area. You can also set",
           " bias.area = NULL to draw the polygon manually.")
    }
    results$bias <- list(bias = bias, bias.strength = bias.strength,
                         bias.area = bias.area)
  }
  if (bias == "extent") {
    if (is.null(bias.area)) {
      message("No object of class SpatExtent provided. A window with a map ",
              "will open, click on the map to draw the extent of the area",
              " sampled with a bias.\n Once finished, press ",
              "escape to close the polygon.")
      if ("RStudioGD" %in% names(grDevices::dev.list())) {
        grDevices::dev.new(noRStudioGD = TRUE)
      }
      plot(sp.raster)
      bias.area <- vect(draw())
    }
    else if (!(inherits(bias.area, c("SpatExtent")))) {
      stop("If you choose bias = 'extent', please provide an extent of class",
           "SpatExtent to argument bias.area. You can also set",
           " bias.area = NULL to draw the extent manually.")
    }
    else {
      bias.area <- vect(bias.area)
    }
    results$bias <- list(bias = bias, bias.strength = bias.strength,
                         bias.area = bias.area)
  }
  if (type == "presence-absence") {
    sample.raster <- sp.raster
    sample.raster[!is.na(sample.raster)] <- 1
  }
  else if (type == "presence only") {
    sample.raster <- sp.raster
  }
  else stop("type must either be 'presence only' or 'presence-absence'")
  if (bias == "manual") {
    if (!(inherits(weights, "SpatRaster"))) {
      stop("You must provide a raster layer of weights (to argument weights) \n           if you choose bias == 'manual'")
    }
    bias.raster <- weights
    results$bias <- list(bias = bias, bias.strength = "Defined by raster weights",
                         weights = wrap(weights))
  }
  else {
    bias.raster <- sample.raster
    bias.raster[bias.raster == 0] <- 1
  }
  if (bias == "country") {
    bias.raster1 <- rasterize(worldmap[which(worldmap$sovereignt %in%
                                               bias.area), ], bias.raster, field = bias.strength,
                              background = 1)
    bias.raster <- bias.raster * bias.raster1
  }
  else if (bias == "region") {
    bias.raster1 <- rasterize(worldmap[which(worldmap$region_un %in%
                                               bias.area), ], bias.raster, field = bias.strength,
                              background = 1)
    bias.raster <- bias.raster * bias.raster1
  }
  else if (bias == "continent") {
    bias.raster1 <- rasterize(worldmap[which(worldmap$continent %in%
                                               bias.area), ], bias.raster, field = bias.strength,
                              background = 1)
    bias.raster <- bias.raster * bias.raster1
  }
  else if (bias == "extent") {
    bias.raster <- bias.raster * rasterize(bias.area, sp.raster,
                                           field = bias.strength, background = 1)
    results$bias <- list(bias = bias, bias.strength = bias.strength,
                         bias.area = bias.area)
  }
  else if (bias == "polygon") {
    bias.raster1 <- rasterize(bias.area, bias.raster, field = bias.strength,
                              background = 1, silent = TRUE)
    bias.raster <- bias.raster * bias.raster1
  }
  if (type == "presence only") {
    number.errors <- stats::rbinom(n = 1, size = n, prob = error.probability)
    error.raster <- sample.raster
    error.raster[error.raster == 0] <- 1
    if (number.errors > 0) {
      sample.points <- spatSample(error.raster * bias.raster,
                                  size = number.errors, method = "weights", xy = TRUE,
                                  values = FALSE, replace = replacement)
    }
    else {
      sample.points <- data.frame()
    }
    sample.points <- rbind(sample.points, spatSample(sample.raster *
                                                       bias.raster, size = n - number.errors, method = "weights",
                                                     xy = TRUE, values = FALSE, replace = replacement))
  }
  else {
    if (is.null(sample.prevalence)) {
      sample.points <- spatSample(sample.raster * bias.raster,
                                  size = n, method = "weights", xy = TRUE, values = FALSE,
                                  replace = replacement)
    }
    else {
      tmp1 <- sample.raster
      tmp1[sp.raster != 1] <- NA
      sample.points <- spatSample(tmp1 * bias.raster,
                                  size = sample.prevalence * n, method = "weights",
                                  xy = TRUE, values = FALSE, replace = replacement)
      tmp1 <- sample.raster
      tmp1[sp.raster != 0] <- NA
      tmp1[tmp1 == 0] <- 1
      sample.points <- rbind(sample.points, spatSample(tmp1 *
                                                         bias.raster, size = (1 - sample.prevalence) *
                                                         n, method = "weights", xy = TRUE, values = FALSE,
                                                       replace = replacement))
      rm(tmp1)
    }
  }
  sample.points <- sample.points[, c(1, 2)]
  if (type == "presence only") {
    sample.points <- data.frame(sample.points, Real = extract(sp.raster,
                                                              sample.points, ID = FALSE), Observed = sample(c(NA,
                                                                                                              1), size = nrow(sample.points), prob = c(1 - detection.probability,
                                                                                                                                                       detection.probability), replace = TRUE))
    colnames(sample.points)[3] <- "Real"
  }
  else if (type == "presence-absence") {
    sample.points <- data.frame(sample.points, extract(sp.raster,
                                                       sample.points, ID = FALSE))
    colnames(sample.points)[3] <- "Real"
    if (correct.by.suitability) {
      suitabs <- extract(x$suitab.raster, sample.points[,
                                                        c("x", "y")], ID = FALSE)[, 1]
    }
    else {
      suitabs <- rep(1, nrow(sample.points))
    }
    sample.points$Observed <- NA
    if (correct.by.suitability) {
      sample.points$Observed[which(sample.points$Real ==
                                     1)] <- sapply(detection.probability * suitabs[which(sample.points$Real ==
                                                                                           1)], function(y) {
                                                                                             sample(c(0, 1), size = 1, prob = c(1 - y, y))
                                                                                           })
    }
    else {
      sample.points$Observed[which(sample.points$Real ==
                                     1)] <- sample(c(0, 1), size = length(which(sample.points$Real ==
                                                                                  1)), prob = c(1 - detection.probability, detection.probability),
                                                   replace = TRUE)
    }
    sample.points$Observed[which(sample.points$Real == 0 |
                                   sample.points$Observed == 0)] <- sample(c(0, 1),
                                                                           size = length(which(sample.points$Real == 0 | sample.points$Observed ==
                                                                                                 0)), prob = c(1 - error.probability, error.probability),
                                                                           replace = TRUE)
  }
  if (plot) {
    plot(original.raster, col = rev(viridis::viridis(3)[2:3]))
    if (type == "presence only") {
      graphics::points(sample.points[, c("x", "y")], pch = 16,
                       cex = 0.5, col = viridis::viridis(3)[1])
    }
    else {
      graphics::points(sample.points[sample.points$Observed ==
                                       1, c("x", "y")], pch = 16, cex = 0.8)
      graphics::points(sample.points[sample.points$Observed ==
                                       0, c("x", "y")], pch = 1, cex = 0.8)
    }
    results$sample.plot <- grDevices::recordPlot()
  }
  if (extract.probability) {
    sample.points <- data.frame(sample.points, extract(x$probability.of.occurrence,
                                                       sample.points[, c("x", "y")], ID = FALSE))
    colnames(sample.points)[ncol(sample.points)] <- "true.probability"
  }
  results$sample.points <- sample.points
  if (type == "presence-absence") {
    true.prev <- length(sample.points$Real[which(sample.points$Real ==
                                                   1)])/nrow(sample.points)
    obs.prev <- length(sample.points$Real[which(sample.points$Observed ==
                                                  1)])/nrow(sample.points)
    results$sample.prevalence <- c(true.sample.prevalence = true.prev,
                                   observed.sample.prevalence = obs.prev)
  }
  class(results) <- append("VSSampledPoints", class(results))
  return(results)
}
