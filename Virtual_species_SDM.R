# Code related to the paper:
# Rocchini et al. (2022). A quixotic view of spatial bias in modelling the distribution of species and their diversity.
# to create virtual species and virtual communities
# Original code by: Elisa Marchetto
# Revised code by: Jakub Nowosad and Duccio Rocchini

# packages installation ---------------------------------------------------
list.of.packages <- c("raster", "tidyverse", "sp", "virtualspecies", "ggplot2",
                      "terra", "sf", "fuzzySim", "rasterVis", "viridis", "colorist",
                      "rnaturalearth", "caret", "patchwork")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

# data downloading --------------------------------------------------------
Worldclim <- getData("worldclim", var = "bio", res = 10)

# creation of Virtual Species ---------------------------------------------
WorldclimS <- stack(Worldclim$bio1, Worldclim$bio4,
                    Worldclim$bio12, Worldclim$bio15)

WorldclimS %>%
  as.data.frame() %>%
  drop_na() %>%
  pivot_longer(bio1:bio15) %>%
  group_by(name) %>%
  summarise(across(everything(), list(mean = mean, sd = sd))) -> dfWC


qB12 <- quantile(WorldclimS$bio12)
qB15 <- quantile(WorldclimS$bio15)

# setting the climatic variables ------------------------------------------
my.parameters1 <- formatFunctions(bio1 = c(fun = "dnorm", mean = -100, sd = 50),
                                  bio4 = c(fun = "dnorm", mean = 20000, sd = 5000))

my.parameters2 <- formatFunctions(bio1 = c(fun = "dnorm", mean = 300, sd = 100),
                                  bio4 = c(fun = "dnorm", mean = 7500, sd = 2000),
                                  bio12 = c(fun = "logisticFun", beta = 259, alpha = 100),
                                  bio15 = c(fun = "dnorm", mean = 60, sd = 34))

my.parameters3 <- formatFunctions(bio1 = c(fun = "dnorm", mean = 111, sd = 31),
                                  bio15 = c(fun = "dnorm", mean = 20, sd = 20))

my.parameters4 <- formatFunctions(bio1 = c(fun = "dnorm", mean = 200, sd = 100),
                                  bio12 = c(fun = "dnorm", mean = 4000, sd = 2000))

# generating environmental suitability ------------------------------------
set.seed(999)
random.sp1 <- generateSpFromFun(WorldclimS[[c("bio1", "bio4")]],
                                parameters = my.parameters1)
plotResponse(random.sp1)

set.seed(999)
random.sp2 <- generateSpFromFun(WorldclimS,
                                parameters = my.parameters2,
                                formula = "2*bio1 + 4*bio4 + bio12 + 0.7*bio15")
plotResponse(random.sp2)

set.seed(999)
random.sp3 <- generateSpFromFun(WorldclimS[[c("bio1", "bio15")]],
                                parameters = my.parameters3,
                                formula ="2*bio1 + bio15")
plotResponse(random.sp3)

set.seed(999)
random.sp4 <- generateSpFromFun(WorldclimS[[c("bio1", "bio12")]],
                                parameters = my.parameters4)
plotResponse(random.sp4)

# converting to PA environmental suitability ------------------------------
set.seed(999)
new.pres1 <- convertToPA(random.sp1,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.2)

set.seed(999)
new.pres2 <- convertToPA(random.sp2,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.2)

set.seed(999)
new.pres3 <- convertToPA(random.sp3,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.2)

set.seed(999)
new.pres4 <- convertToPA(random.sp4,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.2)

# sampling occurrences weighted by prevalence value -----------------------
set.seed(999)
presence.points1 <- sampleOccurrences(new.pres1,
                                      n = 1000,
                                      type = "presence-absence",
                                      sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE)
set.seed(999)
presence.points2 <- sampleOccurrences(new.pres2,
                                      n = 1000,
                                      type = "presence-absence",
                                      sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE)

set.seed(999)
presence.points3 <- sampleOccurrences(new.pres3,
                                      n = 1000,
                                      type = "presence-absence",
                                      sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE)

set.seed(999)
presence.points4 <- sampleOccurrences(new.pres4,
                                      n = 1000,
                                      type = "presence-absence",
                                      sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE)

PresAbs1 <- presence.points1$sample.points[, c( "x", "y",  "Observed")]
PresAbs2 <- presence.points2$sample.points[, c( "x", "y",  "Observed")]
PresAbs3 <- presence.points3$sample.points[, c( "x", "y",  "Observed")]
PresAbs4 <- presence.points4$sample.points[, c( "x", "y",  "Observed")]

#write.csv(PresAbs1, "PresAbs1.csv")
#write.csv(PresAbs2, "PresAbs2.csv")
#write.csv(PresAbs3, "PresAbs3.csv")
#write.csv(PresAbs4, "PresAbs4.csv")

FAV <- function(PA = NULL, predictors = NULL) {
  coordinates(PA) <-  ~ x + y
  crs(PA) <- crs(predictors[[1]])
  values <- raster::extract(predictors, PA, df = TRUE)
  values <- values[, -1]
  ## collinearity test
  Vars_to_remove <-
    data.frame(BIO = findCorrelation(cor(values), cutoff = .6, names = TRUE))
  intersection1 <- colnames(values) %in% Vars_to_remove$BIO
  ## remove correlated variables
  values <- values[!intersection1]

  modSpecies <- data.frame(pres = PA@data[, 1], values[1:ncol(values)])
  FavModel <- multGLM(modSpecies,
                      sp.cols = 1,
                      var.cols = 2:ncol(modSpecies),
                      family = "binomial",
                      step = FALSE,
                      FDR = FALSE,
                      trim = FALSE,
                      Y.prediction = FALSE,
                      P.prediction = TRUE,
                      Favourability = TRUE)

  ## newdata for prediction
  XY <- as.data.frame(predictors[[1]], xy = TRUE, na.rm = TRUE)
  PTXY = data.frame("x" = XY$x, "y" = XY$y)
  predictorsDF <- raster::extract(predictors, PTXY)
  predictorsDF <- data.frame(predictorsDF)
  ## remove correlated variables
  intersection2 <- colnames(predictorsDF) %in% Vars_to_remove$BIO
  preds <- predictorsDF[!intersection2]
  FavPred <- getPreds(preds,
                      models = FavModel$models,
                      id.col = NULL,
                      Y = FALSE,
                      P = TRUE,
                      Favourability = TRUE)
  PredDataFav <- data.frame("x" = XY$x,
                            "y" = XY$y,
                            "fav" = FavPred$pres_F)
  Fav <- rasterFromXYZ(PredDataFav)
  return(Fav)
}

Fav1 <- FAV(PA = PresAbs1, predictors = Worldclim)
Fav2 <- FAV(PA = PresAbs2, predictors = Worldclim)
Fav3 <- FAV(PA = PresAbs3, predictors = Worldclim)
Fav4 <- FAV(PA = PresAbs4, predictors = Worldclim)

#writeRaster(Fav1,"Fav1sdm.tif")
#writeRaster(Fav2,"Fav2sdm.tif")
#writeRaster(Fav3,"Fav3sdm.tif")
#writeRaster(Fav4,"Fav4sdm.tif")

favsp <- stack(Fav1, Fav2, Fav3, Fav4)
world <- ne_coastline(scale = "medium", returnclass = "sf")
crs(favsp) <- crs(world)
names(favsp) <- c("Species1", "Species2", "Species3", "Species4")

# colorist for Community Distribution -------------------------------------
#Metrics pull----
metrics <- metrics_pull(favsp)
palette <- palette_set(favsp)

map_multiples(metrics, palette, ncol = 2,
              labels = c("Virtual Species 1", "Virtual Species 2",
                         "Virtual Species 3", "Virtual Species 4"))
#Metrics distill----
metricsdist <- metrics_distill(favsp)

#Map single----
p2 <- map_single(metricsdist, palette) +
  geom_sf(data = world, colour = "black", fill = "transparent") +
  ggtitle("Community of Virtual Species Distributions") +
  theme(plot.title = element_text(hjust = 0.5), plot.title.position = "plot")

p1 <- map_multiples(metrics, palette, ncol = 2,
                    labels = c("Virtual Species 1", "Virtual Species 2",
                               "Virtual Species 3", "Virtual Species 4")) +
  geom_sf(data = world, colour = "black", fill = "transparent")

map <- p1 + p2  + plot_layout(ncol = 1)

ggsave(plot = map, filename = "SDM_map.jpeg",
       width = 10, height = 10, dpi = 600)
