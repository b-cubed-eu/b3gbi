# Code related to the paper:
# Rocchini et al. (2022). A quixotic view of spatial bias in modelling the distribution of species and their diversity.
# to create virtual species and virtual communities
# Original code by: Elisa Marchetto
# Revised code by: Jakub Nowosad and Duccio Rocchini

# This is an extract only for creating virtual species. The rest is removed

# packages installation ---------------------------------------------------
list.of.packages <- c("raster", "tidyverse", "virtualspecies", "ggplot2",
                      "terra", "sf", "patchwork", "geodata", "sn", "philentropy", "data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

# data downloading --------------------------------------------------------
Worldclimbio <- geodata::worldclim_global(var = "bio", res = 10, path = "inst/extdata") %>%
  raster::stack()
Worldclimelev <- geodata::elevation_global(res = 10, path = "inst/extdata") %>%
  raster::stack()

# creation of Virtual Species ---------------------------------------------
WorldclimS <- raster::stack(Worldclimbio$wc2.1_10m_bio_1,
                            Worldclimbio$wc2.1_10m_bio_4,
                            Worldclimbio$wc2.1_10m_bio_7,
                            Worldclimbio$wc2.1_10m_bio_12,
                            Worldclimbio$wc2.1_10m_bio_15,
                            Worldclimelev$wc2.1_10m_elev)

names(WorldclimS) <- c("bio1", "bio4", "bio7", "bio12", "bio15", "elev")

dfWC <- WorldclimS %>%
  as.data.frame() %>%
  drop_na() %>%
  pivot_longer(c(bio1:bio15, elev)) %>%
  group_by(name) %>%
  summarise(across(everything(), list(mean = mean, sd = sd)))

qB1 <- quantile(WorldclimS$bio1)
qB4 <- quantile(WorldclimS$bio4)
qB7 <- quantile(WorldclimS$bio7)
qB12 <- quantile(WorldclimS$bio12)
qB15 <- quantile(WorldclimS$bio15)
qBelev <- quantile(WorldclimS$elev)

# setting the climatic variables ------------------------------------------

# Here we should call a function to (randomly?) select variables and mean and standard
# deviations for each variable. Reasonable ranges can be provided to ensure nothing
# too unrealistic.The type of distribution can either be set or randomly chosen from
# a vector according to the parameter type.
# The number of variables to select should also involve some randomness.
# But for now, there also need to be testing performed with set parameters.
# This can also be provided by a function though...
get_means_from_seq <- function(x, y, z, n) {
  mean_val <- sample(n, seq(x, y, z))
}


my.parameters1 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 85, sd = 3),
                                  wc2.1_10m_bio_4 = c(fun = "dnorm", mean = 600, sd = 200),
                                  wc2.1_10m_bio_7 = c(fun = "dnorm", mean = 30, sd = 5),
                                  wc2.1_10m_bio_12 = c(fun = "dnorm", mean = 600, sd = 300),
                                  wc2.1_10m_bio_15 = c(fun = "dnorm", mean = 60, sd = 34),
                                  wc2.1_10m_elev = c(fun = "dnorm", mean = 100, sd = 5000))

my.parameters2 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 15, sd = 10),
                                  wc2.1_10m_bio_4 = c(fun = "dnorm", mean = 750, sd = 200),
                                  wc2.1_10m_bio_12 = c(fun = "dnorm", mean = 200, sd = 100),
                                  wc2.1_10m_bio_15 = c(fun = "dnorm", mean = 60, sd = 34))

my.parameters3 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 11, sd = 31),
                                  wc2.1_10m_bio_15 = c(fun = "dnorm", mean = 50, sd = 20))

my.parameters4 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 20, sd = 10),
                                  wc2.1_10m_bio_12 = c(fun = "dnorm", mean = 400, sd = 200))

my.parameters5 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 30, sd = 10),
                                  wc2.1_10m_bio_4 = c(fun = "dnorm", mean = 300, sd = 100),
                                  wc2.1_10m_bio_7 = c(fun = "dnorm", mean = 30, sd = 10),
                                  wc2.1_10m_bio_12 = c(fun = "dnorm", mean = 600, sd = 200),
                                  wc2.1_10m_bio_15 = c(fun = "dnorm", mean = 80, sd = 15))

my.parameters6 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 25, sd = 5),
                                  wc2.1_10m_bio_4 = c(fun = "dnorm", mean = 1200, sd = 300),
                                  wc2.1_10m_bio_7 = c(fun = "dnorm", mean = 30, sd = 20),
                                  wc2.1_10m_bio_15 = c(fun = "dnorm", mean = 80, sd = 50),
                                  wc2.1_10m_elev = c(fun = "dnorm", mean = 200, sd = 500))

my.parameters7 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 2, sd = 10),
                                  wc2.1_10m_bio_4 = c(fun = "dnorm", mean = 1000, sd = 200),
                                  wc2.1_10m_bio_12 = c(fun = "dnorm", mean = 1000, sd = 500),
                                  wc2.1_10m_bio_15 = c(fun = "dnorm", mean = 100, sd = 10))

my.parameters8 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 10, sd = 10),
                                  wc2.1_10m_bio_4 = c(fun = "dnorm", mean = 900, sd = 80),
                                  wc2.1_10m_bio_7 = c(fun = "dnorm", mean = 30, sd = 20),
                                  wc2.1_10m_bio_12 = c(fun = "dnorm", mean = 800, sd = 200),
                                  wc2.1_10m_elev = c(fun = "dnorm", mean = 400, sd = 600))

my.parameters9 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 17, sd = 10),
                                  wc2.1_10m_bio_4 = c(fun = "dnorm", mean = 450, sd = 150),
                                  wc2.1_10m_bio_12 = c(fun = "dnorm", mean = 1500, sd = 800),
                                  wc2.1_10m_bio_15 = c(fun = "dnorm", mean = 60, sd = 20),
                                  wc2.1_10m_elev = c(fun = "dnorm", mean = 800, sd = 300))

my.parameters10 <- formatFunctions(wc2.1_10m_bio_1 = c(fun = "dnorm", mean = 12, sd = 7),
                                  wc2.1_10m_bio_7 = c(fun = "dnorm", mean = 30, sd = 15),
                                  wc2.1_10m_bio_15 = c(fun = "dnorm", mean = 100, sd = 43))



# generating environmental suitability ------------------------------------

# Need a function here that will automatically subset the raster stack based on
# the parameters chosen above for each species. Also need some way of automating
# the creation of formulas based on the parameters.That could be the tricky part.

set.seed(999)
random.sp1 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                              "wc2.1_10m_bio_4",
                                              "wc2.1_10m_bio_7",
                                              "wc2.1_10m_bio_12",
                                              "wc2.1_10m_bio_15",
                                              "wc2.1_10m_elev")]],
                                parameters = my.parameters1,
                                plot = TRUE)
plotResponse(random.sp1)

set.seed(999)
random.sp2 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                              "wc2.1_10m_bio_4",
                                              "wc2.1_10m_bio_12",
                                              "wc2.1_10m_bio_15")]],
                                parameters = my.parameters2,
                                formula = "2*wc2.1_10m_bio_1 +
                                4*wc2.1_10m_bio_4 +
                                wc2.1_10m_bio_12 +
                                0.7*wc2.1_10m_bio_15")
plotResponse(random.sp2)

set.seed(999)
random.sp3 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                              "wc2.1_10m_bio_15")]],
                                parameters = my.parameters3,
                                formula ="2*wc2.1_10m_bio_1 +
                                wc2.1_10m_bio_15")
plotResponse(random.sp3)

set.seed(999)
random.sp4 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                              "wc2.1_10m_bio_12")]],
                                parameters = my.parameters4,
                                formula ="wc2.1_10m_bio_1 +
                                wc2.1_10m_bio_12")
plotResponse(random.sp4)

set.seed(999)
random.sp5 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                              "wc2.1_10m_bio_4",
                                              "wc2.1_10m_bio_7",
                                              "wc2.1_10m_bio_12",
                                              "wc2.1_10m_bio_15")]],
                                parameters = my.parameters5,
                                formula = "2*wc2.1_10m_bio_1 +
                                4*wc2.1_10m_bio_4 +
                                wc2.1_10m_bio_7 +
                                wc2.1_10m_bio_12 +
                                0.7*wc2.1_10m_bio_15")
plotResponse(random.sp5)

set.seed(999)
random.sp6 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                              "wc2.1_10m_bio_4",
                                              "wc2.1_10m_bio_7",
                                              "wc2.1_10m_bio_15",
                                              "wc2.1_10m_elev")]],
                                parameters = my.parameters6,
                                formula = "wc2.1_10m_bio_1 +
                                wc2.1_10m_bio_4 +
                                wc2.1_10m_bio_7 +
                                wc2.1_10m_bio_15 +
                                wc2.1_10m_elev")
plotResponse(random.sp6)

set.seed(999)
random.sp7 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                              "wc2.1_10m_bio_4",
                                              "wc2.1_10m_bio_12",
                                              "wc2.1_10m_bio_15")]],
                                parameters = my.parameters7,
                                formula = "2*wc2.1_10m_bio_1 +
                                4*wc2.1_10m_bio_4 +
                                wc2.1_10m_bio_12 +
                                0.5*wc2.1_10m_bio_15")
plotResponse(random.sp7)

set.seed(999)
random.sp8 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                              "wc2.1_10m_bio_4",
                                              "wc2.1_10m_bio_7",
                                              "wc2.1_10m_bio_12",
                                              "wc2.1_10m_elev")]],
                                parameters = my.parameters8,
                                formula = "wc2.1_10m_bio_1 +
                                2*wc2.1_10m_bio_4 +
                                2*wc2.1_10m_bio_7 +
                                0.7*wc2.1_10m_bio_12 +
                                wc2.1_10m_elev")
plotResponse(random.sp8)

set.seed(999)
random.sp9 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                              "wc2.1_10m_bio_4",
                                              "wc2.1_10m_bio_12",
                                              "wc2.1_10m_bio_15",
                                              "wc2.1_10m_elev")]],
                                parameters = my.parameters9,
                                formula = "wc2.1_10m_bio_1 +
                                wc2.1_10m_bio_4 +
                                wc2.1_10m_bio_12 +
                                wc2.1_10m_bio_15 +
                                2*wc2.1_10m_elev")
plotResponse(random.sp9)

set.seed(999)
random.sp10 <- generateSpFromFun(WorldclimS[[c("wc2.1_10m_bio_1",
                                               "wc2.1_10m_bio_7",
                                               "wc2.1_10m_bio_15")]],
                                parameters = my.parameters10,
                                formula = "wc2.1_10m_bio_1 +
                                2*wc2.1_10m_bio_7 +
                                wc2.1_10m_bio_15")
plotResponse(random.sp10)


# converting to PA environmental suitability ------------------------------
set.seed(999)
new.pres1 <- convertToPA(random.sp1,
                         prob.method = "linear",
                         beta ="random",
                         alpha = -0.05, plot = TRUE)
plot(new.pres1)

set.seed(999)
new.pres2 <- convertToPA(random.sp2,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.2)
plot(new.pres2)

set.seed(999)
new.pres3 <- convertToPA(random.sp3,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.1)
plot(new.pres3)

set.seed(999)
new.pres4 <- convertToPA(random.sp4,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.02)
plot(new.pres4)

set.seed(999)
new.pres5 <- convertToPA(random.sp5,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.05)
plot(new.pres5)

set.seed(999)
new.pres6 <- convertToPA(random.sp6,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.1)
plot(new.pres6)

set.seed(999)
new.pres7 <- convertToPA(random.sp7,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.1)
plot(new.pres7)

set.seed(999)
new.pres8 <- convertToPA(random.sp8,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.05)
plot(new.pres8)

set.seed(999)
new.pres9 <- convertToPA(random.sp9,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.01)
plot(new.pres9)

set.seed(999)
new.pres10 <- convertToPA(random.sp10,
                         beta ="random",
                         alpha = -0.05, plot = FALSE,
                         species.prevalence = 0.04)
plot(new.pres10)

# sampling occurrences weighted by prevalence value -----------------------
nsamp <- 10000
set.seed(999)
presence.points1 <- sampleOccurrences(new.pres1,
                                      n = nsamp,
                                      type = "presence only",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)
set.seed(999)
presence.points2 <- sampleOccurrences(new.pres2,
                                      n = nsamp,
                                      type = "presence-absence",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)

set.seed(999)
presence.points3 <- sampleOccurrences(new.pres3,
                                      n = nsamp,
                                      type = "presence-absence",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)

set.seed(999)
presence.points4 <- sampleOccurrences(new.pres4,
                                      n = nsamp,
                                      type = "presence-absence",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)

set.seed(999)
presence.points5 <- sampleOccurrences(new.pres5,
                                      n = nsamp,
                                      type = "presence-absence",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)

set.seed(999)
presence.points6 <- sampleOccurrences(new.pres6,
                                      n = nsamp,
                                      type = "presence-absence",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)

set.seed(999)
presence.points7 <- sampleOccurrences(new.pres7,
                                      n = nsamp,
                                      type = "presence-absence",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)

set.seed(999)
presence.points8 <- sampleOccurrences(new.pres8,
                                      n = 10000,
                                      type = "presence-absence",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)

set.seed(999)
presence.points9 <- sampleOccurrences(new.pres9,
                                      n = nsamp,
                                      type = "presence-absence",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)

set.seed(999)
presence.points10 <- sampleOccurrences(new.pres10,
                                      n = nsamp,
                                      type = "presence-absence",
                                      #sample.prevalence = 0.5,
                                      detection.probability = 1,
                                      correct.by.suitability = FALSE,
                                      plot = FALSE,
                                      sampling.area = "Europe",
                                      replacement = TRUE)

PresAbs1 <- presence.points1$sample.points[, c( "x", "y",  "Observed")]
PresAbs2 <- presence.points2$sample.points[, c( "x", "y",  "Observed")]
PresAbs3 <- presence.points3$sample.points[, c( "x", "y",  "Observed")]
PresAbs4 <- presence.points4$sample.points[, c( "x", "y",  "Observed")]
PresAbs5 <- presence.points5$sample.points[, c( "x", "y",  "Observed")]
PresAbs6 <- presence.points6$sample.points[, c( "x", "y",  "Observed")]
PresAbs7 <- presence.points7$sample.points[, c( "x", "y",  "Observed")]
PresAbs8 <- presence.points8$sample.points[, c( "x", "y",  "Observed")]
PresAbs9 <- presence.points9$sample.points[, c( "x", "y",  "Observed")]
PresAbs10 <- presence.points10$sample.points[, c( "x", "y",  "Observed")]



