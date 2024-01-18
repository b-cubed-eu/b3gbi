# Function to create virtual species
createvs_custom <- function(clim_data, n, start_val=1) {

  species_data <- list()

  layernames <- vector()
  min <- vector()
  max <- vector()
  for (i in 1:length(clim_data@layers)) {
    layernames[i] <- clim_data@layers[[i]]@data@names
    min[i] <- clim_data@layers[[i]]@data@min
    max[i] <- clim_data@layers[[i]]@data@max
  }

  # Create reasonable parameter distributions
  set.seed(NULL)
  # annual_mean_temp <- rnorm(1000, mean = 8, sd = 8)
  annual_mean_temp <- sn::rsn(1000,
                              xi = 35, # xi is location (can be negative)
                              omega = 17, # omega is scale (positive only)
                              alpha = -6.5) # alpha is shape (can be negative)
  annual_mean_temp <- annual_mean_temp[annual_mean_temp > min[1] & annual_mean_temp < max[1]]
  temp_seasonality <- rnorm(1000, mean = 1000, sd = 400)
  temp_seasonality <- temp_seasonality[temp_seasonality > min[2] & temp_seasonality < max[2]]
  #temp_annual_range <- rnorm(1000, mean = 40, sd = 10)
  temp_annual_range <- rweibull(1000, 1.7, scale = 20)
  temp_annual_range <- temp_annual_range[temp_annual_range > min[3] & temp_annual_range < max[3]]
  #annual_precip <- rweibull(1000, shape = 1.8, scale = 1000)
  annual_precip <- runif(1000, min = 0, max = 5000)
  annual_precip <- annual_precip[annual_precip > min[4] & annual_precip < max[4]]
  precip_seasonality <- rnorm(1000, mean = 100, sd = 40)
  precip_seasonality <- precip_seasonality[precip_seasonality > min[5] & precip_seasonality < max[5]]
  elevation <- rweibull(1000, 1, scale = 500)
  elevation <- elevation[elevation > min[6] & elevation < max[6]]

  amt_sd_dist <- rpois(1000, lambda = 5)
  amt_sd_dist <- amt_sd_dist[amt_sd_dist > 1]
  ts_sd_dist <- rnorm(1000, mean = 300, sd = 200)
  ts_sd_dist <- ts_sd_dist[ts_sd_dist > 100]
  # tar_sd_dist <- rpois(1000, lambda = 10)
  tar_sd_dist <- rweibull(1000, 2, scale = 15)
  tar_sd_dist <- tar_sd_dist[tar_sd_dist > 5]
  ap_sd_percent_dist <- rweibull(1000, 1.8, scale = 100) # this will be used as a percentage of the mean ap
  ap_sd_percent_dist <- ap_sd_percent_dist[ap_sd_percent_dist > 20]
  ps_sd_dist <- rnorm(1000, mean = 50, sd = 20)
  ps_sd_dist <- ps_sd_dist[ps_sd_dist > 20 & ps_sd_dist < 100]
  el_sd_dist <- rnorm(1000, mean = 1000, sd = 500)
  el_sd_dist <- el_sd_dist[el_sd_dist > 0]

  # nsampdist <- round(rweibull(1000, 0.5, nsampmean))
  # nsampdist <- nsampdist[nsampdist >= 1]

  # function to sample from parameter distributions for individual species
  psample <- function(pdist, n) {
    param_pool <- sample(pdist, n, replace = TRUE)
  }

  i <- start_val
  j <- 0
  counter <- 1
  while (counter <= n) {

    set.seed(NULL)
    amt <- sample(annual_mean_temp, 1)
    ts <- sample(temp_seasonality, 1)
    # if (amt < 20) {temp_annual_range <- temp_annual_range[temp_annual_range > 20]}
    # else {temp_annual_range <- temp_annual_range[temp_annual_range < 30]}
    tar <- sample(temp_annual_range, 1)
    # if (amt > 20 & tar < 15) {annual_precip <- annual_precip[annual_precip > 1000]}
    # else if (amt > 20 & tar > 25) {annual_precip <- annual_precip[annual_precip < 400]}
    ap <- sample(annual_precip, 1)
    ps <- sample(precip_seasonality, 1)
    el <- sample(elevation, 1)

    amt_sd <- sample(amt_sd_dist, 1)
    ts_sd <- sample(ts_sd_dist, 1)
    tar_sd <- sample(tar_sd_dist, 1)
    ap_sd <- (sample(ap_sd_percent_dist, 1) * ap) / 100
    ps_sd <- sample(ps_sd_dist, 1)
    el_sd <- sample(el_sd_dist, 1)

    # set.seed(NULL)
    # nsamp = sample(nsampdist, 1)

    # Format parameter function
    spec_parameters <-
      formatFunctions(bio1 = c(fun = "dnorm",
                               mean = amt,
                               sd = amt_sd),
                      bio4 = c(fun = "dnorm",
                               mean = ts,
                               sd = ts_sd),
                      bio7 = c(fun = "dnorm",
                               mean = tar,
                               sd = tar_sd),
                      bio12 = c(fun = "dnorm",
                                mean = ap,
                                sd = ap_sd),
                      #bio15 = c(fun = "dnorm",
                      #                     mean = ps,
                      #                    sd = ps_sd),
                      elev = c(fun = "dnorm",
                               mean = el,
                               sd = el_sd))

    assign(paste0("spec_parameters_", i), spec_parameters)

    req_parameters <- c(spec_parameters[1], spec_parameters[4])

    opt_parameters <- spec_parameters[!(spec_parameters %in% req_parameters)]

    rand_parameters <- c(req_parameters,
                         sample(opt_parameters, sample(1:length(opt_parameters), 1)))

    params <- list("annual mean temp" = amt,
                   "amt sd" = amt_sd,
                   "temperature seasonality" = ts,
                   "ts sd" = ts_sd,
                   "temperature annual range" = tar,
                   "tar sd" = tar_sd,
                   "annual precipitation" = ap,
                   "ap sd" = ap_sd,
                   "precipitation seasonality" = ps,
                   "ps sd" = ps_sd,
                   "elevation" = el,
                   "el sd" = el_sd,
                   "parameters" = rand_parameters)

    capture.output(params, file = paste("F:/VSDB2/Params_", i, ".txt", sep = ""))

    # Generate virtual species distribution
    set.seed(NULL)
    spec_dist <-
      generateSpFromFun(WorldclimS[[c(names(rand_parameters))]],
                        parameters = rand_parameters,
                        plot = TRUE)

    assign(paste0("spec_dist_", i), spec_dist)

    # Set maximum species prevalence to avoid a bug where it occurs everywhere despite having a narrow suitability
    max_sp <- nrow(spec_dist$suitab.raster[spec_dist$suitab.raster >
                                             spec_dist$suitab.raster@pnt[["range_min"]]+0.05]) /
      nrow(spec_dist$suitab.raster[spec_dist$suitab.raster > 0])

    # Create distribution for species prevalence
    sp_dist <- runif(1000, min = 0.0001, max = max_sp)

    sp_good <- 0
    while (sp_good == 0) {

      # Randomly select species prevalence value
      sp <- sample(sp_dist, 1)

      # Convert species distribution to presence-absence
      set.seed(NULL)
      pres_abs <-
        convertToPA(spec_dist,
                    PA.method = "threshold",
                    prob.method = "linear",
                    species.prevalence = sp,
                    plot = TRUE)

      if (nrow(pres_abs$pa.raster[pres_abs$pa.raster > 0]) >= 1) {sp_good <- 1}

    }

    # # Create distribution for species prevalence
    # sp_dist <- runif(1000, min = 0.0001, max = 0.2)
    #
    # set.seed(NULL)
    # # Randomly select species prevalence value
    # sp <- sample(sp_dist, 1)
    #
    # # Convert species distribution to presence-absence
    # set.seed(983)
    # pres_abs <-
    #   convertToPA(spec_dist,
    #               PA.method = "threshold",
    #               prob.method = "linear",
    #               species.prevalence = sp,
    #               plot = TRUE)

    assign(paste0("pres_abs_", i), pres_abs)



    # Check whether the species actually occurs anywhere
    set.seed(NULL)
    possibleError <- tryCatch(presence_points <- sampleOccurrences(pres_abs,
                                                                   n = 1000,
                                                                   plot = TRUE,
                                                                   sampling.area = "Europe",
                                                                   replacement = TRUE),
                              error = function(e) e)

    if(inherits(possibleError, "error")) {

      print(paste0("Species ", counter, " is invalid. Skipping."))
      next

    }

    max_nsamp <- nrow(spec_dist$suitab.raster[spec_dist$suitab.raster >
                                                spec_dist$suitab.raster@pnt[["range_min"]]+0.05])

    nsamp_dist <- round(runif(100, min = 1, max = max_nsamp))

    set.seed(NULL)
    nsamp <- sample(nsamp_dist, 1)

    # Sample occurrences from the presence-absence distribution
    set.seed(NULL)
    presence_points <- sampleOccurrences(pres_abs,
                                         n = nsamp,
                                         plot = TRUE,
                                         sampling.area = "Europe",
                                         replacement = TRUE,
                                         type = "presence only")

    print(paste0("Completed species ", counter))

    assign(paste0("presence_points_", i), presence_points)

    Presence <- presence_points$sample.points[, c( "x", "y",  "Observed")]

    # Presence <- Presence[Presence$Observed == 1,]

    #Presence <- slice_sample(Presence, n = nsamp)

    assign(paste0("Presence_", i), Presence)

    species_data[[counter]] <- Presence

    if(!file.exists(paste("F:/VSDB2/",
                          "Virtual_species_my_",
                          i+j,
                          ".Rdata",
                          sep = ""))) {

      saveRDS(pres_abs, file = paste("F:/VSDB2/",
                                     "Virtual_species_my_",
                                     i+j,
                                     ".Rdata",
                                     sep = ""))

    } else {

      overwrite <- readline(prompt=paste("File ",
                                         "F:/VSDB2/",
                                         "Virtual_species_my_",
                                         i+j,
                                         ".Rdata",
                                         " exists. Overwrite? (y/n)",
                                         sep = ""))

      if (overwrite == "y") {
        saveRDS(pres_abs, file = paste("F:/VSDB2/",
                                       "Virtual_species_my_",
                                       i+j,
                                       ".Rdata",
                                       sep = ""))
      } else if (overwrite == "n") {

        k <- readline(prompt=paste("Enter a new integer to change filename iteration ",
                                   "or type 'x' to cancel."))

        if (k == "x") {

          stop("Cancelling...")

        } else {

          possibleError <- tryCatch(k == round(as.numeric(k)),
                                    error = function(e) e)

          if(inherits(possibleError, "error")) {

            stop("Sorry, did not understand input.")

          }

          k <- as.numeric(k)

          j <- k - i

          saveRDS(pres_abs, file = paste("F:/VSDB2/",
                                         "Virtual_species_my_",
                                         i+j,
                                         ".Rdata",
                                         sep = ""))

        }

      } else {

        stop("Sorry, did not understand input.")

      }

    }

    i <- i + 1

    counter <- counter + 1

  }

  return(species_data)

}


createvirtualspecies <- function(clim_data, n) {

  mandatory_vars <- raster::stack(clim_data$bio1, clim_data$bio12)

  optional_vars <- raster::stack(clim_data@layers[!(clim_data@layers %in% mandatory_vars@layers)])

  random_vars <- sample(optional_vars@layers,
                        sample(1:3, 1),
                        replace = FALSE)

  wc <- raster::stack(c(mandatory_vars, random_vars))

  vs_list <- list()

  for (i in 1:n) {

    vs_list[[i]] <- generateRandomSp(wc, approach = "random", niche.breadth = "narrow")

    saveRDS(vs_list[[1]], file = paste("F:/VSDB/",
                                       "virtual_species_",
                                       i,
                                       ".RData",
                                       sep = "")
    )

  }

  return(vs_list)

}

# Function to create cube ----
createvirtualcube <- function(vslist) {

  # create necessary columns
  for (i in 1:length(vslist)) {
    names(vslist[[i]]) <- c("x", "y", "obs")
    vslist[[i]]$taxonKey <- rep(i, nrow(vslist[[i]]))
    vslist[[i]]$scientificName <- rep(paste0("Species ", i), nrow(vslist[[i]]))
    vslist[[i]]$rank <- rep("Species", nrow(vslist[[i]]))
    vslist[[i]]$kingdom <- rep("Animalia", nrow(vslist[[i]]))
    vslist[[i]]$year <- rep("2020", nrow(vslist[[i]]))
    vslist[[i]]$eea_cell_code <- rep("NA", nrow(vslist[[i]]))
    vslist[[i]]$resolution <- rep("1km", nrow(vslist[[i]]))
  }

  df <- do.call(rbind, vslist)

  # Set projection and then transform to EPSG:3035
  coordinates(df) <- ~x+y
  proj4string(df) <- CRS("+init=epsg:4326")
  df <- spTransform(df, CRS("+init=epsg:3035"))
  colnames(df@coords) <- c("xcoord", "ycoord")
  df@data <-
    df@data %>%
    dplyr::mutate(xcoord = df@coords[, "xcoord"]/1000,
                  ycoord = df@coords[, "ycoord"]/1000)

  # Create grid cells
  df@data <-
    df@data %>%
    dplyr::mutate(eea_cell_code = paste0(
      resolution,
      "E", floor(xcoord),
      "N", floor(ycoord))) %>%
    dplyr::select(xcoord, ycoord, eea_cell_code, obs, taxonKey, scientificName, rank, kingdom, year, resolution)

  # Aggregate occurrences in grid cells
  df <-
    df@data %>%
    dplyr::mutate(n = sum(obs), .by = c(year, taxonKey, eea_cell_code), .keep = "all") %>%
    dplyr::distinct(year, taxonKey, eea_cell_code, .keep_all = TRUE) %>%
    dplyr::select(-obs) %>%
    dplyr::rename(obs = n)

  return(df)

}
