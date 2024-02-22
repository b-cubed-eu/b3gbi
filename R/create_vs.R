# Function to create virtual species
createvs_custom <- function(clim_data,
                            n,
                            start_val=1,
                            samp_scale = 5000000,
                            samp_shape = 0.5,
                            dir = "F:/VSDB_HR/",
                            show_plot = TRUE,
                            show_occ_plot = TRUE) {

  species_data <- list()

  layernames <- names(clim_data)

  if (inherits(clim_data, "RasterBrick")) {
    min <- clim_data@data@min
    max <- clim_data@data@max
  } else if (inherits(clim_data, "SpatRaster")) {
    min <- clim_data@pnt$range_min
    max <- clim_data@pnt$range_max

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

  i <- start_val
  j <- 0
  counter <- 1
  while (counter <= n) {

    tic("Species total")

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

    capture.output(params, file = paste(dir,
                                        "Params_",
                                        i,
                                        ".txt",
                                        sep = ""))

    tic("Habitat Suitability")
    # Generate virtual species distribution
    set.seed(NULL)
    spec_dist <-
      generateSpFromFun(clim_data[[c(names(rand_parameters))]],
                        parameters = rand_parameters,
                        plot = show_plot)

    toc()

    # Set maximum species prevalence to avoid a bug where it occurs everywhere despite having a narrow suitability
    max_sp <- nrow(spec_dist$suitab.raster[spec_dist$suitab.raster >
                                             spec_dist$suitab.raster@pnt[["range_min"]]+0.05]) /
      nrow(spec_dist$suitab.raster[spec_dist$suitab.raster > 0])

    print(paste0("Maximum species prevalence value: ", max_sp))

    # Create distribution for species prevalence
    sp_dist <- runif(1000, min = 0.0001, max = max_sp)

    sp_good <- 0
    while (sp_good == 0) {

      # Randomly select species prevalence value
      sp <- sample(sp_dist, 1)

      print(paste0("Selected species prevalence value: ", sp))

      # Convert species distribution to presence-absence
      set.seed(NULL)
      pres_abs <-
        convertToPA(spec_dist,
                    PA.method = "threshold",
                    prob.method = "linear",
                    species.prevalence = sp,
                    plot = show_plot)

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
    #               plot = show_plot)


    # Check whether the species actually occurs anywhere
    set.seed(NULL)
    possibleError <- tryCatch(
      presence_points <- sampleOccurrences_custom(pres_abs,
                                                  n = 1000,
                                                  plot = show_plot,
                                                  sampling.area = "Europe",
                                                  replacement = TRUE,
                                                  projection = "epsg:3035"),
      error = function(e) e)

    if(inherits(possibleError, "error")) {

      print(paste0("Species ", counter, " is invalid. Skipping."))
      next

    }

    max_nsamp <- nrow(spec_dist$suitab.raster[spec_dist$suitab.raster >
                                                spec_dist$suitab.raster@pnt[["range_min"]]+0.05])

    spec_prev <- as.numeric(pres_abs$PA.conversion[["species.prevalence"]])

    nsamp_dist <- round(rweibull(100, shape = samp_shape, scale = spec_prev * samp_scale))

    nsamp_dist <- nsamp_dist[(nsamp_dist < max_nsamp) & (nsamp_dist > 0)]

    set.seed(NULL)
    nsamp <- sample(nsamp_dist, 1)

    # Sample occurrences from the presence-absence distribution
    set.seed(NULL)
    presence_points <- sampleOccurrences_custom(pres_abs,
                                                n = nsamp,
                                                plot = show_occ_plot,
                                                sampling.area = "Europe",
                                                replacement = TRUE,
                                                type = "presence only",
                                                projection = "epsg:3035")

    print(paste0("Completed species ", counter, ". Sampled ", nrow(presence_points$sample.points), " points."))

    Presence <- presence_points$sample.points[, c( "x", "y",  "Observed")]

    species_data[[counter]] <- Presence

    if(!file.exists(paste(dir,
                          "Virtual_species_my_",
                          i+j,
                          ".Rdata",
                          sep = ""))) {

      saveRDS(pres_abs, file = paste(dir,
                                     "Virtual_species_pa_",
                                     i+j,
                                     ".Rdata",
                                     sep = ""))
      saveRDS(Presence, file = paste(dir,
                                     "Virtual_species_pp_",
                                     i+j,
                                     ".Rdata",
                                     sep = ""))

    } else {

      overwrite <- readline(prompt=paste("File ",
                                         dir,
                                         "Virtual_species_pa_",
                                         i+j,
                                         ".Rdata",
                                         " exists. Overwrite? (y/n)",
                                         sep = ""))

      if (overwrite == "y") {
        saveRDS(pres_abs, file = paste(dir,
                                       "Virtual_species_pa_",
                                       i+j,
                                       ".Rdata",
                                       sep = ""))
        saveRDS(Presence, file = paste(dir,
                                       "Virtual_species_pp_",
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

          saveRDS(pres_abs, file = paste(dir,
                                         "Virtual_species_pa_",
                                         i+j,
                                         ".Rdata",
                                         sep = ""))
          saveRDS(Presence, file = paste(dir,
                                         "Virtual_species_pp_",
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

    toc()

  }

  return(1)

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

