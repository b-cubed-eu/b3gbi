#' @noRd
multiyear_bootstrap <- function(tax_list, indicator, num_bootstrap = 1000) {

  set.seed(432)
  turnover_list <- vector("list", length(tax_list) - 1)

  # iterate over years
  for (i in 2:length(tax_list)) {

    # determine which year has fewer values and label it as such
    shorter <- ifelse(length(tax_list[[i-1]]) < length(tax_list[[i]]),
                      1, 2)

    if (shorter==1) {

      shortyear <- tax_list[[i-1]]
      longyear <- tax_list[[i]]

    } else {

      shortyear <- tax_list[[i]]
      longyear <- tax_list[[i-1]]

    }


    boot_results <- numeric(num_bootstrap)
    current_year_data <- list()

    # iterate over bootstraps
    for (j in 1:num_bootstrap) {

      # determine shared species between the two years
      overlap <- intersect(longyear, shortyear)

      # match the order of species in longyear to the shared species vector
      longyear <- longyear[order(match(longyear, overlap))]

      # match the order in shortyear to shared species vector
      shortyear <- shortyear[order(match(shortyear, overlap))]

      # sample indices from longyear
      longyearindexsamp <- resample(seq_len(length(longyear)),
                                    length(longyear),
                                    replace = TRUE)

      # get the taxon keys corresponding to the index values
      longyearsamp <- longyear[longyearindexsamp]

      # sample the index values from short year to match those in long year
      shortyearindexsamp <- longyearindexsamp[1:length(shortyear)]

      # get the taxon keys corresponding to the index values
      shortyearsamp <- shortyear[shortyearindexsamp]

      # rename the year samples to reflect the appropriate order
      if (shorter==1){

        year1samp <- shortyearsamp
        year2samp <- longyearsamp

      } else {

        year1samp <- longyearsamp
        year2samp <- shortyearsamp

      }

      # Calculate turnover
      boot_results[j] <- multiyear_boot_statistic(year2samp,
                                                  year1samp,
                                                  seq_along(year2samp),
                                                  seq_along(year1samp))
      # collect the data from the present year to feed to boot.return
      current_year_data[[j]] <- year2samp

    }

    # convert current_year_data from a list to a data frame
    current_year_data <- do.call(rbind, current_year_data)

    # needed for boot.return function
    strata <- rep(1, length(boot_results))

    # needed for boot.return function
    weights <- rep(1 / length(boot_results),
                   length(boot_results))

    # use boot.return to format data appropriately for calculating CIs
    boot_results_formatted <- boot.return_int(sim = "ordinary",
                                              t0 = indicator$diversity_val[[i]],
                                              t = as.matrix(boot_results),
                                              strata = strata,
                                              R = num_bootstrap,
                                              data = current_year_data[[i]],
                                              stat = multiyear_boot_statistic,
                                              stype = "i",
                                              call = NULL,
                                              seed = 432,
                                              L = NULL,
                                              m = NULL,
                                              pred.i = NULL,
                                              weights = weights,
                                              ran.gen = function(d, p) d,
                                              mle = NULL)

    turnover_list[[i - 1]] <- boot_results_formatted
  }

  return(turnover_list)

}
