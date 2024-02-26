#' @noRd
retrieve_samples <- function(files_list, parallel = FALSE, num_cores = 4) {

  if (parallel == TRUE) {

    cl <- snow::makeCluster(num_cores) # create cluster for parallel processing

    doSNOW::registerDoSNOW(cl) # register SNOW with the foreach package

  }

  data <- list()

  cat("\nRetrieving", length(files_list), "samples.", "\n\n")

  point_sum <- 0

  tic("Total time")

  if (parallel == TRUE) {

  data <- foreach::foreach(i = 1:length(files_list)) %dopar% {  # loop for parallel processing

      readRDS(files_list[[i]])

    }

    stopCluster(cl) # stop the cluster

  } else {

    for(i in 1:length(files_list)) {

      data[[i]] <- readRDS(files_list[[i]])

      point_sum <- point_sum + nrow(data[[i]])

      cat("\rTotal data points retrieved:", point_sum, "    Samples retrieved:", i)

    }

  }

  toc() # display the time it took to read in all the files

  return(data)

}
