# Function to load the .rda file into R
#' @noRd
load_registered_indicators <- function() {
  b3gbi::available_indicators
}

# Function to backup the available_indicators.rda file as .rda.old
#' @noRd
backup_registered_indicators <- function(suppress_prompt="FALSE") {

  available_indicators <- NULL; rm(available_indicators)

    #Path to backup file
    backup_fname <- system.file("data", "available_indicators.rda.old", package = "b3gbi")


  if (suppress_prompt=="TRUE" | !file.exists(backup_fname)) {

    check_if_certain <- "yes"

  } else {

    #Double-check if user wants to overwrite existing backup
    check_if_certain <- readline(prompt = paste("Warning: This will overwrite the existing file ", backup_fname, ". Are you certain? (yes/no) ", sep=""))

    check_if_certain <- match.arg(check_if_certain, choices = c("yes", "no"))

  }

  if (check_if_certain!="yes") {

    stop("Backup cancelled.")

  } else {

  #Path to the data file
  fname <- system.file("data", "available_indicators.rda", package = "b3gbi")
  stopifnot(file.exists(fname))

  #Load data into new environment
  e <- new.env()
  load(fname, envir = e)

  #Backup .rda file
  save(available_indicators, file = paste(backup_fname, sep=""))

  message(paste("Registered indicators backed up to ", backup_fname, sep=""))

  }
}

# Function to empty indicator registry and start from scratch
#' @noRd
reset_indicator_registry <- function(){

  available_indicators <- NULL; rm(available_indicators)

  #Path to the data file
  fname <- system.file("data", "available_indicators.rda", package = "b3gbi")
  stopifnot(file.exists(fname))

  #Load data into new environment
  e <- new.env()
  load(fname, envir = e)

  e$available_indicators <- list()

  #Write back to file
  save(available_indicators, file = fname, envir = e)

}

# Function to register indicators
#' @noRd
register_indicator <- function(indicator_class,
                               indicator_name,
                               # map_wrapper = NULL,
                               # ts_wrapper = NULL,
                               plot_title = NULL,
                               legend_label = NULL,
                               legend_transformation = NULL,
                               map_function_arguments = NULL,
                               ts_function_arguments = NULL,
                               overwrite = FALSE,
                               backup = TRUE) {

  available_indicators <- NULL; rm(available_indicators)

  map_wrapper_exists <- find(paste0(indicator_class, "_map"))

  if(length(map_wrapper_exists)==0) {
    map_wrapper <- NULL
  } else {
    map_wrapper <- paste0(indicator_class, "_map")
  }

  ts_wrapper_exists <- find(paste0(indicator_class, "_ts"))

  if(length(ts_wrapper_exists)==0) {
    ts_wrapper <- NULL
  } else {
    ts_wrapper <- paste0(indicator_class, "_ts")
  }

  if (is.null(map_wrapper) & is.null(ts_wrapper)) {

    stop(paste0("Could not find functions ",
                indicator_class, "_map or ",
                indicator_class, "_ts in package namespace.
               Before registering an indicator, you must include at least one exported and documented wrapper function for user access.
                              Check that they are correctly named and that you have run devtools::document()."))

  }

    if (is.null(plot_title)) {
      plot_title <- indicator_name
    }

    if (is.null(legend_label)) {
      legend_label <- indicator_name
    }

    if (is.null(legend_transformation)) {
      legend_transformation <- NULL
    }

    if(is.null(map_function_arguments)) {
      map_function_arguments <- NULL
    } else {
      stopifnot_error("map_function_arguments must be a list", inherits(map_function_arguments, "list"))
    }

    if(is.null(ts_function_arguments)) {
      ts_function_arguments <- NULL
    } else {
      stopifnot_error("ts_function_arguments must be a list", inherits(ts_function_arguments, "list"))
    }

  #Path to the data file
  fname <- system.file("data", "available_indicators.rda", package = "b3gbi")
  stopifnot(file.exists(fname))

  #Load data into new environment
  e <- new.env()
  load(fname, envir = e)

  if (backup==TRUE) {

    #Backup .rda file
    save(available_indicators, file = paste(fname, ".old", sep=""))

  }

  new_class <- list(indicator_class = indicator_class,
                    indicator_name = indicator_name,
                    plot_title = plot_title,
                    legend_label = legend_label,
                    legend_transformation = legend_transformation,
                    map_wrapper = map_wrapper,
                    ts_wrapper = ts_wrapper,
                    map_function_arguments = map_function_arguments,
                    ts_function_arguments = ts_function_arguments)

  if (!indicator_class %in% names(e$available_indicators)) {

    #Add new list item to register the indicator
    e$available_indicators <- c(e$available_indicators, list(new_class))

    names(e$available_indicators)[[length(e$available_indicators)]] <- indicator_class

  } else {

    if (overwrite == FALSE) {

      stop("Indicator class already registered. Use overwrite=TRUE to replace it.")

    } else {

      # Get the number for the existing indicator list item
      indicator_number <- which(names(e$available_indicators) %in% indicator_class)

      # Replace it with the new list item
      e$available_indicators[indicator_number] <- list(new_class)

    }
  }

  message(paste0("Indicator class '", indicator_class, "' registered."))

  # Ensure object has the proper class
  if (!inherits(e$available_indicators, "available_indicators")) {
    class(e$available_indicators) <- append("available_indicators", class(e$available_indicators))
  }

  #Write back to file
  save(available_indicators, file = fname, envir = e)

  #Load into global environment if desired
  check_if_apply_now <- readline(prompt = paste0("Load changes into global environment immediately? (yes/no) "))

  check_if_apply_now <- match.arg(check_if_apply_now, choices = c("yes", "no"))

  if (check_if_apply_now=="yes") {

    load_registered_indicators()

    message("Changes applied to global environment.")

  } else {

    message("Changes saved but not applied. Use load_registered_indicators() to apply.")

  }

}

# Function to remove a registered indicator
#' @noRd
deregister_indicator <- function(indicator_number,
                                 #indicator_class=NULL,
                                 #indicator_name=NULL,
                                 backup = TRUE) {

  available_indicators <- NULL; rm(available_indicators)

  # Path to the data file
  fname <- system.file("data", "available_indicators.rda", package = "b3gbi")
  stopifnot(file.exists(fname))

  # Load data into new environment
  e <- new.env()
  load(fname, envir = e)

  # Get class of indicator
  i_class <- e$available_indicators[[indicator_number]]$indicator_class

  check_if_certain <- readline(prompt = paste("Are you sure you want to deregister class ",
                                              i_class,
                                              "? (yes/no) "))

  check_if_certain <- match.arg(check_if_certain, choices = c("yes", "no"))

  if (check_if_certain!="yes") {

    stop("Registration cancelled.")

  }

  if (backup==TRUE) {

    #Backup .rda file
    save(available_indicators, file = paste(fname, ".old", sep=""))

  }

  # Remove indicator from registry
  e$available_indicators <- e$available_indicators[-indicator_number]
  message(paste("Indicator class '", i_class, "' deregistered."))

  # Ensure object has the proper class
  if (!inherits(e$available_indicators, "available_indicators")) {
    class(e$available_indicators) <- append("available_indicators", class(e$available_indicators))
  }

  #Write back to file
  save(available_indicators, file = fname, envir = e)

  #Load into global environment if desired
  check_if_apply_now <- readline(prompt = paste0("Load changes into global environment immediately? (yes/no) "))

  check_if_apply_now <- match.arg(check_if_apply_now, choices = c("yes", "no"))

  if (check_if_apply_now=="yes") {
    load_registered_indicators()
    message("Changes applied to global environment.")
  } else {
    message("Changes saved but not applied. Use load_registered_indicators() to apply.")
  }

  message(paste("Registered indicators backed up to ", fname, ".old", sep=""))
}


