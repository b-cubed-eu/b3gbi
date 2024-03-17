# Create data frame for registering indicators
available_indicators <- data.frame(
  indicator_class = NA,
  indicator_name = NA,
  plot_title = NA,
  legend_label = NA,
  legend_transformation = NA
)

# Save it as .rda file in data directory of the package
usethis::use_data(available_indicators, overwrite = TRUE)

# Register indicators
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")
register_indicator("obs_richness", "Observed Species Richness", "Observed Richness", "Observed Richness")



# Function to load the .rda file into R
load_indicators <- function() {
  data(available_indicators, package = "b3gbi")
}

# Function to register indicators
register_indicator <- function(indicator_class,
                               indicator_name,
                               plot_title = NULL,
                               legend_label = NULL,
                               legend_transformation = NULL,
                               overwrite = FALSE) {

  if (is.null(plot_title)) {
    plot_title <- indicator_name
  }

  if (is.null(legend_label)) {
    legend_label <- indicator_name
  }

  if (is.null(legend_transformation)) {
    legend_transformation <- NA
  }

  #Path to the data file
  fname <- system.file("data", "available_indicators.rda", package = "b3gbi")
  stopifnot(file.exists(fname))

  #Load data into new environment
  e <- new.env()
  load(fname, envir = e)

  if (!indicator_class %in% e$available_indicators$indicator_class) {

   #Add a new row to register the indicator
  e$available_indicators <-
    e$available_indicators %>%
    add_row(indicator_class = indicator_class,
            indicator_name = indicator_name,
            plot_title = plot_title,
            legend_label = legend_label,
            legend_transformation = legend_transformation)

  } else {
    if (!overwrite = FALSE) {
      stop("Indicator class already registered. Use overwrite=TRUE to replace it.")
    } else {
      # Remove any indicators without a class and/or name
      e$available_indicators <-
        e$available_indicators %>%
        replace()
    }
  }

  # Remove any indicators without a class and/or name
  e$available_indicators <-
    e$available_indicators %>%
    filter(!is.na(indicator_class) & !is.na(indicator_name))

  print(paste("Indicator class '", indicator_class, "' registered."))

  #Write back to file
  save(available_indicators, file = fname, envir = e)

}

# Function to remove a registered indicator
deregister_indicator <- function(indicator_class=NULL,
                               indicator_name=NULL) {

  if (is.null(indicator_class) & is.null(indicator_name)) {
    stop("Please provide either the indicator_class or indicator_name.")
  }

  #Path to the data file
  fname <- system.file("data", "available_indicators.rda", package = "b3gbi")
  stopifnot(file.exists(fname))

  #Load data into new environment
  e <- new.env()
  load(fname, envir = e)

  if(!is.null(indicator_class)) {
    class <- indicator_class
    # Remove any indicators without a class and/or name
    e$available_indicators <-
      e$available_indicators %>%
      filter(!(indicator_class == class))
    print(paste("Indicator class '", indicator_class, "' deregistered."))
  } else {
    name <- indicator_name
    # Remove any indicators without a class and/or name
    e$available_indicators <-
      e$available_indicators %>%
      filter(!(indicator_name == name))
    print(paste("Indicator ", indicator_name, "deregistered."))
  }

  #Write back to file
  save(available_indicators, file = fname, envir = e)

}
