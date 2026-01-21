# Script to register the new indicator 'species richness density'
# Run this script to update the package's internal indicator registry

# Ensure the package is loaded/available
devtools::load_all("e:/R_projects/b3gbi")

b3gbi:::register_indicator(
  indicator_class = "spec_richness_density",
  indicator_name = "Species Richness Density",
  plot_title = "Species Richness Density",
  legend_label = "Unique Species / km^2",
  overwrite = TRUE
)
