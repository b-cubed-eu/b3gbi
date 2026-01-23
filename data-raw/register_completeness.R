# Register 'completeness' indicator
# Run this script to add the indicator to available_indicators.rda

# Use absolute path for devtools::load_all to avoid path issues
package_root <- "e:/R_projects/b3gbi"
devtools::load_all(package_root)

b3gbi:::register_indicator(
  indicator_class = "completeness",
  indicator_name = "Completeness (Sample Coverage)",
  plot_title = "Data Completeness",
  legend_label = "Coverage",
  map_function_arguments = list(
    cutoff_length = 0,
    data_type = "abundance",
    assume_freq = FALSE
  ),
  ts_function_arguments = list(
    cutoff_length = 0
  ),
  overwrite = TRUE
)

message("Indicator 'completeness' registered successfully.")
