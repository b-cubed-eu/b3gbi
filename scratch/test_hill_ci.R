# Test hill0_ts + add_ci
devtools::load_all(".")
library(dplyr)

cube <- process_cube(
  system.file("extdata", "denmark_mammals_cube_eqdgc.csv", package = "b3gbi"),
  first_year = 2020
)

# iNEXT calculates CIs via nboot natively here
res_hill <- hill0_ts(cube, nboot = 5)

message("\n=== Running add_ci on hill indicator ===")
start <- Sys.time()
res_ci <- add_ci(res_hill, num_bootstrap = 100)
elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
message("Time: ", round(elapsed, 2), " secs")
message("Did it return correctly?: ", inherits(res_ci, "indicator_ts"))
