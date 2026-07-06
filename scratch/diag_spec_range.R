
# Test with full 1980+ dataset
devtools::load_all(".")
library(dplyr)

cube <- process_cube(
  system.file("extdata", "denmark_mammals_cube_eqdgc.csv", package = "b3gbi"),
  first_year = 1980
)
message("Cube: ", nrow(cube$data), " rows, ", 
        n_distinct(cube$data$taxonKey), " species, ",
        n_distinct(cube$data$year), " years")

res <- spec_range_ts(cube)
message("Indicator: ", nrow(res$data), " year×species groups")

# Test with 100 bootstraps (what the user tried)
message("\n=== Running add_ci with 100 bootstraps on full data ===")
start <- Sys.time()
res_ci <- add_ci(res, num_bootstrap = 100)
elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
message("Time: ", round(elapsed, 1), " secs")
message("Rows with non-NA CIs: ", sum(!is.na(res_ci$data$ll)), " / ", nrow(res_ci$data))
print(head(res_ci$data))

# Also test spec_occ_ts
message("\n=== Testing spec_occ_ts with 100 bootstraps ===")
res_occ <- spec_occ_ts(cube)
start <- Sys.time()
res_occ_ci <- add_ci(res_occ, num_bootstrap = 100)
elapsed2 <- as.numeric(difftime(Sys.time(), start, units = "secs"))
message("Time: ", round(elapsed2, 1), " secs")
message("Rows with non-NA CIs: ", sum(!is.na(res_occ_ci$data$ll)), " / ", nrow(res_occ_ci$data))
