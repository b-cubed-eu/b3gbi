# Diagnostic script to trace bootstrap failure

library(b3gbi)
library(dubicube)

# Load test data
data(example_cube_1)

# Calculate indicator
res <- total_occ_ts(example_cube_1)

# Check raw_data structure
cat("=== Raw data structure ===\n")
print(str(res$raw_data))
cat("\nRaw data classes:", class(res$raw_data), "\n")
cat("Has 'total_occ' class:", "total_occ" %in% class(res$raw_data), "\n")

# Call prepare_indicator_bootstrap
cat("\n=== Calling prepare_indicator_bootstrap ===\n")
params <- prepare_indicator_bootstrap(
  indicator = res,
  num_bootstrap = 10,
  ci_type = "norm"
)

cat("Bootstrap params:\n")
print(str(params$bootstrap_params))

cat("\nData cube in bootstrap params:\n")
print(str(params$bootstrap_params$data_cube))
cat("Classes:", class(params$bootstrap_params$data_cube), "\n")

# Try calling bootstrap_cube directly
cat("\n=== Calling bootstrap_cube ===\n")
tryCatch({
  boot_res <- do.call(dubicube::bootstrap_cube, params$bootstrap_params)
  cat("Bootstrap succeeded! Rows:", nrow(boot_res), "\n")
  print(head(boot_res))
}, error = function(e) {
  cat("Bootstrap error:\n")
  print(e)
})
