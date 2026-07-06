
devtools::load_all(".")
library(dplyr)

# Create a mock cube with many species
n_species <- 500
n_years <- 5
n_cells_per_species <- 10

mock_data <- expand.grid(
  year = 2001:(2000+n_years),
  taxonKey = 1:n_species,
  cellid = 1:n_cells_per_species
) %>%
  mutate(
    scientificName = paste("Species", taxonKey),
    obs = 1,
    cellCode = paste0("C", cellid)
  )

indicator <- list(
  data = expand.grid(year = 2001:(2000+n_years), taxonKey = 1:n_species) %>%
         mutate(scientificName = paste("Species", taxonKey), diversity_val = 5),
  raw_data = mock_data,
  div_type = "spec_range"
)
class(indicator) <- "indicator_ts"

message("Adding CIs (5 bootstraps) for ", n_species * n_years, " groups...")
start_time <- Sys.time()
res_ci <- add_ci(indicator, num_bootstrap = 5) 
end_time <- Sys.time()
message("Time taken for 5 bootstraps: ", end_time - start_time)
