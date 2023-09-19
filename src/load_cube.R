# -----------------------------------------------------
# Calculate species richness from GBIF occurrence cubes
# -----------------------------------------------------

# Load required packages
library(tidyverse)
library(vegan)
library(minpack.lm)

# Read in the csv files
occurrence_data <- read_csv(file = "data/raw/eu_modellingtaxa_cube.csv",
                            col_types = cols(
                              year = col_double(),
                              eea_cell_code = col_character(),
                              taxonKey = col_double(),
                              n = col_double(),
                              min_coord_uncertainty = col_double()
                            ),
                            na = ""
)

taxonomic_info <- read_csv("data/raw/eu_modellingtaxa_info.csv",
                           col_types = cols(
                             taxonKey = col_double(),
                             scientificName = col_character(),
                             rank = col_factor(),
                             taxonomicStatus = col_factor(),
                             kingdom = col_factor(),
                             includes = col_character()
                           ),
                           na = ""
)

# Merge the two data frames together on 'taxonKey'
merged_data <- left_join(occurrence_data, taxonomic_info, by = "taxonKey")

# Separate 'eea_cell_code' into resolution, coordinates
merged_data <- merged_data %>%
  mutate(
    xcoord = as.numeric(str_extract(eea_cell_code, "(?<=E)\\d+")),
    ycoord = as.numeric(str_extract(eea_cell_code, "(?<=N)\\d+")),
    resolution = str_replace_all(eea_cell_code, "(E\\d+)|(N\\d+)", "")
  )

# Remove columns that are not needed
merged_data <-
  merged_data %>%
  select(-min_coord_uncertainty)

merged_data <-
  merged_data %>%
  select(-taxonomicStatus, -includes)

# Rename column n to obs
merged_data <-
  merged_data %>%
  rename(obs = n)

# Set start year and final year for analysis
first_year <- 1950
final_year <- max(merged_data$year)-1

# Limit data set
merged_data <-
  merged_data %>%
  filter(year >= first_year) %>%
  filter(year <= final_year)

# Calculate global species richness
global_species_richness <-
  merged_data %>%
  group_by(year) %>%
  summarise(species_richness = n_distinct(scientificName), .groups = "drop")

# Calculate species richness by site
site_species_richness <-
  merged_data %>%
  group_by(eea_cell_code, year) %>%
  summarise(species_richness = n_distinct(scientificName), .groups = "drop")

# Calculate species richness by kingdom
kingdom_species_richness <-
  merged_data %>%
  group_by(year, kingdom, eea_cell_code) %>%
  summarise(species_richness = n_distinct(scientificName), .groups = "drop") %>%
  pivot_wider(names_from = kingdom, values_from = species_richness)

# Create a grid of eea_cell_code and year combinations for site-based richness
complete_grid <- expand_grid(
  eea_cell_code = unique(site_species_richness$eea_cell_code),
  year = unique(site_species_richness$year)
  )

# Left join the complete grid with the site-based richness data
site_species_richness_filled <-
  complete_grid %>%
  left_join(site_species_richness, by = c("eea_cell_code", "year")) %>%
  arrange(eea_cell_code, year) %>%
  replace_na(list(species_richness = 0))

glob_richness_plot <-
  ggplot(global_species_richness, aes(x = year, y = species_richness)) +
  geom_line(color = "lightblue", size = 0.5) +
  geom_smooth(color = "red", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(global_species_richness$year),
                                  max(global_species_richness$year),
                                  by = 5)) +
  labs(x = "Year", y = "Species Richness",
       title = paste("Time Series of Global Species Richness (",
                     min(global_species_richness$year),
                     "-",
                     max(global_species_richness$year),
                     ")",
                     sep="")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14))

# --------------------------------------------------------------------------

# Create a community matrix with years as rows and species as columns
community_matrix <- table(merged_data$year, merged_data$scientificName)

# Calculate species accumulation curve
sac <- specaccum(community_matrix, method = "random")

# Extract observed counts
observed_counts <- sac$richness

# Create a data frame with the number of samples and the observed counts
sac_df <- data.frame(samples = 1:length(observed_counts), species = observed_counts)

# Fit a non-linear model to the species accumulation curve
model <- nlsLM(species ~ SSasymp(samples, Asym, R0, lrc), data = sac_df, start = list(Asym = max(sac_df$species), R0 = sac_df$species[1], lrc = 0))


# Extract the asymptote from the model
asymptote <- coef(model)["Asym"]

# Calculate the proportion of the asymptote reached each year
proportion_asymptote <- rowSums(community_matrix) / asymptote

# Adjust species richness
adjusted_richness <- rowSums(community_matrix) * proportion_asymptote

# Convert the adjusted richness into a data frame
adjusted_richness_df <- data.frame(year = names(adjusted_richness), species_richness = adjusted_richness)

# Calculate species richness globally
global_species_richness <- merged_data %>%
  group_by(year) %>%
  summarise(species_richness = n_distinct(scientificName), .groups = "drop")

# Calculate species richness by country
# Assuming that x_coord and y_coord can be used to determine the country
# You might need to replace this with your own logic to map coordinates to countries
country_species_richness <- merged_data %>%
  group_by(year, x_coord, y_coord) %>%
  summarise(species_richness = n_distinct(scientificName))

# Calculate species richness by kingdom
# Calculate species richness by kingdom
kingdom_species_richness <- merged_data %>%
  group_by(year, kingdom) %>%
  summarise(species_richness = n_distinct(scientificName), .groups = "drop") %>%
  pivot_wider(names_from = kingdom, values_from = species_richness)
