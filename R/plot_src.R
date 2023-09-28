# Plot unadjusted cube richness trend
cube_richness_plot <-
  ggplot(cube_species_richness, aes(x = year, y = species_richness)) +
  geom_line(color = "lightblue", size = 0.5) +
  geom_smooth(color = "red", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(first_year,
                                  final_year,
                                  by = 5)) +
  labs(x = "Year", y = "Species Richness",
       title = paste("Time Series of Species Richness (",
                     first_year,
                     "-",
                     final_year,
                     ")",
                     sep="")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14))

# Plot adjusted cube richness trend
cube_richness_adj_plot <-
  ggplot(cube_species_richness, aes(x = year, y = spec_rich_adj)) +
  geom_line(color = "lightblue", size = 0.5) +
  geom_smooth(color = "red", size = 1, linetype = "dashed") +
  scale_x_continuous(breaks = seq(first_year,
                                  final_year,
                                  by = 5)) +
  labs(x = "Year", y = "Species Richness",
       title = paste("Time Series of Species Richness (",
                     first_year,
                     "-",
                     final_year,
                     ")",
                     sep="")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(size = 14))
