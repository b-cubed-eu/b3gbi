plot_ts <- function(coverage_df, type = "obs") {

  if (type == "obs") {

    # Plot unadjusted cube richness trend
    obs_richness_ts_plot <-
      ggplot(coverage_df, aes(x = year, y = obs_richness)) +
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
    obs_richness_ts_plot

  } else if (type == "adj") {

    # Plot adjusted cube richness trend
    adj_richness_ts_plot <-
      ggplot(coverage_df, aes(x = year, y = est_richness_index)) +
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
    adj_richness_ts_plot

  } else {

    print("Please use 'obs' or 'adj' for type.")

  }

}
