plot_ts <- function(coverage_df) {

  # Get start and end years
  first_year <- head(coverage_df$year, n=1)
  final_year <- tail(coverage_df$year, n=1)

  # Determine legend label
  diversity_type <- c("total_records",
                      "rarefied",
                      "0",
                      "1",
                      "2",
                      "obs_richness",
                      "evenness")

  names(diversity_type) <- c("Adjusted Species Richness",
                             "Rarefied Species Richness",
                             "Estimated Species Richness",
                             "Shannon Diversity",
                             "Simpson Diversity",
                             "Observed Species Richness",
                             "Evenness")

  y_label <- names(diversity_type)[diversity_type %in%
                                     coverage_df$diversity_type[1]]

  # Suppress y axis values except for certain diversity types
  suppress_y <- if (coverage_df$diversity_type[1]=="obs_richness" |
                    coverage_df$diversity_type[1]=="evenness" |
                    coverage_df$diversity_type[1]=="total_records")
    {
    "n"
  } else {
    "y"
  }

    # Plot adjusted cube richness trend
  adj_richness_ts_plot <-
      ggplot(coverage_df, aes(x = year,
                              y = diversity_val)) +
      geom_line(color = "lightblue",
                size = 0.8) +
      geom_smooth(color = "red",
                  size = 1,
                  linetype = "dashed",
                  method = "loess",
                  formula = "y ~ x") +
      scale_x_continuous(breaks = seq(first_year,
                                      final_year,
                                      by = 5)) +
      labs(x = "Year", y = y_label,
           title = paste(y_label,
                         " Trend",
                         " (",
                         first_year,
                         "-",
                         final_year,
                         ")",
                         sep="")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14),
          axis.text.y = if (suppress_y=="y") {
            element_blank()
          } else {
            element_text()
          }
    )

  adj_richness_ts_plot

}
