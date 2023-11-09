plot_ts <- function(coverage_df) {

  # Get start and end years
  first_year <- head(coverage_df$year, n=1)
  final_year <- tail(coverage_df$year, n=1)

  # Determine legend label
  diversity_types <- c("total_records",
                      "rarefied",
                      "0",
                      "1",
                      "2",
                      "obs_richness",
                      "evenness",
                      "total_obs",
                      "cum_richness")

  names(diversity_types) <- c("Adjusted Species Richness",
                             "Rarefied Species Richness",
                             "Estimated Species Richness",
                             "Shannon Diversity",
                             "Simpson Diversity",
                             "Observed Species Richness",
                             "Evenness",
                             "Total Observations",
                             "Cumulative Species Richness")

  div_type <- dplyr::first(coverage_df$diversity_type, na_rm = TRUE)

  y_label <- names(diversity_type)[diversity_type %in%
                                     div_type]

  # # Suppress y axis values except for certain diversity types
  # suppress_y <- if (div_type=="obs_richness" |
  #                   div_type=="evenness" |
  #                   div_type=="total_records")
  #   {
  #   "n"
  # } else {
  #   "y"
  # }
  suppress_y <- "n"

  if ("type" %in% colnames(coverage_df)) {

    # Plot trend
    trend_plot <-
      ggplot(coverage_df, aes(x = year,
                              y = diversity_val)) +
      geom_line(aes(color = type),
                size = 0.8) +
      # geom_smooth(aes(color = type),
      #             size = 1,
      #             linetype = "dashed",
      #             method = "loess",
      #             formula = "y ~ x") +
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

  } else {

    # Plot adjusted cube richness trend
  trend_plot <-
      ggplot(coverage_df, aes(x = year,
                              y = diversity_val)) +
      geom_line(color = "lightblue",
                size = 0.8) +
      geom_smooth(color = "red",
                  size = 1,
                  linetype = "dashed",
                  method = "loess",
                  formula = "y ~ x") +
      scale_x_continuous(breaks = pretty_breaks(n=10)) +
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

  }

  trend_plot

}
