#' @export
plot.spec_range <- function(x, species, ...) {

  wrong_class(x, "spec_range", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  # Prepare a list of default arguments for call_plot
  plot_args <- list(
    x = x,
    species = species,
    y_label_default = "Cells Occupied",
    auto_title_ts = "Species Range Size",
    auto_title_map = "Species Range",
    ...
  )

  # Conditionally add the suppress_legend argument
  if (inherits(x, "indicator_map")) {
    plot_args$suppress_legend <- TRUE
  }

  # Use do.call to execute the function with the prepared arguments
  do.call(call_plot, plot_args)

}

#' @export
plot.spec_occ <- function(x, species, ...) {

  wrong_class(x, "spec_occ", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    species = species,
    y_label_default = "Occurrences",
    auto_title_ts = "Species Occurrences",
    leg_label_default = "Occurrences",
    auto_title_map = "Species Occurrences",
    ...
  )

}


#' @export
plot.cum_richness <- function(x, envelopecolour = NULL, ...) {

  wrong_class(x, class = c("cum_richness", "indicator_ts"),
              reason = "incorrect", multiple = TRUE)

  call_plot(
    x,
    y_label_default = "Cumulative Species Richness",
    auto_title_ts = "Cumulative Species Richness",
    smoothed_trend = FALSE,
    ...
  )

}


#' @export
plot.pielou_evenness <- function(x, ...) {

  wrong_class(x, class = "pielou_evenness", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Evenness",
    auto_title_ts = "Pielou's Evenness Trend",
    leg_label_default = "Evenness",
    auto_title_map = "Pielou's Evenness",
    ...
  )

}


#' @export
plot.williams_evenness <- function(x, ...) {

  wrong_class(x, class = "williams_evenness", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Evenness",
    auto_title_ts = "Williams' Evenness Trend",
    leg_label_default = "Evenness",
    auto_title_map = "Williams' Evenness",
    ...
  )

}


#' @export
plot.tax_distinct <- function(x, ...) {

  wrong_class(x, "tax_distinct", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Taxonomic Distinctness",
    auto_title_ts = "Taxonomic Distinctness Trend",
    leg_label_default = "Taxonomic Distinctness",
    auto_title_map = "Taxonomic Distinctness",
    ...
  )

}


#' @export
plot.spec_richness_density <- function(x, ...) {

  wrong_class(x, "spec_richness_density", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Mean Unique Species \nper km^2",
    auto_title_ts = "Trend of Mean Species Richness Density",
    leg_label_default = "Unique Species \nper km^2",
    auto_title_map = "Density of Species Richness",
    ...
  )

}

#' @export
plot.occ_density <- function(x, ...) {

  wrong_class(x, "occ_density", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Mean Occurrences \nper km^2",
    auto_title_ts = "Trend of Mean Occurrence Density",
    leg_label_default = "Occurrences \nper km^2",
    auto_title_map = "Density of Occurrences",
    ...
  )

}

#' @export
plot.newness <- function(x, ...) {

  wrong_class(x, "newness", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Mean Year of Occurrence",
    auto_title_ts = "Trend of Mean Year of Occurrence",
    leg_label_default = "Mean Year of \nOccurrence",
    auto_title_map = "Mean Year of Occurrence",
    ...
  )

}


#' @export
plot.total_occ <- function(x, ...) {

  wrong_class(x, "total_occ", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Occurrences",
    auto_title_ts = "Trend of Total Occurrences",
    leg_label_default = "Occurrences",
    auto_title_map = "Total Occurrences",
    ...
  )

}

#' @export
plot.area_rarity <- function(x, ...) {

  wrong_class(x, "area_rarity", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Mean of Rarity (Summed by Cell)",
    auto_title_ts = "Area-Based Rarity Trend",
    leg_label_default = "Summed Rarity",
    auto_title_map = "Area-Based Rarity",
    ...
  )

}

#' @export
plot.ab_rarity <- function(x, ...) {

  wrong_class(x, "ab_rarity", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Mean of Rarity (Summed by Cell)",
    auto_title_ts = "Abundance-Based Rarity Trend",
    leg_label_default = "Summed Rarity",
    auto_title_map = "Abundance-Based Rarity",
    ...
  )

}

#' @export
plot.completeness <- function(x, ...) {

  wrong_class(x, "completeness", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Sample Coverage",
    auto_title_ts = "Trend of Sample Coverage",
    leg_label_default = "Sample Coverage",
    auto_title_map = "Sample Coverage",
    ...
  )

}

#' @export
plot.hill2 <- function(x, ...) {

  wrong_class(x, "hill2", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Hill-Simpson Diversity",
    auto_title_ts = "Hill-Simpson Diversity Trend (Estimated by Coverage-Based Rarefaction)",
    leg_label_default = "Hill-Simpson Diversity",
    auto_title_map = "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)",
    ...
  )

}

#' @export
plot.hill1 <- function(x, ...) {

  wrong_class(x, "hill1", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Hill-Shannon Diversity",
    auto_title_ts = "Hill-Shannon Diversity Trend (Estimated by Coverage-Based Rarefaction)",
    leg_label_default = "Hill-Shannon Diversity",
    auto_title_map = "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)",
    ...
  )

}

#' @export
plot.hill0 <- function(x, ...) {

  wrong_class(x, "hill0", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Species Richness",
    auto_title_ts = "Species Richness Trend (Estimated by Coverage-Based Rarefaction)",
    leg_label_default = "Richness",
    auto_title_map = "Species Richness (Estimated by Coverage-Based Rarefaction)",
    ...
  )

}

#' @export
plot.obs_richness <- function(x, ...) {

  wrong_class(x, "obs_richness", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(x,
            y_label_default = "Species Richness",
            auto_title_ts = "Observed Species Richness Trend",
            leg_label_default = "Richness",
            auto_title_map = "Observed Species Richness",
            ...)

}

#' @export
plot.occ_turnover <- function(x, auccolour = NULL,  ...) {

  wrong_class(x, c("occ_turnover", "indicator_ts"),
              reason = "incorrect", multiple = TRUE)

  # Set defaults
  y_label_default <- "Occupancy Turnover"
  auto_title <- "Occupancy Turnover"

  call_plot(x,
            y_label_default = "Occupancy Turnover",
            auto_title_ts = "Occupancy Turnover",
            ...)

}
