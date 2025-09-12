#' @export
plot.spec_range <- function(x, ...) {

  wrong_class(x, "spec_range", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    y_label_default = "Cells Occupied",
    auto_title_ts = "Species Range Size",
    auto_title_map = "Species Range",
    suppress_legend = TRUE,
    ...
  )

}

#' @export
plot.spec_occ <- function(x, ...) {

  wrong_class(x, "spec_occ", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Occurrences",
    "Species Occurrences",
    "Occurrences",
    "Species Occurrences",
    ...
  )

}


#' @export
plot.cum_richness <- function(x, envelopecolour = NULL, ...) {

  wrong_class(x, class = c("cum_richness", "indicator_ts"),
              reason = "incorrect", multiple = TRUE)

  call_plot(
    x,
    "Cumulative Species Richness",
    "Cumulative Species Richness",
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
    "Evenness",
    "Pielou's Evenness Trend",
    "Evenness",
    "Pielou's Evenness",
    ...
  )

}


#' @export
plot.williams_evenness <- function(x, ...) {

  wrong_class(x, class = "williams_evenness", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Evenness",
    "Williams' Evenness Trend",
    "Evenness",
    "Williams' Evenness",
    ...
  )

}


#' @export
plot.tax_distinct <- function(x, ...) {

  wrong_class(x, "tax_distinct", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Taxonomic Distinctness",
    "Taxonomic Distinctness Trend",
    "Taxonomic Distinctness",
    "Taxonomic Distinctness",
    ...
  )

}


#' @export
plot.occ_density <- function(x, ...) {

  wrong_class(x, "occ_density", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Mean Occurrences \nper km^2",
    "Trend of Mean Occurrence Density",
    "Occurrences \nper km^2",
    "Density of Occurrences",
    ...
  )

}

#' @export
plot.newness <- function(x, ...) {

  wrong_class(x, "newness", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Mean Year of Occurrence",
    "Trend of Mean Year of Occurrence",
    "Mean Year of \nOccurrence",
    "Mean Year of Occurrence",
    ...
  )

}


#' @export
plot.total_occ <- function(x, ...) {

  wrong_class(x, "total_occ", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Occurrences",
    "Trend of Total Occurrences",
    "Occurrences",
    "Total Occurrences",
    ...
  )

}

#' @export
plot.area_rarity <- function(x, ...) {

  wrong_class(x, "area_rarity", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Mean of Rarity (Summed by Cell)",
    "Area-Based Rarity Trend",
    "Summed Rarity",
    "Area-Based Rarity",
    ...
  )

}

#' @export
plot.ab_rarity <- function(x, ...) {

  wrong_class(x, "ab_rarity", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Mean of Rarity (Summed by Cell)",
    "Abundance-Based Rarity Trend",
    "Summed Rarity",
    "Abundance-Based Rarity",
    ...
  )

}

#' @export
plot.hill2 <- function(x, ...) {

  wrong_class(x, "hill2", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Hill-Simpson Diversity",
    "Hill-Simpson Diversity Trend (Estimated by Coverage-Based Rarefaction)",
    "Hill-Simpson Diversity",
    "Hill-Simpson Diversity (Estimated by Coverage-Based Rarefaction)",
    ...
  )

}

#' @export
plot.hill1 <- function(x, ...) {

  wrong_class(x, "hill1", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Hill-Shannon Diversity",
    "Hill-Shannon Diversity Trend (Estimated by Coverage-Based Rarefaction)",
    "Hill-Shannon Diversity",
    "Hill-Shannon Diversity (Estimated by Coverage-Based Rarefaction)",
    ...
  )

}

#' @export
plot.hill0 <- function(x, ...) {

  wrong_class(x, "hill0", reason = "incorrect")
  wrong_class(x, c("indicator_ts", "indicator_map"), reason = "incorrect")

  call_plot(
    x,
    "Species Richness",
    "Species Richness Trend (Estimated by Coverage-Based Rarefaction)",
    "Richness",
    "Species Richness (Estimated by Coverage-Based Rarefaction)",
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
