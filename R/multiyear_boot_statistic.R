#' @noRd
# Function for boot statistic that uses two years of data
multiyear_boot_statistic <- function(data_current,
                                     data_previous,
                                     indices_current,
                                     indices_previous) {
  tax_current <- data_current # [indices_current]
  tax_previous <- data_previous # [indices_previous]

  # Determine the new species added each year
  tax_added <- setdiff(tax_current, tax_previous)

  # Determine the species lost each year
  tax_lost <- setdiff(tax_previous, tax_current)

  # Combine the species present in the current with those present in the
  # previous year
  tax_present <- union(tax_previous, tax_current)

  total_species <- length(tax_present)

  if (total_species == 0) {
    return(NA)
  }

  # Calculate occupancy turnover as the sum of the number of species added and
  # the number of species lost divided by the total number of species present in
  # the current and previous year combined
  occ_turnover <- (length(tax_added) + length(tax_lost)) /
    length(tax_present)

  return(occ_turnover)
}
