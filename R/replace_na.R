#' Replace NA Values in Indicator Objects
#'
#' This function takes an \code{indicator_map} or \code{indicator_ts} object and
#' replaces all NA values in the \code{diversity_val} column with zeros. This is
#' useful when NA values represent areas or years with no data that should be
#' treated as zero for mapping or analysis purposes.
#'
#' @param x An \code{indicator_map} or \code{indicator_ts} object containing a
#'  \code{diversity_val} column with NA values to replace.
#'
#' @return The input indicator object with NA values in \code{diversity_val}
#'  replaced by zero.
#'
#' @examples
#' \dontrun{
#' # Assuming 'result' is an indicator_map or indicator_ts object
#' result_filled <- replace_na(result)
#' }
#'
#' @export
replace_na <- function(x) {

  # Check that the object is the correct class
  if (!inherits(x, c("indicator_map", "indicator_ts"))) {
    stop("Incorrect object class. Must be class indicator_map or indicator_ts.")
  }

  # Pull the diversity value
  dv <- x$data$diversity_val

  # Record how many NA values are present
  number_na_before <- length(dv[is.na(dv)])
  if (number_na_before == 0) {
    stop("No NA values present. Nothing replaced.")
  }

  # Replace all NA values with zero
  dv[is.na(dv)] <- 0

  # Record how many NA values (if any) are still present
  number_na_after <- length(dv[is.na(dv)])

  # Record how many NA values were replaced with zeroes
  number_na_replaced <- number_na_before - number_na_after

  # Tell the user how many NA values were replaced
  print(paste0("Replaced ", number_na_replaced, " NA values with zeroes."))

  # If any NA values remain, tell the user
  if (number_na_after > 0) {
    print(paste0(number_na_after, " NA values could not be replaced."))
  }

  # Put the vector with the replaced values back into the indicator object
  x$data$diversity_val <- dv

  return(x)

}
