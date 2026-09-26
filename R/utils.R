# is_sf_empty()
# A simple helper function to check if an sf object has an empty geometry or no rows.
#
# @param x An sf or sfc object.
#
# @return A logical value: TRUE if the object is empty (no rows or empty geometry),
#         FALSE otherwise.
#' @noRd
is_sf_empty <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  # Handles cases where nrow() is a valid check (e.g., sf data frames)
  if (is.data.frame(x) && nrow(x) == 0) {
    return(TRUE)
  }
  # A more robust check for GEOMETRYCOLLECTION EMPTY and sfc objects
  # This handles cases like `sf::st_sfc(sf::st_geometrycollection())`
  if (all(sf::st_is_empty(x))) {
    return(TRUE)
  }
  return(FALSE)
}

# Define function to wrap title and legend title if too long
#' @noRd
wrapper <- function(x, ...) {
  paste(strwrap(x, ...), collapse = "\n")
}

# alternative sampling function that works properly even with length of 1
#' @noRd
resample <- function(x, size, replace = TRUE) {

  if (length(x) == 1) {

    return(rep(x, size))

  } else {

    return(sample(x, size, replace = replace))

  }

}

# Function to add missing year values into bootstrap results
# boot is the bootstrap output, and orig_data is the original data used to
# calculate the bootstraps (a data frame with a year column arranged in order)
#' @noRd
add_yearvals_to_boot <- function(boot, orig_data) {
  unique_years <- unique(orig_data$year)
  if (length(unique_years) <= 1) {
    # Ensure a non-error path for single-year input
    names(boot) <- NULL
  } else {
    names(boot) <- unique_years[2:length(unique_years)]
  }
  return(boot)
}

#' Stop with a custom error message unless all conditions are TRUE
#'
#' @param err_message The error message.
#' @param ... Logical conditions; each must be non-missing and all `TRUE`.
#' @noRd
stopifnot_error <- function(err_message, ...) {
  conditions <- list(...)
  ok <- vapply(conditions, function(cond) {
    is.logical(cond) && !anyNA(cond) && all(cond)
  }, logical(1))
  if (!all(ok)) {
    stop(err_message)
  }
  invisible(TRUE)
}

#' Stop with an error message if the object is not the correct class
#' @noRd
wrong_class <- function(object,
                        class,
                        reason = c("internal",
                                   "unrecognized",
                                   "incorrect"),
                        multiple = FALSE,
                        ...) {

  reason <- match.arg(reason)

  collapse <- if (multiple == TRUE) " and " else " or "

  err_message <- if (reason == "internal") {
    paste0("Wrong data class. Must be class, ",
           paste(class, collapse = collapse), ". Note that this is ",
           "an internal function and is not meant to be called directly.")
  } else if (reason == "incorrect") {
    paste0("Incorrect object class. Must be class ",
           paste(class, collapse = collapse), ".")
  } else if (reason == "unrecognized") {
    paste0("Object class not recognized. Must be one of the following: ",
           paste(class, collapse = collapse), ".")
  }

  is_correct <- if (multiple) {
    # Check if the object inherits from ALL classes
    all(sapply(class, function(cl) my_inherits(object, cl)))
  } else {
    # Check if the object inherits from ANY of the classes
    my_inherits(object, class)
  }

  if (length(is_correct) == 0 || is.na(is_correct) || !is_correct) {
    stop(err_message)
  }
}

# Wrapper of function readRDS from base. This is for mocking in testthat tests.
#' @noRd
my_readRDS <- function(file, ...) {
  readRDS(file, ...)
}

# Wrapper of function iNext::estimateD. This is for mocking in testthat tests.
#' @noRd
my_estimateD <- function(x, datatype = "abundance", base = "size", level = NULL, q = 0, conf = 0.95, nboot = 50, ...) {
  # If q does not contain 1, or datatype is abundance, bypass the patch
  if (!(1 %in% q) || !(datatype %in% c("incidence_freq", "incidence_raw"))) {
    return(iNEXT::estimateD(x, datatype = datatype, base = base, level = level, q = q, conf = conf, nboot = nboot, ...))
  }

  # Ensure x is a list and has names (since iNEXT behaves differently otherwise)
  is_list_input <- is.list(x)
  if (!is_list_input) {
    x_list <- list(site1 = x)
  } else {
    x_list <- x
    if (is.null(names(x_list))) {
      names(x_list) <- paste0("site", seq_along(x_list))
    }
  }

  bad_indices <- logical(length(x_list))

  for (i in seq_along(x_list)) {
    x_i <- x_list[[i]]
    if (datatype == "incidence_freq") {
      nT <- x_i[1]
      Yi <- x_i[-1]
      Yi <- Yi[Yi != 0]
      yi <- Yi[Yi >= 1 & Yi <= (nT - 1)]
      if (length(yi) == 0) {
        bad_indices[i] <- TRUE
      }
    } else if (datatype == "incidence_raw") {
      nT <- ncol(x_i)
      Yi <- rowSums(x_i)
      Yi <- Yi[Yi != 0]
      yi <- Yi[Yi >= 1 & Yi <= (nT - 1)]
      if (length(yi) == 0) {
        bad_indices[i] <- TRUE
      }
    }
  }

  results_list <- list()

  # Process the good ones
  if (any(!bad_indices)) {
    good_x <- x_list[!bad_indices]
    good_results <- iNEXT::estimateD(good_x, datatype = datatype, base = base, level = level, q = q, conf = conf, nboot = nboot, ...)
    results_list[[length(results_list) + 1]] <- good_results
  }

  # Process the bad ones
  if (any(bad_indices)) {
    bad_x <- x_list[bad_indices]
    
    # 1. Estimate for q values other than 1
    q_other <- q[q != 1]
    bad_other_results <- NULL
    if (length(q_other) > 0) {
      bad_other_results <- iNEXT::estimateD(bad_x, datatype = datatype, base = base, level = level, q = q_other, conf = conf, nboot = nboot, ...)
    }
    
    # 2. Estimate for q = 1
    bad_q1_results <- data.frame(
      Assemblage = names(bad_x),
      t = numeric(length(bad_x)),
      Method = character(length(bad_x)),
      Order.q = numeric(length(bad_x)),
      SC = numeric(length(bad_x)),
      qD = numeric(length(bad_x)),
      qD.LCL = numeric(length(bad_x)),
      qD.UCL = numeric(length(bad_x)),
      stringsAsFactors = FALSE
    )
    
    for (k in seq_along(bad_x)) {
      x_i <- bad_x[[k]]
      if (datatype == "incidence_freq") {
        nT <- x_i[1]
        Yi <- x_i[-1]
        Yi <- Yi[Yi != 0]
        Sobs <- length(Yi)
      } else {
        nT <- ncol(x_i)
        Yi <- rowSums(x_i)
        Yi <- Yi[Yi != 0]
        Sobs <- length(Yi)
      }
      
      sc_val <- if (!is.null(level)) level else 1.0
      
      bad_q1_results$t[k] <- nT
      bad_q1_results$Method[k] <- "Observed"
      bad_q1_results$Order.q[k] <- 1
      bad_q1_results$SC[k] <- sc_val
      bad_q1_results$qD[k] <- Sobs
      bad_q1_results$qD.LCL[k] <- Sobs
      bad_q1_results$qD.UCL[k] <- Sobs
    }
    
    if (!is.null(bad_other_results)) {
      bad_combined <- rbind(bad_other_results, bad_q1_results)
    } else {
      bad_combined <- bad_q1_results
    }
    
    results_list[[length(results_list) + 1]] <- bad_combined
  }

  # Combine all results
  combined <- do.call(rbind, results_list)

  # Reorder to match original input list order and q order
  assemblage_names <- names(x_list)
  
  combined$assemblage_order <- match(combined$Assemblage, assemblage_names)
  combined$q_order <- match(combined$Order.q, q)
  combined <- combined[order(combined$assemblage_order, combined$q_order), , drop = FALSE]
  
  combined$assemblage_order <- NULL
  combined$q_order <- NULL
  rownames(combined) <- NULL

  # If the original input was not a list, strip the Assemblage column to match iNEXT behavior
  if (!is_list_input) {
    combined$Assemblage <- NULL
  }

  return(combined)
}

# Transformation functions
# Logit transformation
logit <- function(p) {
  log(p / (1 - p))
}

# Inverse logit transformation
inv_logit <- function(l) {
  exp(l) / (1 + exp(l))
}
# Wrapper of function inherits. This is for mocking in testthat tests.
#' @noRd
my_inherits <- function(x, what) {
  inherits(x, what)
}
# Wrapper of function iNEXT::DataInfo. This is for mocking in testthat tests.
#' @noRd
my_DataInfo <- function(...) {
  iNEXT::DataInfo(...)
}

# Wrapper of requireNamespace. This is for mocking in testthat tests.
#' @noRd
is_package_installed <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

