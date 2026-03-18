#' Checks and adjusts cell size based on resolution and level.
#'
#' @param cell_size The desired cell size (numeric or NULL).
#' @param resolution The resolution of the data (e.g., "10km", "0.25degrees").
#' @param level The geographical level ("world", "continent", or other).
#' @param area (Optional) The area of the cube in square kilometers (numeric or
#'  NULL).
#' @param max_warn_cells (Optional) Maximum recommended number of cells before
#' issuing a warning (default: 1,000,000).
#'
#' @return A compatible cell size in meters (if resolution is in km) or degrees.
#'
#' @examples
#' check_cell_size(10, "1km", "country")
#' check_cell_size(NULL, "0.25degrees", "world")
#'
#' @noRd
#'
check_cell_size <- function(cell_size,
                            resolution,
                            level,
                            area = NULL,
                            max_warn_cells = 1000000) {

  # --- 1. Determine the base resolution size & DYNAMIC warning threshold ---
  if (stringr::str_detect(resolution, "km")) {
    res_unit <- "km"
    res_size <- as.numeric(stringr::str_extract(resolution, "[0-9.]+(?=km)"))

    # Calculate the AUTO threshold (used ONLY if cell_size="auto")
    if (!is.null(area)) {
      # Original logic for 'auto' determination (remains unchanged)
      auto_threshold <- ifelse(as.numeric(area) >= 1000000, 100,
                               ifelse(as.numeric(area) >= 10000, 10,
                                      ifelse(as.numeric(area) >= 100, 1, 0.1)))

      # FIX: Calculate raw threshold, then round UP to the nearest valid multiple
      if (as.numeric(area) > 0 && max_warn_cells > 0) {
        raw_warn_threshold <- sqrt(as.numeric(area) / max_warn_cells)
        num_multiples <- ceiling(raw_warn_threshold / res_size)
        final_warning_threshold <- num_multiples * res_size
      } else {
        final_warning_threshold <- auto_threshold
      }

    } else {
      # If area is NULL, use the fallback threshold for both auto and warning
      auto_threshold <- 10
      final_warning_threshold <- 10
    }
  } else if (stringr::str_detect(resolution, "degrees")) {
    res_unit <- "degrees"
    res_size <- as.numeric(stringr::str_extract(resolution, "[0-9.]+(?=degrees)"))

    if (level == "world" || level == "continent") {
      final_warning_threshold <- 1
    } else {
      final_warning_threshold <- 0.1
    }
    # Apply rounding up to the nearest res_size multiple for degrees threshold
    if (final_warning_threshold < res_size) {
      final_warning_threshold <- res_size
    } else {
      num_multiples <- ceiling(final_warning_threshold / res_size)
      final_warning_threshold <- num_multiples * res_size
    }
  } else if (resolution == "isea3h") {
    res_unit <- "isea3h"
    res_size <- 1 # Placeholder for hex grid "size"
    
    if (level == "world" || level == "continent") {
      final_warning_threshold <- 1
    } else {
      final_warning_threshold <- 0.1
    }
  } else {
    stop(paste0("Resolution units not recognized."))
  }

  original_cell_size <- cell_size


  if (is.character(cell_size)) {
    cell_size <- tolower(cell_size)
    if (cell_size == "grid") {
      cell_size <- res_size

      if (res_size < final_warning_threshold) {

        message(paste0(
          "Setting cell_size to match data resolution (", res_size, res_unit,
          "). This is smaller than the recommended minimum based on ", max_warn_cells,
          " total cells (", final_warning_threshold,
          res_unit, "). This may result in a large output and long processing time.",
          " Consider using 'auto' or manually setting a larger size."
        ))
        prompt_text <- "Do you want to proceed ('y'/'n') or switch to 'auto' ('a')? [y/n/a]: "

        if (interactive()) {
          user_input <- tolower(readline(prompt = prompt_text))
          if (user_input == "a") {
            cell_size <- NULL
          } else if (user_input == "n") {
            stop("User aborted process. To proceed, manually set cell_size or choose 'auto'.")
          } else if (user_input != "y") {
            # Invalid input: abort cleanly
            stop("Invalid input. Aborting. Please restart and confirm, or set to 'auto'.")
          }
        } else {
          stop(paste0(
            "Non-interactive session detected. 'cell_size = \"grid\"' requested ",
            "but requires interactive confirmation due to potential large output. ",
            "Please run interactively, or set cell_size to a numeric value or 'auto'."
          ))
        }
      }
    } else if (cell_size == "auto") {
      cell_size <- NULL
    } else {
      stop("Invalid character value for cell_size. Use 'grid', 'auto', or a numeric value.")
    }
  }

  if (is.null(cell_size)) {
    if (res_unit == "km") {

      # FIX: Apply the area-based auto-determination for ALL km levels if area is available.
      if (!is.null(area)) {
        # This is the untouched 'auto' determination logic (uses original thresholds)
        cell_size <- ifelse(as.numeric(area) >= 1000000, 100,
                            ifelse(as.numeric(area) >= 10000, 10,
                                   ifelse(as.numeric(area) >= 100, 1, 0.1)))
      } else if (level == "cube") {
        # If level is 'cube' and area is missing, this is a definite STOP.
        stop(paste0("Unable to determine area of cube for automated cell ",
                    "size determination. Please enter cell size manually."))
      } else {
        # Fallback for non-cube levels without area (e.g., if world/continent was supported)
        # Use a sensible, large default to avoid the comparison error.
        cell_size <- 10 # Default to 10km if area is unknown for non-cube level.
      }

      # This check now runs reliably on a numeric cell_size value.
      if (cell_size < res_size) {
        message(paste0("Automatically determined grid cell size of ", cell_size,
                       " km would be smaller than the grid cells of the cube. Therefore, ",
                       "setting cell_size to ", res_size, " km to match cube resolution."))
        cell_size <- res_size
      }
    } else { # res_unit == "degrees"
      if (level == "world" || level == "continent") {
        cell_size <- ifelse(res_size < 1, 1, res_size)
      } else {
        cell_size <- ifelse(res_size < 0.1, 0.1, res_size)
      }
    }
  }

  if (is.numeric(original_cell_size)) {

    # Check against the rounded, valid final_warning_threshold
    is_small_and_not_native <- (original_cell_size < final_warning_threshold)

    if (is_small_and_not_native) {
      message(paste0(
        "ATTENTION: User-provided cell_size (", original_cell_size, res_unit,
        ") is smaller than the recommended minimum based on ", max_warn_cells,
        " total cells (", final_warning_threshold,
        res_unit, "). This may result in a large output and long processing time."
      ))

      prompt_text <- "Do you want to proceed ('y'/'n')? [y/n]: "

      if (interactive()) {
        user_input <- tolower(readline(prompt = prompt_text))
        if (user_input == "n") {
          stop("User aborted process. Please increase cell_size or set to 'auto'.")
        } else if (user_input != "y") {
          message("Invalid input. Proceeding with the set cell_size.")
        }
      } else {
        warning(paste0("Non-interactive session detected. Proceeding with small cell_size (",
                       original_cell_size, res_unit, ")."))
      }
    }
  }

  # --- 5. Multiplier check and return ---

  # 💥 FIX: Add defensive check for non-numeric arguments (Prevents the 'non-numeric argument' error) 💥
  if (!is.numeric(cell_size) || !is.numeric(res_size) || is.na(cell_size) || is.na(res_size)) {
    stop(paste0("Internal error during cell size check: cell_size (",
                cell_size, ") or resolution size (", res_size,
                ") is not numeric. Check your 'resolution' input string (e.g., '10km')."))
  }

  if (res_unit == "km") {
    if (!isTRUE(all.equal(cell_size / res_size, round(cell_size / res_size)))) {
      stop(paste0(
        "cell_size must be a whole number multiple of the resolution. For ",
        "example, if resolution is ", res_size, " km, cell_size can be ",
        paste(unique(c(res_size, 2*res_size, 10*res_size)), collapse = ", "), ", etc."
      ))
    }
    cell_size <- cell_size * 1000
  } else {
    if (!isTRUE(all.equal(cell_size / res_size, round(cell_size / res_size)))) {
      stop(paste0(
        "cell_size must be a whole number multiple of the resolution. For ",
        "example, if resolution is ", res_size, " degrees, cell_size ",
        "can be ", paste(unique(c(res_size, 2*res_size, 1.0)), collapse = ", "), ", etc."
      ))
    }
  }
  return(cell_size)
}
