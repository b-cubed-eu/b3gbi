# Create the mock_cube object
mock_cube <- list(
  data = data.frame(
    scientificName = c("A", "B", "A"),
    xcoord = c(1, 2, 1),
    ycoord = c(1, 2, 1)
  ),
  class = "processed_cube"
)

# Mock compute_indicator_workflow function
mock_compute_indicator <- function(data, type, dim_type, ...) {
  list(data = data, type = type, dim_type = dim_type, dots = list(...))
}

test_that(
  paste0(
    "obs_richness_map calls compute_indicator_workflow with correct arguments ",
    "and returns correct class"
  ), {
    # Create a mock compute_indicator_workflow function
    mock_compute_indicator <- function(
    data, type, dim_type, ...
    ) {
      # Simulate return value of compute_indicator_workflow
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "obs_richness")
      return(result)
    }

    # Call obs_richness_map with additional arguments
    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator,
      obs_richness_map(mock_cube, level = "country", region = "Denmark")
    )

    # Verify arguments
    expect_equal(result$type, "obs_richness")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")

    # Verify class
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "obs_richness")
  })

test_that(
  paste0(
    "obs_richness_ts calls compute_indicator_workflow with correct arguments ",
    "and returns correct class"
  ), {
    # Create a mock compute_indicator_workflow function
    mock_compute_indicator <- function(
    data, type, dim_type, ...
    ) {
      # Simulate return value of compute_indicator_workflow
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "obs_richness")
      return(result)
    }

    # Call obs_richness_ts with additional arguments
    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator,
      obs_richness_ts(mock_cube, first_year = 1985)
    )

    # Verify arguments
    expect_equal(result$type, "obs_richness")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)

    # Verify class
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "obs_richness")
  })

test_that(
  paste0(
    "total_occ_map calls compute_indicator_workflow with correct arguments ",
    "and returns correct class"
  ), {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "total_occ")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      total_occ_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "total_occ")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "total_occ")
  })

test_that(
  paste0(
    "total_occ_map calls compute_indicator_workflow ",
    "with correct arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "total_occ")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      total_occ_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "total_occ")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "total_occ")
  }
)

test_that(
  paste0(
    "total_occ_ts calls compute_indicator_workflow ",
    "with correct arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "total_occ")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      total_occ_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "total_occ")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "total_occ")
  }
)

test_that(
  paste0(
    "pielou_evenness_map calls compute_indicator_workflow ",
    "with correct arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "pielou_evenness")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      pielou_evenness_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "pielou_evenness")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "pielou_evenness")
  }
)

test_that(
  paste0(
    "pielou_evenness_ts calls compute_indicator_workflow ",
    "with correct arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "pielou_evenness")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      pielou_evenness_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "pielou_evenness")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "pielou_evenness")
  }
)

test_that(
  paste0(
    "williams_evenness_map calls compute_indicator_workflow ",
    "with correct arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "williams_evenness")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      williams_evenness_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "williams_evenness")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "williams_evenness")
  }
)

test_that(
  paste0(
    "williams_evenness_ts calls compute_indicator_workflow ",
    "with correct arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "williams_evenness")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      williams_evenness_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "williams_evenness")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "williams_evenness")
  }
)

test_that(
  paste0(
    "area_rarity_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "area_rarity")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      area_rarity_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "area_rarity")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "area_rarity")
  }
)

test_that(
  paste0(
    "area_rarity_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "area_rarity")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      area_rarity_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "area_rarity")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "area_rarity")
  }
)

test_that(
  paste0(
    "ab_rarity_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "ab_rarity")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      ab_rarity_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "ab_rarity")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "ab_rarity")
  }
)

test_that(
  paste0(
    "ab_rarity_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "ab_rarity")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      ab_rarity_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "ab_rarity")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "ab_rarity")
  }
)

test_that(
  paste0(
    "hill0_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "hill0")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      hill0_map(mock_cube, level = "country", region = "Denmark",
                hill0_type = "hill0")
    )

    expect_equal(result$type, "hill0")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_equal(result$dots$hill0_type, "hill0")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "hill0")
  }
)

test_that(
  paste0(
    "hill0_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "hill0")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      hill0_ts(mock_cube, first_year = 1985, hill0_type = "hill0")
    )

    expect_equal(result$type, "hill0")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_equal(result$dots$hill0_type, "hill0")
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "hill0")
  }
)

test_that(
  paste0(
    "hill1_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "hill1")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      hill1_map(mock_cube, level = "country", region = "Denmark",
                hill1_type = "hill1")
    )

    expect_equal(result$type, "hill1")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_equal(result$dots$hill1_type, "hill1")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "hill1")
  }
)

test_that(
  paste0(
    "hill1_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "hill1")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      hill1_ts(mock_cube, first_year = 1985, hill1_type = "hill1")
    )

    expect_equal(result$type, "hill1")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_equal(result$dots$hill1_type, "hill1")
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "hill1")
  }
)

test_that(
  paste0(
    "hill2_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "hill2")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      hill2_map(mock_cube, level = "country", region = "Denmark",
                hill2_type = "hill2")
    )

    expect_equal(result$type, "hill2")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_equal(result$dots$hill2_type, "hill2")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "hill2")
  }
)

test_that(
  paste0(
    "hill2_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "hill2")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      hill2_ts(mock_cube, first_year = 1985, hill2_type = "hill2")
    )

    expect_equal(result$type, "hill2")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_equal(result$dots$hill2_type, "hill2")
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "hill2")
  }
)

test_that(
  paste0(
    "cum_richness_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "cum_richness")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      cum_richness_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "cum_richness")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "cum_richness")
  }
)

test_that(
  paste0(
    "newness_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "newness")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      newness_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "newness")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "newness")
  }
)

test_that(
  paste0(
    "newness_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "newness")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      newness_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "newness")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "newness")
  }
)

test_that(
  paste0(
    "occ_density_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "occ_density")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      occ_density_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "occ_density")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "occ_density")
  }
)

test_that(
  paste0(
    "occ_density_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "occ_density")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      occ_density_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "occ_density")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "occ_density")
  }
)

test_that(
  paste0(
    "spec_occ_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "spec_occ")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      spec_occ_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "spec_occ")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "spec_occ")
  }
)

test_that(
  paste0(
    "spec_occ_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "spec_occ")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      spec_occ_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "spec_occ")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "spec_occ")
  }
)

test_that(
  paste0(
    "spec_range_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "spec_range")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      spec_range_map(mock_cube, level = "country", region = "Denmark")
    )

    expect_equal(result$type, "spec_range")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "spec_range")
  }
)

test_that(
  paste0(
    "spec_range_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "spec_range")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      spec_range_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "spec_range")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "spec_range")
  }
)

test_that(
  paste0(
    "tax_distinct_map calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_map", "tax_distinct")
      return(result)
    }

    result <- withCallingHandlers(
      {
        with_mocked_bindings(
          compute_indicator_workflow = mock_compute_indicator_return,
          tax_distinct_map(mock_cube, level = "country", region = "Denmark")
        )
      },
      requireNamespace = function(e) {
        invokeRestart("muffleMessage")
        TRUE # Simulate taxize being installed
      }
    )

    expect_equal(result$type, "tax_distinct")
    expect_equal(result$dim_type, "map")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$level, "country")
    expect_equal(result$dots$region, "Denmark")
    expect_s3_class(result, "indicator_map")
    expect_s3_class(result, "tax_distinct")
  }
)

test_that(
  paste0(
    "tax_distinct_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "tax_distinct")
      return(result)
    }

    result <- withCallingHandlers(
      {
        with_mocked_bindings(
          compute_indicator_workflow = mock_compute_indicator_return,
          tax_distinct_ts(mock_cube, first_year = 1985)
        )
      },
      requireNamespace = function(e) {
        invokeRestart("muffleMessage")
        TRUE # Simulate taxize being installed
      }
    )

    expect_equal(result$type, "tax_distinct")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "tax_distinct")
  }
)

test_that(
  paste0(
    "occ_turnover_ts calls compute_indicator_workflow with correct ",
    "arguments and returns correct class"
  ),
  {
    mock_compute_indicator_return <- function(
    data, type, dim_type, ...
    ) {
      result <- list(
        data = data,
        type = type,
        dim_type = dim_type,
        dots = list(...)
      )
      class(result) <- c("indicator_ts", "occ_turnover")
      return(result)
    }

    result <- with_mocked_bindings(
      compute_indicator_workflow = mock_compute_indicator_return,
      occ_turnover_ts(mock_cube, first_year = 1985)
    )

    expect_equal(result$type, "occ_turnover")
    expect_equal(result$dim_type, "ts")
    expect_equal(result$data, mock_cube)
    expect_equal(result$dots$first_year, 1985)
    expect_s3_class(result, "indicator_ts")
    expect_s3_class(result, "occ_turnover")
  }
)

# Mock get_ne_data to avoid downloads and slow intersections
mock_get_ne_data <- function(projected_crs, ...) {
  # Create test geometry in 4326
  geom <- sf::st_sfc(sf::st_polygon(list(matrix(c(5, 50, 15, 50, 15, 60, 5, 60, 5, 50), ncol = 2, byrow = TRUE))), crs = 4326)

  # Transform to the requested projected_crs to avoid mismatch
  map_data_combined <- sf::st_sf(geometry = sf::st_transform(geom, crs = projected_crs))
  map_data_save <- sf::st_sf(geometry = sf::st_transform(geom, crs = projected_crs))

  list(
    combined = map_data_combined,
    saved = map_data_save
  )
}

# Test 1: Map output basic
test_that("spec_richness_density_map basic", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")
  with_mocked_bindings(
    get_ne_data = mock_get_ne_data,
    {
      res <- spec_richness_density_map(example_cube_1, level = "country", region = "Denmark")
      expect_s3_class(res, "indicator_map")
      expect_true(any(!is.na(res$data$diversity_val)))
    }
  )
})

# Test 2: TS output (CIs now excluded)
test_that("spec_richness_density_ts Denmark CI exclusion", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")
  # Expect warning since it's now in noci_list
  expect_warning(
    res <- spec_richness_density_ts(example_cube_1,
                                    level = "country",
                                    region = "Denmark",
                                    # Minimize bootstraps to speed up the test
                                    num_bootstrap = 2
    ),
    "Bootstrapped confidence intervals cannot be calculated"
  )
  expect_s3_class(res, "indicator_ts")
  expect_false("ll" %in% names(res$data))
})

# Test 3: Cube level map
test_that("spec_richness_density_map cube", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")
  res <- spec_richness_density_map(example_cube_1)
  expect_s3_class(res, "indicator_map")
})

# Test 4: Cube level TS
test_that("spec_richness_density_ts cube", {
  skip_on_cran()
  data(example_cube_1, package = "b3gbi")
  # Use ci_type = "none" to avoid warning
  res <- spec_richness_density_ts(example_cube_1, ci_type = "none")
  expect_s3_class(res, "indicator_ts")
  expect_false("ll" %in% names(res$data))
})
