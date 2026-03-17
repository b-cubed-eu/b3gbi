# Helper: checks if the native ISEA3H fallback can produce a non-empty grid.
# Returns TRUE if the grid has rows, FALSE otherwise.
.isea3h_proj_works <- function() {
    grid <- suppressWarnings(suppressMessages(
        create_isea3h_grid(
            data.frame(
                cellCode = c("-458282525011250000", "452583359004665569"),
                xcoord = c(11.25, 4.67), ycoord = c(58.28, 52.58)
            ),
            projection = "+proj=longlat +datum=WGS84"
        )
    ))
    nrow(grid) > 0
}

test_that("completeness_map works with isea3h cube", {
    skip_if_not(
        .isea3h_proj_works(),
        "PROJ cannot round-trip LAEA on this system"
    )

    # Mock a processed cube with isea3h data
    # Need enough species to pass the default cutoff_length filter
    mock_data <- tibble::tibble(
        cellCode = rep(c("-458282525011250000", "452583359004665569"), each = 10),
        year = rep(2020:2021, 10),
        obs = rpois(20, 5) + 1,
        taxonKey = rep(1:10, 2),
        scientificName = paste("Species", rep(1:10, 2)),
        xcoord = rep(c(11.25, 4.67), each = 10),
        ycoord = rep(c(58.28, 52.58), each = 10),
        resolution = "4"
    )

    mock_cube <- list(
        data = mock_data,
        grid_type = "isea3h",
        first_year = 2020,
        last_year = 2021,
        num_species = 10,
        species_names = paste("Species", 1:10),
        coord_range = list(xmin = -20, xmax = 40, ymin = 30, ymax = 75),
        resolution = "4"
    )
    class(mock_cube) <- c("processed_cube", "completeness", "list")

    # Test completeness_map
    result <- suppressWarnings(suppressMessages(completeness_map(mock_cube)))
    expect_s3_class(result, "indicator_map")
    expect_equal(result$grid_type, "isea3h")
    expect_true("diversity_val" %in% names(result$data))
})

test_that("indicator_ts for completeness works with isea3h", {
    skip_if_not(
        .isea3h_proj_works(),
        "PROJ cannot round-trip LAEA on this system"
    )

    mock_data <- tibble::tibble(
        cellCode = rep(c("-458282525011250000", "452583359004665569"), each = 10),
        year = rep(2020:2021, 10),
        obs = rpois(20, 5) + 1,
        taxonKey = rep(1:10, 2),
        scientificName = paste("Species", rep(1:10, 2)),
        xcoord = rep(c(11.25, 4.67), each = 10),
        ycoord = rep(c(58.28, 52.58), each = 10),
        resolution = "4"
    )

    mock_cube <- list(
        data = mock_data,
        grid_type = "isea3h",
        first_year = 2020,
        last_year = 2021,
        num_species = 10,
        species_names = paste("Species", 1:10),
        coord_range = list(xmin = -20, xmax = 40, ymin = 30, ymax = 75),
        resolution = "4"
    )
    class(mock_cube) <- c("processed_cube", "completeness", "list")

    result <- suppressWarnings(suppressMessages(completeness_ts(mock_cube)))
    expect_s3_class(result, "indicator_ts")
    expect_equal(nrow(result$data), 2)
})
