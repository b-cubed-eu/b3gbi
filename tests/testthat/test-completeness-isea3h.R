# Mock my_DataInfo to avoid slow iNEXT calculations
mock_DataInfo <- function(x, ...) {
    tibble::tibble(
        Assemblage = names(x),
        SC = rep(0.8, length(x))
    )
}

test_that("completeness_map works with isea3h cube", {
    # These tests require dggridR for safe grid creation.
    # Without dggridR, the native ISEA3H fallback uses LAEA->WGS84
    # transforms that can crash R on some PROJ installations.
    skip_if_not(
        requireNamespace("dggridR", quietly = TRUE),
        "dggridR not available for safe ISEA3H grid creation"
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
        coord_range = c(xmin = -20, xmax = 40, ymin = 30, ymax = 75),
        resolution = "4"
    )
    class(mock_cube) <- c("processed_cube", "completeness", "list")

    # Test completeness_map
    with_mocked_bindings(
        my_DataInfo = mock_DataInfo,
        {
            result <- suppressWarnings(suppressMessages(completeness_map(mock_cube)))
        }
    )
    expect_s3_class(result, "indicator_map")
    expect_equal(result$grid_type, "isea3h")
    expect_true("diversity_val" %in% names(result$data))
})

test_that("indicator_ts for completeness works with isea3h", {
    skip_if_not(
        requireNamespace("dggridR", quietly = TRUE),
        "dggridR not available for safe ISEA3H grid creation"
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
        coord_range = c(xmin = -20, xmax = 40, ymin = 30, ymax = 75),
        resolution = "4"
    )
    class(mock_cube) <- c("processed_cube", "completeness", "list")

    # Add ci_type = 'none' to speed up the test significantly
    with_mocked_bindings(
        my_DataInfo = mock_DataInfo,
        {
            result <- suppressWarnings(suppressMessages(completeness_ts(mock_cube, ci_type = "none")))
        }
    )
    expect_s3_class(result, "indicator_ts")
    expect_equal(nrow(result$data), 2)
})
