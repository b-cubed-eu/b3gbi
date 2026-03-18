library(testthat)
library(b3gbi)
library(sf)
library(dplyr)

test_that("coverage boost for indicator formulas", {
    # compute_evenness_formula
    vec <- c(10, 20, 30, 40)
    expect_type(compute_evenness_formula(vec, "pielou_evenness"), "double")
    expect_type(compute_evenness_formula(vec, "williams_evenness"), "double")

    # compute_tax_distinct_formula
    hier1 <- data.frame(rank = c("kingdom", "phylum", "class"), name = c("K1", "P1", "C1"), stringsAsFactors = FALSE)
    hier2 <- data.frame(rank = c("kingdom", "phylum", "class"), name = c("K1", "P1", "C2"), stringsAsFactors = FALSE)
    hier3 <- data.frame(rank = c("kingdom", "phylum", "class"), name = c("K1", "P2", "C3"), stringsAsFactors = FALSE)

    tax_hier <- list(S1 = hier1, S2 = hier2, S3 = hier3)
    x_df <- data.frame(scientificName = c("S1", "S2", "S3"))

    res_tdi <- compute_tax_distinct_formula(x_df, tax_hier)
    expect_type(res_tdi, "double")
})

test_that("coverage boost for union_helper and is_sf_empty", {
    poly1 <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))), crs = 4326))
    empty_sf <- sf::st_sf(geometry = sf::st_sfc(crs = 4326))

    # union_helper branches
    expect_equal(nrow(union_helper(poly1, empty_sf)), 1)
    expect_equal(nrow(union_helper(empty_sf, poly1)), 1)
    expect_equal(nrow(union_helper(empty_sf, empty_sf)), 0)

    # is_sf_empty branches
    expect_true(is_sf_empty(NULL))
    expect_true(is_sf_empty(data.frame()))
    expect_true(is_sf_empty(empty_sf))
    expect_false(is_sf_empty(poly1))
})

test_that("coverage boost for calc_map_hill_core", {
    # Small data for abundance
    mock_data <- tibble::tibble(
        cellid = c(1, 1),
        cellCode = "C1",
        taxonKey = c(1, 2),
        scientificName = c("S1", "S2"),
        obs = c(10, 20)
    )
    class(mock_data) <- c("hill0", "data.frame")

    res <- calc_map_hill_core(mock_data,
        type = "hill0", data_type = "abundance",
        cutoff_length = 1, coverage = 0.5
    )
    expect_s3_class(res, "data.frame")

    # Small data for incidence
    mock_data_inc <- mock_data %>% dplyr::mutate(year = 2020)
    class(mock_data_inc) <- c("hill0", "data.frame")
    res_inc <- calc_map_hill_core(mock_data_inc,
        type = "hill0", data_type = "incidence",
        cutoff_length = 1, coverage = 0.5, assume_freq = TRUE
    )
    expect_s3_class(res_inc, "data.frame")
})
