library(testthat)
library(b3gbi)
library(sf)
library(dplyr)

# Test 1: Map output basic
test_that("spec_richness_density_map basic", {
    skip_on_cran()
    data(example_cube_1, package = "b3gbi")
    res <- spec_richness_density_map(example_cube_1, level = "country", region = "Denmark")
    expect_s3_class(res, "indicator_map")
    expect_true(any(!is.na(res$data$diversity_val)))
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
            num_bootstrap = 10
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
