## ------------------------------------------------------------------
## Helper transformations
## ------------------------------------------------------------------
logit <- function(p) {
  log(p / (1 - p))
}

inv_logit <- function(l) {
  exp(l) / (1 + exp(l))
}

## ------------------------------------------------------------------
## Indicator objects for unit tests
## ------------------------------------------------------------------
indicator_total_occ <- list(div_type = "total_occ", raw_data = iris)
indicator_pielou_evenness <- list(div_type = "pielou_evenness", raw_data = iris)
indicator_williams_evenness <- list(div_type = "williams_evenness", raw_data = iris)
indicator_occ_density <- list(div_type = "occ_density", raw_data = iris)
indicator_ab_rarity <- list(div_type = "ab_rarity", raw_data = iris)
indicator_area_rarity <- list(div_type = "area_rarity", raw_data = iris)
indicator_newness <- list(div_type = "newness", raw_data = iris)
indicator_spec_occ <- list(div_type = "spec_occ", raw_data = iris)
indicator_spec_range <- list(div_type = "spec_range", raw_data = iris)
indicator_occ_ts <- list(div_type = "occ_ts", raw_data = iris)

## ------------------------------------------------------------------
## Expected behaviour per div_type
## ------------------------------------------------------------------
indicator_expectations <- list(
  total_occ = list(
    indicator = indicator_total_occ,
    method = "group_specific",
    grouping = "year",
    trans = function(t) t,
    inv_trans = function(t) t,
    no_bias = TRUE
  ),
  pielou_evenness = list(
    indicator = indicator_pielou_evenness,
    method = "boot_whole_cube",
    grouping = "year",
    trans = logit,
    inv_trans = inv_logit,
    no_bias = FALSE
  ),
  williams_evenness = list(
    indicator = indicator_williams_evenness,
    method = "boot_whole_cube",
    grouping = "year",
    trans = logit,
    inv_trans = inv_logit,
    no_bias = FALSE
  ),
  occ_density = list(
    indicator = indicator_occ_density,
    method = "boot_whole_cube",
    grouping = "year",
    trans = function(t) t,
    inv_trans = function(t) t,
    no_bias = FALSE
  ),
  ab_rarity = list(
    indicator = indicator_ab_rarity,
    method = "boot_whole_cube",
    grouping = "year",
    trans = function(t) t,
    inv_trans = function(t) t,
    no_bias = FALSE
  ),
  area_rarity = list(
    indicator = indicator_area_rarity,
    method = "boot_whole_cube",
    grouping = "year",
    trans = function(t) t,
    inv_trans = function(t) t,
    no_bias = FALSE
  ),
  newness = list(
    indicator = indicator_newness,
    method = "boot_whole_cube",
    grouping = "year",
    trans = function(t) t,
    inv_trans = function(t) t,
    no_bias = FALSE
  ),
  spec_occ = list(
    indicator = indicator_spec_occ,
    method = "group_specific",
    grouping = c("year", "taxonKey"),
    trans = function(t) t,
    inv_trans = function(t) t,
    no_bias = FALSE
  ),
  spec_range = list(
    indicator = indicator_spec_range,
    method = "group_specific",
    grouping = c("year", "taxonKey"),
    trans = function(t) t,
    inv_trans = function(t) t,
    no_bias = FALSE
  )
)

test_that("prepare_indicator_bootstrap returns correct params for all div_types", {
  for (div_type in names(indicator_expectations)) {

    exp <- indicator_expectations[[div_type]]

    params <- prepare_indicator_bootstrap(
      indicator = exp$indicator,
      num_bootstrap = 1000,
      ci_type = c("perc", "bca"),
      confidence_level = 0.9
    )

    # ---- bootstrap params ----
    expect_equal(
      params$bootstrap_params$samples,
      1000,
      info = paste("bootstrap samples for", div_type)
    )

    expect_identical(
      params$bootstrap_params$method,
      exp$method,
      info = paste("bootstrap method for", div_type)
    )

    expect_identical(
      params$bootstrap_params$grouping_var,
      exp$grouping,
      info = paste("grouping_var for", div_type)
    )

    ## ---- CI params ----
    expect_equal(
      sort(params$ci_params$type),
      sort(c("perc", "bca")),
      info = paste("confidence intervals for", div_type)
    )

    expect_equal(
      params$ci_params$conf,
      0.9,
      info = paste("confidence level for", div_type)
    )

    expect_identical(
      params$ci_params$no_bias,
      exp$no_bias,
      info = paste("no_bias for", div_type)
    )

    x <- 0.3
    expect_equal(
      params$ci_params$h(x),
      exp$trans(x),
      info = paste("transformation behaviour for", div_type)
    )

    y <- 0.2
    expect_equal(
      params$ci_params$hinv(y),
      exp$inv_trans(y),
      info = paste("inverse transformation behaviour for", div_type)
    )
  }
})

test_that("", {
  params <- prepare_indicator_bootstrap(
    indicator = indicator_expectations[["total_occ"]]$indicator,
    num_bootstrap = 1000,
    ci_type = c("perc", "bca"),
    confidence_level = 0.9,
    ci_args = list(no_bias = FALSE)
  )

  expect_identical(
    params$bootstrap_params$method,
    "boot_group_specific"
  )

  expect_identical(
    params$ci_params$no_bias,
    FALSE
  )
})

