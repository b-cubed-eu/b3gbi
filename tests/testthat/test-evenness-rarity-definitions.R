# Hand-computed checks of the evenness and rarity definitions.
# Evenness: S is the number of species observed in the cell/year.
# Abundance-based rarity (ts): relative abundance calculated per year, each
#   species counted once per year.
# Area-based rarity (ts): occupancy calculated per year, rarity summed per cell,
#   then averaged over cells.

# --- Evenness ----------------------------------------------------------------

test_that("evenness ignores species absent from the cell/year", {
  x <- c(5, 5, 5)
  x_padded <- c(5, 5, 5, 0, 0, 0, 0)
  expect_equal(compute_evenness_formula(x, "pielou_evenness"), 1)
  expect_equal(compute_evenness_formula(x, "williams_evenness"), 1)
  expect_equal(compute_evenness_formula(x_padded, "pielou_evenness"), 1)
  expect_equal(compute_evenness_formula(x_padded, "williams_evenness"), 1)
})

test_that("evenness matches the published formulas", {
  x <- c(2, 2, 4, 0)
  p <- c(0.25, 0.25, 0.5)
  expect_equal(compute_evenness_formula(x, "pielou_evenness"),
               -sum(p * log(p)) / log(3))
  expect_equal(compute_evenness_formula(x, "williams_evenness"),
               1 - sqrt((3 * sum(p^2) - 1) / 2))
})

test_that("evenness is NA with fewer than two species", {
  expect_true(is.na(compute_evenness_formula(c(7, 0, 0), "pielou_evenness")))
  expect_true(is.na(compute_evenness_formula(c(7, 0, 0), "williams_evenness")))
})

test_that("calc_map_evenness_core uses per-cell species counts", {
  x <- data.frame(
    cellid = c(1, 1, 2, 2, 2),
    cellCode = c("A", "A", "B", "B", "B"),
    taxonKey = c(1, 2, 1, 3, 4),
    obs = c(5, 5, 2, 2, 4)
  )
  res <- calc_map_evenness_core(x, type = "pielou_evenness")
  p <- c(0.25, 0.25, 0.5)
  expect_equal(res$diversity_val[res$cellid == 1], 1)
  expect_equal(res$diversity_val[res$cellid == 2], -sum(p * log(p)) / log(3))
})

test_that("calc_ts_evenness_core uses per-year species counts", {
  x <- data.frame(
    year = c(2000, 2000, 2001, 2001, 2001),
    cellCode = "A",
    taxonKey = c(1, 2, 1, 3, 4),
    obs = c(5, 5, 2, 2, 4)
  )
  res <- calc_ts_evenness_core(x, type = "williams_evenness")
  p <- c(0.25, 0.25, 0.5)
  expect_equal(res$diversity_val[res$year == 2000], 1)
  expect_equal(res$diversity_val[res$year == 2001],
               1 - sqrt((3 * sum(p^2) - 1) / 2))
})

# --- Rarity ------------------------------------------------------------------

# Species 10 occurs in two cells in 2000; it must only be counted once.
rarity_cube <- data.frame(
  year = c(2000, 2000, 2000, 2000, 2001, 2001, 2001),
  cellid = c(1, 1, 2, 2, 1, 2, 3),
  taxonKey = c(10, 20, 10, 30, 10, 10, 20),
  obs = c(6, 2, 2, 2, 1, 1, 2)
)

test_that("calc_ts.ab_rarity uses yearly relative abundance, once per species", {
  x <- structure(rarity_cube, class = c("ab_rarity", "data.frame"))
  res <- calc_ts.ab_rarity(x)
  # 2000: totals 10 = 8, 20 = 2, 30 = 2 (N = 12) -> 12/8 + 12/2 + 12/2 = 13.5
  # 2001: totals 10 = 2, 20 = 2 (N = 4)          -> 4/2 + 4/2          = 4
  expect_equal(res$year, c(2000, 2001))
  expect_equal(res$diversity_val, c(13.5, 4))
})

test_that("calc_ts.area_rarity uses yearly occupancy and averages over cells", {
  x <- structure(rarity_cube[, c("year", "cellid", "taxonKey")],
                 class = c("area_rarity", "data.frame"))
  res <- calc_ts.area_rarity(x)
  # 2000: 2 cells; 10 in 2 cells (rarity 1), 20 and 30 in 1 cell (rarity 2)
  #       cell 1 = 1 + 2 = 3, cell 2 = 1 + 2 = 3 -> mean 3
  # 2001: 3 cells; 10 in 2 cells (1.5), 20 in 1 cell (3)
  #       cell 1 = 1.5, cell 2 = 1.5, cell 3 = 3 -> mean 2
  expect_equal(res$year, c(2000, 2001))
  expect_equal(res$diversity_val, c(3, 2))
})

test_that("rarity CIs are calculated around the corrected yearly values", {
  set.seed(1)
  x_ab <- structure(rarity_cube, class = c("ab_rarity", "data.frame"))
  ind_ab <- calc_ts.ab_rarity(x_ab)
  res_ab <- suppressWarnings(
    calc_ci.ab_rarity(x_ab, indicator = ind_ab, num_bootstrap = 50)
  )
  expect_true(all(c("ll", "ul") %in% names(res_ab)))
  expect_equal(res_ab$diversity_val, c(13.5, 4))

  x_area <- structure(rarity_cube[, c("year", "cellid", "taxonKey")],
                      class = c("area_rarity", "data.frame"))
  ind_area <- calc_ts.area_rarity(x_area)
  res_area <- suppressWarnings(
    calc_ci.area_rarity(x_area, indicator = ind_area, num_bootstrap = 50)
  )
  expect_true(all(c("ll", "ul") %in% names(res_area)))
  expect_equal(res_area$diversity_val, c(3, 2))
})

# --- Bootstrap helper -----------------------------------------------------------

test_that("drop_degenerate_bootstraps removes undefined groups", {
  good <- structure(list(t0 = 0.5, t = matrix(c(0.4, 0.6, 0.5))), class = "boot")
  bad <- structure(list(t0 = NA_real_, t = matrix(rep(NA_real_, 3))), class = "boot")
  const <- structure(list(t0 = 1, t = matrix(c(1, NA, 1))), class = "boot")
  res <- drop_degenerate_bootstraps(
    list(`2000` = good, `2001` = bad, `2002` = const), "year"
  )
  expect_equal(names(res), "2000")

  df <- data.frame(
    year = rep(c(2000, 2001), each = 3),
    est_original = rep(c(0.5, NA), each = 3),
    rep_boot = c(0.4, 0.6, 0.5, NA, NA, NA)
  )
  res_df <- drop_degenerate_bootstraps(df, "year")
  expect_equal(unique(res_df$year), 2000)
})

test_that("Williams' evenness is exactly 1 for perfectly even communities", {
  # Many equal abundances can give a tiny negative value from rounding
  for (k in 2:50) {
    expect_identical(compute_evenness_formula(rep(7, k), "williams_evenness"), 1)
    expect_equal(compute_evenness_formula(rep(7, k), "pielou_evenness"), 1)
  }
})
