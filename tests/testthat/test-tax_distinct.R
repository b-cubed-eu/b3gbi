# Taxonomic distinctness: GBIF lookup (mocked), distances and index ----------

# A small fake taxonomy (GBIF-style keys for each rank)
fake_taxonomy <- data.frame(
  usageKey = c("101", "102", "103", "104", "105"),
  scientificName = c("Vulpes vulpes", "Vulpes lagopus", "Meles meles",
                     "Turdus merula", "Quercus robur"),
  kingdom = c("Animalia", "Animalia", "Animalia", "Animalia", "Plantae"),
  phylum = c("Chordata", "Chordata", "Chordata", "Chordata", "Tracheophyta"),
  class = c("Mammalia", "Mammalia", "Mammalia", "Aves", "Magnoliopsida"),
  order = c("Carnivora", "Carnivora", "Carnivora", "Passeriformes", "Fagales"),
  family = c("Canidae", "Canidae", "Mustelidae", "Turdidae", "Fagaceae"),
  genus = c("Vulpes", "Vulpes", "Meles", "Turdus", "Quercus"),
  species = c("Vulpes vulpes", "Vulpes lagopus", "Meles meles",
              "Turdus merula", "Quercus robur"),
  kingdomKey = c("1", "1", "1", "1", "6"),
  phylumKey = c("44", "44", "44", "44", "7707728"),
  classKey = c("359", "359", "359", "212", "220"),
  orderKey = c("732", "732", "732", "729", "1354"),
  familyKey = c("9701", "9701", "5307", "6171", "4689"),
  genusKey = c("5219234", "5219234", "2433874", "2490717", "2877951"),
  speciesKey = c("101", "102", "103", "104", "105"),
  stringsAsFactors = FALSE
)

# Mimics rgbif::name_backbone_checklist(): one row per input
fake_name_backbone_checklist <- function(name_data, checklistKey, ...) {
  if ("usageKey" %in% names(name_data)) {
    idx <- match(name_data$usageKey, fake_taxonomy$usageKey)
  } else {
    nm <- if ("name" %in% names(name_data)) name_data$name else name_data$scientificName
    idx <- match(nm, fake_taxonomy$scientificName)
  }
  out <- fake_taxonomy[idx, , drop = FALSE]
  out$matchType <- ifelse(is.na(idx), "NONE", "EXACT")
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

expected_tdi <- function(d, L = 7) {
  n <- nrow(d)
  sum(d[upper.tri(d)]) / (L * n * (n - 1) / 2)
}

# --- Distances -----------------------------------------------------------------

test_that("tax_distance_matrix counts levels below the lowest shared rank", {
  ids <- fake_taxonomy[, tax_ranks]
  d <- tax_distance_matrix(ids)
  expect_equal(d[1, 2], 1) # same genus
  expect_equal(d[1, 3], 3) # same order (Carnivora)
  expect_equal(d[1, 4], 5) # same phylum (Chordata)
  expect_equal(d[1, 5], 7) # different kingdoms
  expect_equal(diag(d), rep(0, 5))
  expect_true(isSymmetric(d))
})

test_that("tax_distance_matrix does not treat homonyms as shared", {
  ids <- data.frame(
    kingdom = c("Animalia", "Plantae"), phylum = c("A", "B"),
    class = c("A", "B"), order = c("A", "B"), family = c("A", "B"),
    genus = c("Morus", "Morus"), species = c("Morus bassanus", "Morus nigra")
  )
  expect_equal(tax_distance_matrix(ids)[1, 2], 7)
})

test_that("ranks missing for both taxa do not break the shared lineage", {
  ids <- data.frame(
    kingdom = "K", phylum = "P", class = "C", order = NA, family = "F",
    genus = "G", species = c("s1", "s2")
  )
  expect_equal(tax_distance_matrix(ids)[1, 2], 1)
})

# --- Index -------------------------------------------------------------------

test_that("compute_tax_distinct_formula gives the Clarke & Warwick index", {
  hier <- data.frame(taxonKey = fake_taxonomy$usageKey,
                     fake_taxonomy[, tax_ranks], stringsAsFactors = FALSE)
  x <- data.frame(taxonKey = c(101, 102, 103, 104, 105, 101))
  d <- tax_distance_matrix(fake_taxonomy[, tax_ranks])
  expect_equal(compute_tax_distinct_formula(x, hier), expected_tdi(d))

  # Order does not matter; duplicates are ignored
  x2 <- data.frame(taxonKey = rev(x$taxonKey))
  expect_equal(compute_tax_distinct_formula(x2, hier), expected_tdi(d))

  # Three congeners/confamilials: (1 + 3 + 3) / (7 * 3)
  x3 <- data.frame(taxonKey = c(101, 102, 103))
  expect_equal(compute_tax_distinct_formula(x3, hier), 7 / 21)
})

test_that("compute_tax_distinct_formula needs three classified taxa", {
  hier <- data.frame(taxonKey = fake_taxonomy$usageKey,
                     fake_taxonomy[, tax_ranks], stringsAsFactors = FALSE)
  hier[hier$taxonKey == "103", tax_ranks] <- NA
  expect_true(is.na(compute_tax_distinct_formula(
    data.frame(taxonKey = c(101, 102)), hier)))
  # An unclassified taxon is excluded, leaving only two
  expect_true(is.na(compute_tax_distinct_formula(
    data.frame(taxonKey = c(101, 102, 103)), hier)))
})

# --- GBIF lookup -------------------------------------------------------------

test_that("parse_gbif_classification prefers keys and handles non-matches", {
  res <- fake_name_backbone_checklist(data.frame(usageKey = c("101", "999")))
  out <- parse_gbif_classification(res, n = 2)
  expect_equal(unname(unlist(out[1, ])),
               c("1", "44", "359", "732", "9701", "5219234", "101"))
  expect_true(all(is.na(out[2, ])))

  # Falls back to names when no keys are returned
  res_names <- res[, c("kingdom", "genus", "matchType")]
  out2 <- parse_gbif_classification(res_names, n = 2)
  expect_equal(out2$kingdom, c("Animalia", NA))
  expect_true(all(is.na(out2$family)))
})

test_that("get_taxonomic_hierarchy makes one request and caches results", {
  clear_taxonomy_cache()
  calls <- list()
  mock_checklist <- function(name_data, checklistKey, ...) {
    calls[[length(calls) + 1]] <<- list(n = nrow(name_data),
                                        checklist = checklistKey)
    fake_name_backbone_checklist(name_data, checklistKey)
  }
  x <- data.frame(taxonKey = c(101, 102, 103, 101, 104),
                  scientificName = fake_taxonomy$scientificName[c(1:3, 1, 4)])

  local_mocked_bindings(
    my_name_backbone_checklist = mock_checklist,
    check_rgbif_installed = function() invisible(TRUE),
    rgbif_supports_checklists = function() TRUE
  )
  expect_message(hier <- get_taxonomic_hierarchy(x), "4 taxa")
  expect_equal(length(calls), 1)
  expect_equal(calls[[1]]$n, 4)
  expect_equal(calls[[1]]$checklist, gbif_checklists[["backbone"]])
  expect_equal(hier$taxonKey, c("101", "102", "103", "104"))
  expect_equal(hier$family, c("9701", "9701", "5307", "6171"))

  # Second call: everything comes from the cache
  expect_no_message(hier2 <- get_taxonomic_hierarchy(x))
  expect_equal(length(calls), 1)
  expect_equal(hier2, hier)

  # Only new taxa are requested
  x_new <- data.frame(taxonKey = c(101, 105), scientificName = c("a", "b"))
  suppressMessages(get_taxonomic_hierarchy(x_new))
  expect_equal(length(calls), 2)
  expect_equal(calls[[2]]$n, 1)
  clear_taxonomy_cache()
})

test_that("get_taxonomic_hierarchy uses COL XR for alphanumeric keys", {
  clear_taxonomy_cache()
  used <- character(0)
  local_mocked_bindings(
    my_name_backbone_checklist = function(name_data, checklistKey, ...) {
      used <<- c(used, checklistKey)
      fake_name_backbone_checklist(name_data, checklistKey)
    },
    check_rgbif_installed = function() invisible(TRUE),
    rgbif_supports_checklists = function() TRUE
  )
  suppressWarnings(suppressMessages(
    get_taxonomic_hierarchy(data.frame(taxonKey = c("Q2M4", "101")))
  ))
  expect_setequal(used, unname(gbif_checklists))
  clear_taxonomy_cache()
})

test_that("taxa not found by key are retried by name, others warned about", {
  clear_taxonomy_cache()
  local_mocked_bindings(
    my_name_backbone_checklist = fake_name_backbone_checklist,
    check_rgbif_installed = function() invisible(TRUE),
    rgbif_supports_checklists = function() TRUE
  )
  x <- data.frame(taxonKey = c(101, 555, 556),
                  scientificName = c("Vulpes vulpes", "Meles meles", "Nonexistent"))
  expect_warning(
    hier <- suppressMessages(get_taxonomic_hierarchy(x)),
    "1 of 3 taxa"
  )
  expect_equal(hier$family, c("9701", "5307", NA))
  clear_taxonomy_cache()
})

test_that("GBIF errors give an informative message", {
  clear_taxonomy_cache()
  local_mocked_bindings(
    my_name_backbone_checklist = function(...) stop("Status: 0"),
    check_rgbif_installed = function() invisible(TRUE),
    rgbif_supports_checklists = function() TRUE
  )
  expect_error(
    suppressMessages(get_taxonomic_hierarchy(data.frame(taxonKey = 101))),
    "Could not retrieve taxonomic classifications from GBIF"
  )
})

test_that("older rgbif versions match by name against the GBIF Backbone", {
  clear_taxonomy_cache()
  args_seen <- list()
  local_mocked_bindings(
    my_name_backbone_checklist = function(name_data, checklistKey, ...) {
      args_seen[[length(args_seen) + 1]] <<- names(name_data)
      fake_name_backbone_checklist(name_data, checklistKey)
    },
    check_rgbif_installed = function() invisible(TRUE),
    rgbif_supports_checklists = function() FALSE
  )
  x <- data.frame(taxonKey = c(101, 103, 104),
                  scientificName = c("Vulpes vulpes", "Meles meles",
                                     "Turdus merula"))
  hier <- suppressMessages(get_taxonomic_hierarchy(x))
  expect_equal(length(args_seen), 1)
  expect_equal(args_seen[[1]], "name")
  expect_equal(hier$family, c("9701", "5307", "6171"))

  # COL XR keys cannot be handled by older rgbif versions
  expect_error(
    suppressMessages(get_taxonomic_hierarchy(data.frame(taxonKey = "Q2M4"))),
    "requires rgbif 3.8.4"
  )
  clear_taxonomy_cache()
})

# --- Indicator calculation -----------------------------------------------------

tax_cube <- data.frame(
  year = c(2000, 2000, 2000, 2001, 2001, 2001, 2001),
  cellid = c(1, 1, 1, 2, 2, 2, 2),
  cellCode = c("A", "A", "A", "B", "B", "B", "B"),
  taxonKey = c(101, 102, 103, 101, 103, 104, 105),
  scientificName = fake_taxonomy$scientificName[c(1, 2, 3, 1, 3, 4, 5)]
)

test_that("calc_map.tax_distinct and calc_ts.tax_distinct use the index", {
  clear_taxonomy_cache()
  local_mocked_bindings(
    my_name_backbone_checklist = fake_name_backbone_checklist,
    check_rgbif_installed = function() invisible(TRUE),
    rgbif_supports_checklists = function() TRUE
  )
  d <- tax_distance_matrix(fake_taxonomy[, tax_ranks])
  tdi_b <- expected_tdi(d[c(1, 3, 4, 5), c(1, 3, 4, 5)])

  x_map <- structure(tax_cube, class = c("tax_distinct", "data.frame"))
  res_map <- suppressMessages(calc_map.tax_distinct(x_map))
  expect_named(res_map, c("cellid", "cellCode", "diversity_val"))
  expect_equal(res_map$diversity_val, c(7 / 21, tdi_b))

  x_ts <- structure(tax_cube, class = c("tax_distinct", "data.frame"))
  res_ts <- calc_ts.tax_distinct(x_ts)
  expect_equal(res_ts$year, c(2000, 2001))
  expect_equal(res_ts$diversity_val, c(7 / 21, tdi_b))
  clear_taxonomy_cache()
})

test_that("calc_map.tax_distinct handles empty input", {
  empty <- structure(
    tibble::tibble(cellid = integer(), cellCode = character(),
                   taxonKey = character(), scientificName = character()),
    class = c("tax_distinct", "data.frame")
  )
  res <- calc_map.tax_distinct(empty)
  expect_equal(nrow(res), 0)
  expect_named(res, c("cellid", "cellCode", "diversity_val"))
})

test_that("the 'rows' argument of the wrappers is deprecated", {
  local_mocked_bindings(
    compute_indicator_workflow = function(...) "ok",
    check_rgbif_installed = function() invisible(TRUE),
    rgbif_supports_checklists = function() TRUE
  )
  expect_warning(tax_distinct_map(NULL, rows = 2), "deprecated")
  expect_warning(tax_distinct_ts(NULL, rows = 2), "deprecated")
  expect_no_warning(tax_distinct_ts(NULL))
})

# --- Live check against GBIF (not run on CRAN) ---------------------------------

test_that("classifications can be retrieved from GBIF", {
  skip_on_cran()
  skip_if_not_installed("rgbif", minimum_version = "3.7.0")
  skip_if_offline("api.gbif.org")
  clear_taxonomy_cache()
  # Micromys minutus and Myotis daubentonii (GBIF Backbone keys, both in
  # example_cube_1); Animalia has backbone key 1
  hier <- suppressMessages(get_taxonomic_hierarchy(
    data.frame(taxonKey = c(5219833, 2432439),
               scientificName = c("Micromys minutus", "Myotis daubentonii"))
  ))
  expect_equal(hier$kingdom, c("1", "1"))
  expect_false(anyNA(hier$family))
  expect_false(hier$order[1] == hier$order[2]) # Rodentia vs Chiroptera
  clear_taxonomy_cache()
})
