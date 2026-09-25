# Taxonomic hierarchies for taxonomic distinctness -----------------------------
#
# Classifications are retrieved from GBIF with rgbif::name_backbone_checklist()
# in a single batched (asynchronous) request for all taxa in the cube, looked up
# by their taxon keys. Results are cached for the rest of the R session, so
# recalculating an indicator (e.g. map after time series, or with different
# settings) does not query GBIF again.

# GBIF checklist keys
gbif_checklists <- c(
  # GBIF Backbone Taxonomy: numeric taxon keys (e.g. 5219833)
  backbone = "d7dddbf4-2cf0-4f39-9b2a-bb099caae36c",
  # Catalogue of Life eXtended Release: alphanumeric taxon keys (e.g. "Q2M4")
  col_xr = "7ddf754f-d193-4cc9-b351-99906754a03b"
)

# The taxonomic ranks used for taxonomic distinctness (L = 7)
tax_ranks <- c("kingdom", "phylum", "class", "order", "family", "genus",
               "species")

# Session cache of retrieved classifications
b3gbi_tax_cache <- new.env(parent = emptyenv())

#' Check that rgbif is installed
#'
#' Wrapper so that it can be mocked in tests.
#' @noRd
check_rgbif_installed <- function() {
  rlang::check_installed(
    "rgbif",
    version = "3.7.0",
    reason = "to retrieve taxonomic classifications from GBIF for taxonomic distinctness."
  )
  invisible(TRUE)
}

#' Does the installed rgbif support lookups in a chosen checklist?
#'
#' rgbif >= 3.8.4 (GBIF API v2) accepts `checklistKey` and `usageKey`. Older
#' versions only match names against the GBIF Backbone.
#' @noRd
rgbif_supports_checklists <- function() {
  "checklistKey" %in% names(formals(rgbif::name_backbone_checklist))
}

#' Wrapper of rgbif::name_backbone_checklist() (for mocking in tests)
#' @noRd
my_name_backbone_checklist <- function(name_data, checklistKey, ...) {
  if (rgbif_supports_checklists()) {
    rgbif::name_backbone_checklist(name_data, checklistKey = checklistKey, ...)
  } else {
    rgbif::name_backbone_checklist(name_data, ...)
  }
}

#' Clear the session cache of taxonomic classifications
#'
#' Classifications retrieved from GBIF for taxonomic distinctness are cached
#' for the rest of the R session. Use this to force them to be retrieved again.
#'
#' @return Invisibly, the number of cached taxa that were removed.
#' @noRd
clear_taxonomy_cache <- function() {
  n <- length(ls(b3gbi_tax_cache))
  rm(list = ls(b3gbi_tax_cache), envir = b3gbi_tax_cache)
  invisible(n)
}

#' Retrieve the taxonomic hierarchy of the taxa in a cube
#'
#' @param x A data frame with a `taxonKey` column and, ideally, a
#'   `scientificName` column (used as a fallback when a key cannot be found).
#'   A `kingdom` column, if present, is used to disambiguate the fallback name
#'   matches.
#'
#' @return A data frame with one row per unique taxon key: `taxonKey`
#'   (character) and one column per rank in `tax_ranks`, holding the GBIF key
#'   of that rank (or its name when no key is available). Ranks that could not
#'   be resolved are `NA`.
#' @noRd
get_taxonomic_hierarchy <- function(x) {

  if (!"taxonKey" %in% names(x)) {
    stop("Taxonomic distinctness requires a 'taxonKey' column in the cube.")
  }

  taxa <- data.frame(taxonKey = as.character(x$taxonKey),
                     stringsAsFactors = FALSE)
  taxa$scientificName <- if ("scientificName" %in% names(x)) {
    as.character(x$scientificName)
  } else {
    NA_character_
  }
  taxa$kingdom <- if ("kingdom" %in% names(x)) {
    as.character(x$kingdom)
  } else {
    NA_character_
  }
  taxa <- taxa[!is.na(taxa$taxonKey) & !duplicated(taxa$taxonKey), ,
               drop = FALSE]

  empty <- as.data.frame(
    stats::setNames(
      rep(list(character(0)), length(tax_ranks) + 1),
      c("taxonKey", tax_ranks)
    ),
    stringsAsFactors = FALSE
  )
  if (nrow(taxa) == 0) return(empty)

  # Numeric keys belong to the GBIF Backbone, others to COL XR
  taxa$checklist <- ifelse(grepl("^[0-9]+$", taxa$taxonKey),
                           gbif_checklists[["backbone"]],
                           gbif_checklists[["col_xr"]])
  taxa$cache_id <- paste(taxa$checklist, taxa$taxonKey, sep = "|")

  to_fetch <- taxa[!vapply(taxa$cache_id, exists, logical(1),
                           envir = b3gbi_tax_cache, inherits = FALSE), ,
                   drop = FALSE]

  if (nrow(to_fetch) > 0) {
    check_rgbif_installed()
    message(sprintf(
      paste0("Retrieving the taxonomic classification of %d taxa from GBIF ",
             "(results are cached for the rest of this session)."),
      nrow(to_fetch)
    ))
    for (cl in unique(to_fetch$checklist)) {
      fetch_cl <- to_fetch[to_fetch$checklist == cl, , drop = FALSE]
      hier <- fetch_gbif_classification(fetch_cl, checklist = cl)
      for (i in seq_len(nrow(fetch_cl))) {
        assign(fetch_cl$cache_id[i], unlist(hier[i, tax_ranks]),
               envir = b3gbi_tax_cache)
      }
    }
  }

  out <- do.call(rbind, lapply(taxa$cache_id, function(id) {
    get(id, envir = b3gbi_tax_cache, inherits = FALSE)
  }))
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- tax_ranks
  out <- cbind(taxonKey = taxa$taxonKey, out, stringsAsFactors = FALSE)
  rownames(out) <- NULL

  n_missing <- sum(is.na(out$kingdom))
  if (n_missing > 0) {
    warning(sprintf(
      paste0("No GBIF classification was found for %d of %d taxa. They are ",
             "excluded from taxonomic distinctness."),
      n_missing, nrow(out)
    ), call. = FALSE)
  }

  out
}

#' Query GBIF for the classification of a set of taxa
#'
#' Looks up all taxa in one batched request by their taxon key. Taxa that are
#' not found by key are retried by scientific name (and kingdom, if known).
#'
#' @param taxa Data frame with `taxonKey`, `scientificName` and `kingdom`.
#' @param checklist GBIF checklist key.
#' @return Data frame with one row per taxon (same order) and one column per
#'   rank in `tax_ranks`.
#' @noRd
fetch_gbif_classification <- function(taxa, checklist) {

  by_key <- rgbif_supports_checklists()

  if (!by_key && checklist != gbif_checklists[["backbone"]]) {
    stop("This cube uses Catalogue of Life (COL XR) taxon keys, which ",
         "requires rgbif 3.8.4 or later. Please update rgbif.", call. = FALSE)
  }

  if (by_key) {
    res <- tryCatch(
      my_name_backbone_checklist(
        data.frame(usageKey = taxa$taxonKey, stringsAsFactors = FALSE),
        checklistKey = checklist
      ),
      error = function(e) {
        stop("Could not retrieve taxonomic classifications from GBIF: ",
             conditionMessage(e), call. = FALSE)
      }
    )
    hier <- parse_gbif_classification(res, n = nrow(taxa))
  } else {
    # Older rgbif: match all taxa by name against the GBIF Backbone
    hier <- parse_gbif_classification(NULL, n = nrow(taxa))
  }

  # Retry taxa without a result by scientific name
  retry <- which(is.na(hier$kingdom) & !is.na(taxa$scientificName))
  if (length(retry) > 0) {
    name_data <- data.frame(scientificName = taxa$scientificName[retry],
                            stringsAsFactors = FALSE)
    # Older rgbif versions expect the name column to be called 'name'
    # (newer ones 'scientificName'); using the expected name avoids a message
    if (!by_key) names(name_data)[1] <- "name"
    if (!all(is.na(taxa$kingdom[retry]))) {
      name_data$kingdom <- taxa$kingdom[retry]
    }
    res2 <- tryCatch(
      my_name_backbone_checklist(name_data, checklistKey = checklist),
      error = function(e) {
        if (!by_key) {
          stop("Could not retrieve taxonomic classifications from GBIF: ",
               conditionMessage(e), call. = FALSE)
        }
        NULL
      }
    )
    if (!is.null(res2)) {
      hier[retry, ] <- parse_gbif_classification(res2, n = length(retry))
    }
  }

  hier
}

#' Extract rank identifiers from rgbif::name_backbone_checklist() output
#'
#' Uses the key of each rank (e.g. `familyKey`) where available and falls back
#' to the rank's name. Rows without a match give `NA` for all ranks.
#'
#' @param res Output of `rgbif::name_backbone_checklist()` (one row per input).
#' @param n Expected number of rows.
#' @noRd
parse_gbif_classification <- function(res, n) {

  out <- as.data.frame(
    stats::setNames(rep(list(rep(NA_character_, n)), length(tax_ranks)),
                    tax_ranks),
    stringsAsFactors = FALSE
  )
  if (is.null(res) || nrow(res) != n) return(out)

  matched <- rep(TRUE, n)
  if ("matchType" %in% names(res)) {
    matched <- !is.na(res$matchType) & res$matchType != "NONE"
  }

  for (r in tax_ranks) {
    key_col <- paste0(r, "Key")
    val <- if (key_col %in% names(res)) as.character(res[[key_col]]) else NULL
    if (r %in% names(res)) {
      nm <- as.character(res[[r]])
      val <- if (is.null(val)) nm else ifelse(is.na(val), nm, val)
    }
    if (!is.null(val)) out[[r]] <- ifelse(matched, val, NA_character_)
  }

  out
}

#' Pairwise taxonomic distances between taxa
#'
#' The distance between two taxa is the number of taxonomic levels (out of
#' `L = length(tax_ranks)`) below their lowest shared rank: 1 for species in the
#' same genus, 2 for the same family, ..., 7 for different kingdoms. Ranks are
#' compared from the top down, so homonyms in different higher taxa are not
#' treated as shared.
#'
#' @param ids Data frame or matrix of rank identifiers (one row per taxon, one
#'   column per rank in `tax_ranks`, top-down).
#' @return A symmetric numeric matrix of distances.
#' @noRd
tax_distance_matrix <- function(ids) {
  ids <- as.matrix(ids)
  n <- nrow(ids)
  L <- ncol(ids)
  depth <- matrix(0, n, n)
  still_shared <- matrix(TRUE, n, n)
  for (r in seq_len(L)) {
    v <- ids[, r]
    both_na <- outer(is.na(v), is.na(v), "&")
    same <- outer(v, v, "==")
    same[is.na(same)] <- FALSE
    # A rank missing for both taxa (e.g. incertae sedis) does not break the
    # shared lineage, but does not add to it either
    still_shared <- still_shared & (same | both_na)
    depth[still_shared & same] <- r
  }
  d <- L - depth
  diag(d) <- 0
  d
}
