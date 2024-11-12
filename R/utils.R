# alternative sampling function that works properly even with length of 1
#' @noRd
resample <- function(x, size, replace = TRUE) {

  if (length(x) == 1) {

    return(rep(x, size))

  } else {

    return(sample(x, size, replace = replace))

  }

}

# Function to add missing year values into bootstrap results
# boot is the bootstrap output, and orig_data is the original data used to
# calculate the bootstraps (a data frame with a year column arranged in order)
#' @noRd
add_yearvals_to_boot <- function(boot, orig_data) {
  names(boot) <- unique(orig_data$year)[2:length(unique(orig_data$year))]
  return(boot)
}


# Copy of stopifnot_error from iregnet package
#' @title stopifnot with custom error message
#'
#' @description
#' Like \code{stopifnot}, but with a custom error message.
#'
#' @param err_message The error message to print.
#' @param ... An error is raised if any these expressions is \code{FALSE}.
#'
#' @noRd
stopifnot_error <- function(err_message, ...)
{
  n <- length(ll <- list(...))
  for (i in 1:n)
    if (!(is.logical(r <- ll[[i]]) && !anyNA(r) && all(r))) {
      stop(err_message)
    }
}

# Copy of breaks_log from scales package
#' @noRd
breaks_log_int <- function (n = 5, base = 10)
{
  force_all(n, base)
  n_default <- n
  function(x, n = n_default) {
    raw_rng <- suppressWarnings(range(x, na.rm = TRUE))
    if (any(!is.finite(raw_rng))) {
      return(numeric())
    }
    rng <- log(raw_rng, base = base)
    min <- floor(rng[1])
    max <- ceiling(rng[2])
    if (max == min) {
      return(base^min)
    }
    by <- floor((max - min)/n) + 1
    breaks <- base^seq(min, max, by = by)
    relevant_breaks <- base^rng[1] <= breaks & breaks <=
      base^rng[2]
    if (sum(relevant_breaks) >= (n - 2)) {
      return(breaks)
    }
    while (by > 1) {
      by <- by - 1
      breaks <- base^seq(min, max, by = by)
      relevant_breaks <- base^rng[1] <= breaks & breaks <=
        base^rng[2]
      if (sum(relevant_breaks) >= (n - 2)) {
        return(breaks)
      }
    }
    log_sub_breaks(rng, n = n, base = base)
  }
}

# Copy of breaks_pretty from scales package
#' @noRd
breaks_pretty_int <- function (n = 5, ...)
{
  force_all(n, ...)
  n_default <- n
  function(x, n = n_default) {
    breaks <- pretty(x, n, ...)
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
}

# Copy of internal function force_all from scales package
#' @noRd
force_all <- function (...)
{
  list(...)
}

# Copy of internal function log_sub_breaks from scales package
#' @noRd
log_sub_breaks <- function (rng, n = 5, base = 10)
{
  min <- floor(rng[1])
  max <- ceiling(rng[2])
  if (base <= 2) {
    return(base^(min:max))
  }
  steps <- 1
  delta <- function(x) {
    min(diff(log(sort(c(x, steps, base)), base = base)))
  }
  candidate <- seq_len(base)
  candidate <- candidate[1 < candidate & candidate < base]
  while (length(candidate)) {
    best <- which.max(vapply(candidate, delta, 0))
    steps <- c(steps, candidate[best])
    candidate <- candidate[-best]
    breaks <- as.vector(outer(base^seq(min, max), steps))
    relevant_breaks <- base^rng[1] <= breaks & breaks <=
      base^rng[2]
    if (sum(relevant_breaks) >= (n - 2)) {
      break
    }
  }
  if (sum(relevant_breaks) >= (n - 2)) {
    breaks <- sort(breaks)
    lower_end <- pmax(min(which(base^rng[1] <= breaks)) -
                        1, 1)
    upper_end <- pmin(max(which(breaks <= base^rng[2])) +
                        1, length(breaks))
    breaks[lower_end:upper_end]
  }
  else {
    extended_breaks(n = n)(base^rng)
  }
}

# Copy of internal function cli_abort from cli package
#' @noRd
cli_abort <- function (message, ..., call = .envir, .envir = parent.frame(),
          .frame = .envir)
{
  message[] <- vcapply(message, format_inline, .envir = .envir)
  rlang::abort(message, ..., call = call, use_cli_format = TRUE,
               .frame = .frame)
}

# Copy of internal function exec from rlang package
#' @noRd
exec <- function (.fn, ..., .env = caller_env())
{
  .External2(ffi_exec, .fn, .env)
}

# Copy of internal function is.grob from grid package
#' @noRd
is.grob <- function (x)
{
  inherits(x, "grob")
}

# Copy of plot_annotation from patchwork package
#' @noRd
plot_annotation_int <- function (title = NULL, subtitle = NULL, caption = NULL, tag_levels = NULL,
          tag_prefix = NULL, tag_suffix = NULL, tag_sep = NULL, theme = NULL)
{
  th <- if (is.null(theme))
    ggplot2::theme()
  else theme
  structure(list(title = title, subtitle = subtitle, caption = caption,
                 tag_levels = tag_levels, tag_prefix = tag_prefix, tag_suffix = tag_suffix,
                 tag_sep = tag_sep, theme = th), class = "plot_annotation")
}

# # Copy of function gifski from gifski package
# # Gifski converts image frames to high quality GIF animations.
# #' @noRd
# #' @useDynLib b3gbi R_png_to_gif
# gifski_int <- function (png_files, gif_file = "animation.gif", width = 800,
#           height = 600, delay = 1, loop = TRUE, progress = TRUE)
# {
#   png_files <- normalizePath(png_files, mustWork = TRUE)
#   gif_file <- normalizePath(gif_file, mustWork = FALSE)
#   if (!file.exists(dirname(gif_file)))
#     stop("Target directory does not exist:", dirname(gif_file))
#   width <- as.integer(width)
#   height <- as.integer(height)
#   delay <- as.numeric(delay)
#   repeats <- if (is.logical(loop) || loop == 0) {
#     isTRUE(loop) - 1
#   }
#   else {
#     as.integer(loop)
#   }
#   progress <- as.logical(progress)
#   .Call(R_png_to_gif, enc2utf8(png_files), enc2utf8(gif_file),
#         width, height, delay, repeats, progress)
#u }

# Copy of function specaccum from vegan package
#' @noRd
specaccum_int <- function (comm, method = "exact", permutations = 100, conditioned = TRUE,
          gamma = "jack1", w = NULL, subset, ...)
{
  METHODS <- c("collector", "random", "exact", "rarefaction",
               "coleman")
  method <- match.arg(method, METHODS)
  if (!is.null(w) && !(method %in% c("random", "collector")))
    stop("weights 'w' can be only used with methods 'random' and 'collector'")
  if (!missing(subset)) {
    comm <- subset(comm, subset)
    w <- subset(w, subset)
  }
  x <- comm
  x <- as.matrix(x)
  x <- x[, colSums(x) > 0, drop = FALSE]
  n <- nrow(x)
  p <- ncol(x)
  accumulator <- function(x, ind) {
    rowSums(apply(x[ind, , drop = FALSE], 2, cumsum) > 0)
  }
  specaccum <- sdaccum <- sites <- perm <- NULL
  if (n == 1 && method != "rarefaction")
    message("no actual accumulation since only one site provided")
  switch(method, collector = {
    sites <- seq_len(n)
    xout <- weights <- cumsum(w)
    specaccum <- accumulator(x, sites)
    perm <- as.matrix(specaccum)
    weights <- as.matrix(weights)
  }, random = {
    permat <- getPermuteMatrix(permutations, n)
    perm <- apply(permat, 1, accumulator, x = x)
    if (!is.null(w)) weights <- as.matrix(apply(permat,
                                                1, function(i) cumsum(w[i])))
    sites <- seq_len(n)
    if (is.null(w)) {
      specaccum <- apply(perm, 1, mean)
      sdaccum <- apply(perm, 1, sd)
    } else {
      sumw <- sum(w)
      xout <- seq(sumw/n, sumw, length.out = n)
      intx <- sapply(seq_len(NCOL(perm)), function(i) approx(weights[,
                                                                     i], perm[, i], xout = xout)$y)
      specaccum <- apply(intx, 1, mean)
      sdaccum <- apply(intx, 1, sd)
    }
  }, exact = {
    freq <- colSums(x > 0)
    freq <- freq[freq > 0]
    f <- length(freq)
    ldiv <- lchoose(n, 1:n)
    result <- array(dim = c(n, f))
    for (i in 1:n) {
      result[i, ] <- ifelse(n - freq < i, 0, exp(lchoose(n -
                                                           freq, i) - ldiv[i]))
    }
    sites <- 1:n
    specaccum <- rowSums(1 - result)
    if (conditioned) {
      V <- result * (1 - result)
      tmp1 <- cor(x > 0)
      ind <- lower.tri(tmp1)
      tmp1 <- tmp1[ind]
      tmp1[is.na(tmp1)] <- 0
      cv <- numeric(n)
      for (i in 1:n) {
        tmp2 <- outer(sqrt(V[i, ]), sqrt(V[i, ]))[ind]
        cv[i] <- 2 * sum(tmp1 * tmp2)
      }
      V <- rowSums(V)
      sdaccum <- sqrt(V + cv)
    } else {
      Stot <- specpool(x)[, gamma]
      sdaccum1 <- rowSums((1 - result)^2)
      sdaccum2 <- specaccum^2/Stot
      sdaccum <- sqrt(sdaccum1 - sdaccum2)
    }
  }, rarefaction = {
    minobs <- min(x[x > 0])
    if (minobs > 1) warning(gettextf("most observed count data have counts 1, but smallest count is %d",
                                     minobs))
    freq <- colSums(x)
    freq <- freq[freq > 0]
    tot <- sum(freq)
    ind <- round(seq(tot/n, tot, length = n))
    result <- matrix(NA, nrow = 2, ncol = n)
    for (i in 1:n) {
      result[, i] <- suppressWarnings(rarefy_int(t(freq),
                                             ind[i], se = TRUE))
    }
    specaccum <- result[1, ]
    sdaccum <- result[2, ]
    sites <- ind/tot * n
  }, coleman = {
    freq <- colSums(x > 0)
    result <- array(dim = c(n, p))
    for (i in 1:n) {
      result[i, ] <- (1 - i/n)^freq
    }
    result <- 1 - result
    sites <- seq_len(n)
    specaccum <- rowSums(result)
    sdaccum <- sqrt(rowSums(result * (1 - result)))
  })
  out <- list(call = match.call(), method = method, sites = sites,
              richness = specaccum, sd = sdaccum, perm = perm)
  if (!is.null(w)) {
    out$weights <- weights
    out$effort <- xout
  }
  if (method == "rarefaction")
    out$individuals <- ind
  if (method %in% c("exact", "rarefaction", "coleman"))
    out$freq <- freq
  if (method == "random")
    attr(out, "control") <- attr(permat, "control")
  class(out) <- "specaccum"
  out
}

# Copy of internal function getPermuteMatrix from vegan package
#' @noRd
getPermuteMatrix <- function (perm, N, strata = NULL)
{
  if (length(perm) == 1) {
    perm <- permute::how(nperm = perm)
  }
  if (!missing(strata) && !is.null(strata)) {
    if (inherits(perm, "how") && is.null(permute::getBlocks(perm)))
      permute::setBlocks(perm) <- strata
  }
  if (inherits(perm, "how"))
    perm <- permute::shuffleSet(N, control = perm)
  else {
    if (!is.integer(perm) && !all(perm == round(perm)))
      stop("permutation matrix must be strictly integers: use round()")
  }
  if (is.null(attr(perm, "control")))
    attr(perm, "control") <- structure(list(within = list(type = "supplied matrix"),
                                            nperm = nrow(perm)), class = "how")
  perm
}

# Copy of function rarefy from vegan package
#' @noRd
rarefy_int <- function (x, sample, se = FALSE, MARGIN = 1)
{
  x <- as.matrix(x)
  if (ncol(x) == 1 && MARGIN == 1)
    x <- t(x)
  if (!identical(all.equal(x, round(x)), TRUE))
    stop("function accepts only integers (counts)")
  minobs <- min(x[x > 0])
  if (minobs > 1)
    warning(gettextf("most observed count data have counts 1, but smallest count is %d",
                     minobs))
  minsample <- min(apply(x, MARGIN, sum))
  if (missing(sample)) {
    stop(gettextf("the size of 'sample' must be given --\nHint: Smallest site maximum %d",
                  minsample))
  }
  if (any(sample > minsample))
    warning(gettextf("requested 'sample' was larger than smallest site maximum (%d)",
                     minsample))
  rarefun <- function(x, sample) {
    x <- x[x > 0]
    J <- sum(x)
    ldiv <- lchoose(J, sample)
    p1 <- ifelse(J - x < sample, 0, exp(lchoose(J - x, sample) -
                                          ldiv))
    out <- sum(1 - p1)
    if (se) {
      V <- sum(p1 * (1 - p1))
      Jxx <- J - outer(x, x, "+")
      ind <- lower.tri(Jxx)
      Jxx <- Jxx[ind]
      V <- V + 2 * sum(ifelse(Jxx < sample, 0, exp(lchoose(Jxx,
                                                           sample) - ldiv)) - outer(p1, p1)[ind])
      out <- cbind(out, sqrt(max(V, 0)))
    }
    out
  }
  if (length(sample) > 1) {
    S.rare <- sapply(sample, function(n) apply(x, MARGIN,
                                               rarefun, sample = n))
    S.rare <- matrix(S.rare, ncol = length(sample))
    colnames(S.rare) <- paste("N", sample, sep = "")
    if (se) {
      dn <- unlist(dimnames(x)[MARGIN])
      rownames(S.rare) <- paste(rep(dn, each = 2), c("S",
                                                     "se"), sep = ".")
    }
  }
  else {
    S.rare <- apply(x, MARGIN, rarefun, sample = sample)
    if (se)
      rownames(S.rare) <- c("S", "se")
  }
  attr(S.rare, "Subsample") <- sample
  S.rare
}


# Copy of function meters_to_decdeg from the occUncertain github repository:
# https://github.com/mlammens/occUncertain
# Documentation below also copied from occUncertain repository.
#
#' @title Convert from meters to degrees correcting for global position
#'
#' @description
#' \code{meters_to_decdeg} converts from meters to degrees at a specified
#' position on the globe. The use case this function was developed for was to
#' calculate occurrence point uncertainty values, which are usually reported in
#' meters, as degrees.
#'
#' The formula for converting from meters to decimal degrees is in part
#' based on information from the ESRI ArcUser magazine at this site
#' \url{https://www.esri.com/news/arcuser/0400/wdside.html}
#'
#' @param occs_df A \code{data.frame} of occurrence locations that incudes
#'   \emph{at least these three columns} - latitude, longitude, and a distance
#'   in meters to be converted to decimal degrees.
#' @param lat_col Name of column of latitude values. Caps sensitive.
#' @param lon_col Name of column of longitude values. Caps sensitive.
#' @param distance Name of column of distance values, in meters. Caps sensitive.
#' @param na_action Enact distance options for NA values. Caps sensitive
#' @return dist_dd A \code{data.frame} of latitude and longitude distances in
#'   units of degree decimal.
#'
#' @noRd
meters_to_decdeg <- function(occs_df, lat_col = "latitude",
                             lon_col = "longitude", distance, na_action = "NA as 0") {
  lat <- occs_df[[lat_col]]
  lon <- occs_df[[lon_col]]
  dist <- occs_df[[distance]]
  #enact distance options for NA
  # treat NA as 0
  # treat NA as mean dist
  # treat NA as NA
  if (na_action == "NA as 0") {
    dist <- ifelse (is.na(dist), yes = 0, no = dist )
  } else if (na_action == "NA as mean") {
    dist <- ifelse (is.na(dist), yes = mean(dist, na.rm = TRUE), no = dist )
  } else if (na_action == "NA as NA" ) {
    dist = dist
  } else {
    warning("Incorrect na_action chosen")
    return(0)
  }

  #each degree the radius line of the Earth corresponds to 111139 meters.
  lat_uncertainty <- dist/111325
  #at the equator, longitude approx equals latitude
  #decrease in a trigonometric cosine-based fashion as one moves toward the earth's poles
  lon_uncertainty <- dist / (111325  * cos(lat * (pi / 180)))
  dist_dd <- data.frame(lon_uncertainty = lon_uncertainty,
                        lat_uncertainty = lat_uncertainty)
  return(dist_dd)
}
