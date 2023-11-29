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


# Copy of wrap_plots from patchwork package
#' @noRd
wrap_plots_int <- function (..., ncol = NULL, nrow = NULL, byrow = NULL, widths = NULL,
          heights = NULL, guides = NULL, tag_level = NULL, design = NULL)
{
  if (is_valid_plot(..1)) {
    plots <- list(...)
  }
  else if (is.list(..1)) {
    plots <- ..1
  }
  else {
    cli_abort("Can only wrap {.cls ggplot} and/or {.cls grob} objects or a list of them")
  }
  if (!all(vapply(plots, is_valid_plot, logical(1))))
    cli_abort("Only know how to add {.cls ggplot} and/or {.cls grob} objects")
  if (!is.null(names(plots)) && !is.null(design) && is.character(design)) {
    area_names <- unique(trimws(strsplit(design, "")[[1]]))
    area_names <- sort(setdiff(area_names, c("", "#")))
    if (all(names(plots) %in% area_names)) {
      plot_list <- vector("list", length(area_names))
      names(plot_list) <- area_names
      plot_list[names(plots)] <- plots
      plot_list[vapply(plot_list, is.null, logical(1))] <- list(plot_spacer())
      plots <- plot_list
    }
  }
  Reduce(`+`, plots, init = plot_filler()) + plot_layout(ncol = ncol,
                                                         nrow = nrow, byrow = byrow, widths = widths, heights = heights,
                                                         guides = guides, tag_level = tag_level, design = design)
}

# Copy of internal function plot_filler from patchwork package
#' @noRd
plot_filler <- function ()
{
  p <- ggplot()
  class(p) <- c("plot_filler", class(p))
  p
}

# Copy of function plot_layout from patchwork package
#' @noRd
plot_layout <- function (ncol = NULL, nrow = NULL, byrow = NULL, widths = NULL,
          heights = NULL, guides = NULL, tag_level = NULL, design = NULL)
{
  if (!is.null(guides))
    guides <- match.arg(guides, c("auto", "collect", "keep"))
  if (!is.null(tag_level))
    tag_level <- match.arg(tag_level, c("keep", "new"))
  structure(list(ncol = ncol, nrow = nrow, byrow = byrow,
                 widths = widths, heights = heights, guides = guides,
                 tag_level = tag_level, design = as_areas(design)), class = "plot_layout")
}

# Copy of internal function as_areas from patchwork package
#' @noRd
as_areas <- function (x)
{
  if (is.null(x))
    return(NULL)
  if (is_area(x))
    return(x)
  if (!is.character(x)) {
    cli_abort("Don't know how to convert {.cls {class(x)}} into area positions")
  }
  x <- strsplit(x, split = "\n")[[1]]
  x <- lapply(x, trimws)
  if (identical(x[[1]], ""))
    x[1] <- NULL
  if (identical(x[[length(x)]], ""))
    x[length(x)] <- NULL
  x <- lapply(x, function(x) strsplit(x, "")[[1]])
  ncols <- vapply(x, length, integer(1))
  if (length(unique(ncols)) != 1) {
    cli_abort("character layout must be rectangular")
  }
  row <- rep(seq_along(x), each = ncols[1])
  col <- rep(seq_len(ncols[1]), length(x))
  x <- unlist(x)
  area_names <- unique(sort(x))
  area_names[area_names == "#"] <- NA
  x <- match(x, area_names)
  exec(c, !!!lapply(split(seq_along(x), x), function(i) {
    if (is.na(x[i[1]]))
      return(area())
    area_rows <- range(row[i])
    area_cols <- range(col[i])
    if (!all(x[row >= area_rows[1] & row <= area_rows[2] &
               col >= area_cols[1] & col <= area_cols[2]] == x[i[1]])) {
      cli_abort("Patch areas must be rectangular")
    }
    area(area_rows[1], area_cols[1], area_rows[2], area_cols[2])
  }))
}


# Copy of internal function is_valid_plot from patchwork package
#' @noRd
is_valid_plot <- function (x)
{
  is.ggplot(x) || is.grob(x)
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

# Copy of function gifski from gifski package
# Gifski converts image frames to high quality GIF animations.
#' @noRd
#' @useDynLib b3gbi R_png_to_gif
gifski_int <- function (png_files, gif_file = "animation.gif", width = 800,
          height = 600, delay = 1, loop = TRUE, progress = TRUE)
{
  png_files <- normalizePath(png_files, mustWork = TRUE)
  gif_file <- normalizePath(gif_file, mustWork = FALSE)
  if (!file.exists(dirname(gif_file)))
    stop("Target directory does not exist:", dirname(gif_file))
  width <- as.integer(width)
  height <- as.integer(height)
  delay <- as.numeric(delay)
  repeats <- if (is.logical(loop) || loop == 0) {
    isTRUE(loop) - 1
  }
  else {
    as.integer(loop)
  }
  progress <- as.logical(progress)
  .Call(R_png_to_gif, enc2utf8(png_files), enc2utf8(gif_file),
        width, height, delay, repeats, progress)
}

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
      result[, i] <- suppressWarnings(rarefy(t(freq),
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

