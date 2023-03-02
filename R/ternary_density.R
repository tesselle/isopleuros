# DENSITY
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_density
#' @aliases ternary_density,numeric,numeric,numeric-method
setMethod(
  f = "ternary_density",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, h = NULL, n = 25, nlevels = 10, levels = NULL,
                        col = graphics::par("fg"), lty = graphics::par("lty"),
                        lwd = graphics::par("lwd"), ...) {
    ## ILR
    coda <- cbind(x, y, z)
    ratio <- ilr(coda)

    ## Compute KDE
    lims <- range(ratio)
    lims <- lims + c(-1, 1) * (diff(lims) * 0.2) # Expand range
    dens <- kde(
      x = ratio[, 1],
      y = ratio[, 2],
      h = h,
      n = n,
      lims = c(lims, lims) # x and y range should be same
    )

    ## Compute contours
    xy <- grDevices::contourLines(
      x = dens$x,
      y = dens$y,
      z = dens$z,
      nlevels = nlevels,
      levels = levels %||% pretty(range(dens$z, na.rm = TRUE), nlevels)
    )

    ## Map colors
    if (length(col) == 1) col <- rep(col, length(xy))
    if (length(col) < length(xy)) {
      k <- vapply(
        X = xy,
        FUN = getElement,
        FUN.VALUE = numeric(1),
        name = "level"
      )
      k <- (k - min(k)) / (max(k) - min(k))
      r <- grDevices::colorRamp(col)(k)
      col <- grDevices::rgb(r[, 1], r[, 2], r[, 3], maxColorValue = 255)
    }

    for (i in seq_along(xy)) {
      level <- xy[[i]]

      ## Inverse ILR transform
      tern <- ilr_inv(cbind(level$x, level$y))

      ## Plot ternary lines
      ternary_lines(tern, col = col[[i]], lty = lty, lwd = lwd, ...)
    }
  }
)

#' @export
#' @rdname ternary_density
#' @aliases ternary_density,ANY,missing,missing-method
setMethod(
  f = "ternary_density",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, h = NULL, n = 25, nlevels = 10, levels = NULL,
                        col = graphics::par("fg"), lty = graphics::par("lty"),
                        lwd = graphics::par("lwd"), ...) {
    x <- grDevices::xyz.coords(x)
    methods::callGeneric(x = x$x, y = x$y, z = x$z,
                         h = h, n = n, nlevels = nlevels, levels = levels,
                         col = col, lty = lty, lwd = lwd, ...)
  }
)

## Adapted from MASS::kde2d
kde <- function(x, y, h = NULL, n = 25, lims = c(range(x), range(y))) {
  n <- rep(n, length.out = 2L)
  gx <- seq(lims[1L], lims[2L], length.out = n[1L])
  gy <- seq(lims[3L], lims[4L], length.out = n[2L])

  if (is.null(h)) {
    h <- c(bandwidth(x), bandwidth(y))
  } else {
    h <- rep(h, length.out = 2L)
  }
  h <- h / 4

  if (any(h <= 0))
    stop("Bandwidths must be strictly positive.", call. = FALSE)

  ax <- outer(gx, x, "-") / h[1L]
  ay <- outer(gy, y, "-") / h[2L]

  nx <- length(x)
  z <- tcrossprod(
    matrix(stats::dnorm(ax), ncol = nx),
    matrix(stats::dnorm(ay), ncol = nx)
  )
  z <- z / (nx * h[1L] * h[2L])

  list(x = gx, y = gy, z = z)
}

# Copied from MASS::bandwidth.nrd()
bandwidth <- function(x) {
  r <- stats::quantile(x, c(0.25, 0.75))
  h <- (r[2L] - r[1L]) / 1.34
  4 * 1.06 * min(sqrt(stats::var(x)), h) * length(x)^(-1 / 5)
}
