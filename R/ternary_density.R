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
                        palette = function(i) grDevices::hcl.colors(i, "YlOrRd", rev = TRUE),
                        ...) {
    ## Calculate density contour lines
    xy <- coordinates_kde(x = x, y = y, z = z, h = h, n = n,
                          nlevels = nlevels, levels = levels)

    ## Get contour levels
    lvl <- vapply(X = xy, FUN = getElement, FUN.VALUE = numeric(1),
                  name = "level")

    ## Colors
    ## (number of levels may differ from nlevels due to pretty())
    col <- palette(length(unique(lvl)))
    names(col) <- unique(lvl)
    col <- col[as.character(lvl)]

    ## Plot
    for (i in seq_along(xy)) {
      ## Get contour
      level <- xy[[i]]

      ## Inverse ILR transform
      tern <- ilr_inv(cbind(level$x, level$y))

      ## Plot ternary lines
      ternary_lines(tern, col = col[[i]], ...)
    }

    invisible(list(levels = lvl, colors = col))
  }
)

#' @export
#' @rdname ternary_density
#' @aliases ternary_density,ANY,missing,missing-method
setMethod(
  f = "ternary_density",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, h = NULL, n = 25, nlevels = 10, levels = NULL,
                        palette = function(i) grDevices::hcl.colors(i, "YlOrRd", rev = TRUE),
                        ...) {
    xyz <- grDevices::xyz.coords(x)
    pt <- methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z,
                               h = h, n = n, nlevels = nlevels, levels = levels,
                               palette = palette, ...)
    invisible(pt)
  }
)

#' Calculate KDE Contour Lines
#'
#' Computes 2D-KDE contours coordinates.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param h A length-one [`numeric`] vector giving the bandwidth.
#' @param n A length-one [`numeric`] specifying the number of grid points.
#' @param nlevels A length-one [`numeric`] vector specifying the number of
#'  contour levels desired. Only used if `levels` is `NULL`.
#' @param levels A [`numeric`] vector of levels at which to draw contour lines.
#' @return
#'  A [`list`] of contours, each itself a list with elements
#'  (see [grDevices::contourLines()]):
#'  \tabular{ll}{
#'   `level` \tab The contour level. \cr
#'   `x`     \tab The ILR x-coordinates of the contour. \cr
#'   `y`     \tab The ILR y-coordinates of the contour. \cr
#'  }
#' @keywords internal
#' @noRd
coordinates_kde <- function(x, y, z, h = NULL, n = 25,
                            nlevels = 10, levels = NULL) {
  ## ILR
  coda <- cbind(x, y, z)
  ratio <- ilr(coda)

  ## Compute KDE
  lims <- expand_range(ratio, mult = 0.2)
  dens <- kde(
    x = ratio[, 1],
    y = ratio[, 2],
    h = h,
    n = n,
    xlim = lims, # x and y range should be same
    ylim = lims
  )

  ## Compute contours
  grDevices::contourLines(
    x = dens$x,
    y = dens$y,
    z = dens$z,
    nlevels = nlevels,
    levels = levels %||% pretty(range(dens$z, na.rm = TRUE), nlevels)
  )
}

## Adapted from MASS::kde2d
kde <- function(x, y, h = NULL, n = 25, gx = NULL, gy = NULL,
                xlim = range(x), ylim = range(y)) {
  n <- rep(n, length.out = 2L)
  if (is.null(gx)) gx <- seq(xlim[1L], xlim[2L], length.out = n[1L])
  if (is.null(gy)) gy <- seq(ylim[1L], ylim[2L], length.out = n[2L])

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
