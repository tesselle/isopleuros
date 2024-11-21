# CONTOUR
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_contour
#' @aliases ternary_contour,numeric,numeric,numeric-method
setMethod(
  f = "ternary_contour",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, value, n = 50, nlevels = 10,
                        levels = pretty(range(value, na.rm = TRUE), nlevels),
                        ilr = TRUE, method = "linear", extrapolate = FALSE,
                        palette = function(i) grDevices::hcl.colors(i, "YlOrRd", rev = TRUE),
                        ...) {
    ## Calculate contour lines
    xy <- coordinates_contour(x = x, y = y, z = z, value = value, n = n,
                              nlevels = nlevels, levels = levels,
                              ilr = ilr, method = method,
                              extrapolate = extrapolate)

    ## Get contour levels
    lvl <- vapply(X = xy, FUN = getElement, FUN.VALUE = numeric(1),
                  name = "level")

    ## Colors
    ## (number of levels may differ from nlevels due to interp())
    col <- palette(length(unique(lvl)))
    names(col) <- unique(lvl)
    col <- col[as.character(lvl)]

    ## Plot
    for (i in seq_along(xy)) {
      ## Get contour
      level <- xy[[i]]

      ## Inverse ILR transform
      tern <- cbind(level$x, level$y)
      tern <- if (ilr) ilr_inv(tern) else coordinates_cartesian(tern)

      ## Plot ternary lines
      ternary_lines(tern, col = col[[i]], ...)
    }

    invisible(list(levels = lvl, colors = col))
  }
)

#' @export
#' @rdname ternary_contour
#' @aliases ternary_contour,ANY,missing,missing-method
setMethod(
  f = "ternary_contour",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, value, n = 50, nlevels = 10,
                        levels = pretty(range(value, na.rm = TRUE), nlevels),
                        ilr = TRUE, method = "linear", extrapolate = FALSE,
                        palette = function(i) grDevices::hcl.colors(i, "YlOrRd", rev = TRUE),
                        ...) {
    xyz <- grDevices::xyz.coords(x)
    coords <- methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z, value = value,
                                   n = n, nlevels = nlevels, levels = levels,
                                   ilr = ilr, method = method,
                                   extrapolate = extrapolate,
                                   palette = palette, ...)
    invisible(coords)
  }
)

#' Calculate Contour Lines
#'
#' Computes contours coordinates.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param value A [`numeric`] [`matrix`] containing the values to be plotted.
#' @param n A length-one [`numeric`] specifying the number of grid points.
#' @param nlevels A length-one [`numeric`] vector specifying the number of
#'  contour levels desired. Only used if `levels` is `NULL`.
#' @param levels A [`numeric`] vector of levels at which to draw contour lines.
#' @param ilr A [`logical`] scalar: should interpolation be computed in ILR
#'  space? If `FALSE`, interpolation is computed in Cartesian space.
#' @param method A [`character`] string: specifying the method for interpolation
#'  (see [interp::interp()]).
#' @param extrapolate A [`logical`] scalar: should extrapolation be used outside
#'  of the convex hull determined by the data points (see [interp::interp()])?
#' @param ... Further parameters to be passed to [interp::interp()].
#' @return
#'  A [`list`] of contours, each itself a list with elements
#'  (see [grDevices::contourLines()]):
#'  \tabular{ll}{
#'   `level` \tab The contour level. \cr
#'   `x`     \tab The (ILR) x-coordinates of the contour. \cr
#'   `y`     \tab The (ILR) y-coordinates of the contour. \cr
#'  }
#' @keywords internal
#' @noRd
coordinates_contour <- function(x, y, z, value, n = 50, nlevels = 10,
                                levels = pretty(range(value, na.rm = TRUE), nlevels),
                                ilr = TRUE, method = "linear", extrapolate = FALSE,
                                ...) {
  ## Validation
  if (!requireNamespace("interp", quietly = TRUE)) {
    msg <- "The interp package is required. Please install it."
    stop(msg, call. = FALSE)
  }
  assert_length(value, length(x))

  ## ILR vs Cartesian
  coda <- cbind(x, y, z)
  ratio <- if (ilr) ilr(coda) else do.call(cbind, coordinates_ternary(coda))

  ## Remove NA/Inf (if any)
  ok <- apply(X = ratio, MARGIN = 1, FUN = function(x) all(is.finite(x)))
  ratio <- ratio[ok, , drop = FALSE]
  value <- value[ok]

  ## Interpolate
  xlim <- expand_range(ratio[, 1], mult = 0.2)
  ylim <- expand_range(ratio[, 2], mult = 0.2)
  interp <- interp::interp(
    x = ratio[, 1],
    y = ratio[, 2],
    z = value,
    xo = seq(xlim[1L], xlim[2L], length.out = n),
    yo = seq(ylim[1L], ylim[2L], length.out = n),
    method = method,
    extrap = extrapolate,
    ...
  )

  ## Compute contours
  grDevices::contourLines(
    x = interp$x,
    y = interp$y,
    z = interp$z,
    nlevels = nlevels,
    levels = levels
  )
}
