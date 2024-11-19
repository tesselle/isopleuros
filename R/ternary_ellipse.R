# ELLIPSE
#' @include AllGenerics.R
NULL

# Ellipse ======================================================================
#' @export
#' @rdname ternary_ellipse
#' @aliases ternary_ellipse,numeric,numeric,numeric-method
setMethod(
  f = "ternary_ellipse",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, radius = 1, ...) {
    ## ILR
    coda <- cbind(x, y, z)
    ratio <- ilr(coda)

    ## Compute ellipse
    mu <- colMeans(ratio)
    sigma <- stats::cov(ratio)
    # rob <- robustbase::covMcd(ratio)
    # mu <- rob$center
    # sigma <- rob$cov
    xy <- ellipse(sigma = sigma, mu = mu, radius = radius)

    for (i in seq_along(xy)) {
      tern <- ilr_inv(xy[[i]]) # Inverse transform
      coords <- coordinates_ternary(tern)
      graphics::polygon(x = coords$x, y = coords$y, ...)
    }

    invisible(list(x = x, y = y, z = z))
  }
)

#' @export
#' @rdname ternary_ellipse
#' @aliases ternary_ellipse,ANY,missing,missing-method
setMethod(
  f = "ternary_ellipse",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, radius = 1, ...) {
    xyz <- grDevices::xyz.coords(x)
    pt <- methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z,
                               radius = radius, ...)
    invisible(pt)
  }
)

#' Computes an Ellipse
#'
#' @param sigma A square positive definite \eqn{2 \times 2}{2 x 2} covariance
#'  or correlation `matrix`.
#' @param mu A length-two [`numeric`] vector giving the centre of the ellipse.
#' @param scale If `sigma` is a correlation matrix, then the standard deviations
#'  of each parameter can be given in the scale parameter.
#'  Defaults to `c(1, 1)`, so no rescaling will be done.
#' @param level A length-\eqn{k} [`numeric`] vector giving the confidence level
#'  of a pairwise confidence region.
#' @param radius The size of the ellipse may also be controlled by specifying
#'  the value of a t-statistic on its boundary.
#' @param n A length-one [`numeric`] vector specifying the number of points used
#'  in the ellipse.
#' @note Adapted from [ellipse::ellipse()].
#' @return
#'  A [`list`] of \eqn{k} \eqn{n \times 2}{n x 2} `matrix`, suitable for
#'  plotting.
#' @keywords internal
#' @noRd
ellipse <- function(sigma, mu = c(0, 0), scale = c(1, 1), level = 0.95,
                    radius = sqrt(stats::qchisq(level, 2)), n = 100, ...) {
  r <- sigma[1, 2]

  if (missing(scale)) {
    scale <- sqrt(diag(sigma))
    if (scale[1] > 0) r <- r / scale[1]
    if (scale[2] > 0) r <- r / scale[2]
  }

  r <- min(max(r, -1), 1)  # clamp to -1..1, in case of rounding errors
  d <- acos(r)
  a <- seq(0, 2 * pi, len = n)

  lapply(
    X = radius,
    FUN = function(x) {
      matrix(
        data = c(x * scale[1] * cos(a + d / 2) + mu[1],
                 x * scale[2] * cos(a - d / 2) + mu[2]),
        nrow = n,
        ncol = 2,
        dimnames = list(NULL, c("x", "y"))
      )
    }
  )
}

# Confidence ellipse ===========================================================
#' @export
#' @rdname ternary_ellipse
#' @aliases ternary_confidence,numeric,numeric,numeric-method
setMethod(
  f = "ternary_confidence",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, level = 0.95, ...) {
    df1 <- 2
    df2 <- length(x) - 2
    radius <- sqrt(stats::qf(p = level, df1, df2) * df1 / df2)
    ternary_ellipse(x, y, z, radius = radius, ...)
  }
)

#' @export
#' @rdname ternary_ellipse
#' @aliases ternary_confidence,ANY,missing,missing-method
setMethod(
  f = "ternary_confidence",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, level = 0.95, ...) {
    x <- grDevices::xyz.coords(x)
    methods::callGeneric(x = x$x, y = x$y, z = x$z, level = level, ...)
  }
)

# Probability ellipse ==========================================================
#' @export
#' @rdname ternary_ellipse
#' @aliases ternary_tolerance,numeric,numeric,numeric-method
setMethod(
  f = "ternary_tolerance",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, level = 0.95, ...) {
    radius <- sqrt(stats::qchisq(p = level, df = 2))
    ternary_ellipse(x, y, z, radius = radius, ...)
  }
)

#' @export
#' @rdname ternary_ellipse
#' @aliases ternary_tolerance,ANY,missing,missing-method
setMethod(
  f = "ternary_tolerance",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, level = 0.95, ...) {
    x <- grDevices::xyz.coords(x)
    methods::callGeneric(x = x$x, y = x$y, z = x$z, level = level, ...)
  }
)
