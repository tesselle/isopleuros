# STATISTICS
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
    ## ALR
    coda <- cbind(x, y, z)
    ratio <- alr(coda)

    ## Compute ellipse
    mu <- colMeans(ratio)
    sigma <- stats::cov(ratio)
    # rob <- robustbase::covMcd(ratio)
    # mu <- rob$center
    # sigma <- rob$cov
    xy <- ellipse(sigma = sigma, mu = mu, radius = radius)

    for (i in seq_along(xy)) {
      tern <- alr_inv(xy[[i]]) # Inverse transform
      coords <- coordinates_ternary(tern)
      graphics::polygon(x = coords$x, y = coords$y, ...)
    }
  }
)

#' @export
#' @rdname ternary_ellipse
#' @aliases ternary_ellipse,ANY,missing,missing-method
setMethod(
  f = "ternary_ellipse",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, radius = 1, ...) {
    x <- grDevices::xyz.coords(x)
    methods::callGeneric(x = x$x, y = x$y, z = x$z, radius = radius, ...)
  }
)

## Confidence ellipse ----------------------------------------------------------
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

## Probability ellipse ---------------------------------------------------------
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

# Convex hull ==================================================================
#' @export
#' @rdname ternary_hull
#' @aliases ternary_hull,numeric,numeric,numeric-method
setMethod(
  f = "ternary_hull",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, ...) {
    coords <- coordinates_ternary(x, y, z)
    hull <- grDevices::chull(coords)
    graphics::polygon(x = coords$x[hull], y = coords$y[hull], ...)
  }
)

#' @export
#' @rdname ternary_hull
#' @aliases ternary_hull,ANY,missing,missing-method
setMethod(
  f = "ternary_hull",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, ...) {
    x <- grDevices::xyz.coords(x)
    methods::callGeneric(x = x$x, y = x$y, z = x$z, ...)
  }
)
