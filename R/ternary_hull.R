# CONVEX HULL
#' @include AllGenerics.R
NULL

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
