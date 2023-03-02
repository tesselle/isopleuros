# TERNARY POINTS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_points
#' @aliases ternary_points,numeric,numeric,numeric-method
setMethod(
  f = "ternary_points",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, type = "p", ...) {
    coords <- coordinates_ternary(x, y, z)
    graphics::points(x = coords, type = type, ...)
    invisible(data.frame(x = x, y = y, z = z))
  }
)

#' @export
#' @rdname ternary_points
#' @aliases ternary_points,ANY,missing,missing-method
setMethod(
  f = "ternary_points",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, type = "p", ...) {
    xyz <- grDevices::xyz.coords(x)
    methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z, type = type, ...)
  }
)
