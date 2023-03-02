# TERNARY POLYGON
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_polygon
#' @aliases ternary_polygon,numeric,numeric,numeric-method
setMethod(
  f = "ternary_polygon",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, ...) {
    coords <- coordinates_ternary(x, y, z)
    graphics::polygon(x = coords, ...)
    invisible(data.frame(x = x, y = y, z = z))
  }
)

#' @export
#' @rdname ternary_polygon
#' @aliases ternary_polygon,ANY,missing,missing-method
setMethod(
  f = "ternary_polygon",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, ...) {
    xyz <- grDevices::xyz.coords(x)
    methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z, ...)
  }
)
