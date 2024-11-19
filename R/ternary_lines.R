# TERNARY LINES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_lines
#' @aliases ternary_lines,numeric,numeric,numeric-method
setMethod(
  f = "ternary_lines",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, type = "l", ...) {
    coords <- coordinates_ternary(x, y, z)
    graphics::lines(x = coords, type = type, ...)

    invisible(list(x = x, y = y, z = z))
  }
)

#' @export
#' @rdname ternary_lines
#' @aliases ternary_lines,ANY,missing,missing-method
setMethod(
  f = "ternary_lines",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, type = "l", ...) {
    xyz <- grDevices::xyz.coords(x)
    coords <- methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z,
                                   type = type, ...)
    invisible(coords)
  }
)
