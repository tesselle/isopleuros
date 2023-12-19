# TERNARY POINTS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_points
#' @aliases ternary_points,numeric,numeric,numeric-method
setMethod(
  f = "ternary_points",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, center = FALSE, scale = FALSE, type = "p", ...) {
    pt <- coordinates_ternary(x, y, z, center = center, scale = scale)
    graphics::points(x = pt, type = type, ...)

    pt <- utils::modifyList(pt, list(x = x, y = y, z = z))
    invisible(pt)
  }
)

#' @export
#' @rdname ternary_points
#' @aliases ternary_points,ANY,missing,missing-method
setMethod(
  f = "ternary_points",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, center = FALSE, scale = FALSE, type = "p", ...) {
    xyz <- grDevices::xyz.coords(x)
    pt <- methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z,
                               center = center, scale = scale,
                               type = type, ...)
    invisible(pt)
  }
)
