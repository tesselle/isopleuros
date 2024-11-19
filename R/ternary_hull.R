# CONVEX HULL
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_hull
#' @aliases ternary_hull,numeric,numeric,numeric-method
setMethod(
  f = "ternary_hull",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, center = FALSE, scale = FALSE, ...) {
    coords <- coordinates_ternary(x, y, z, center = center, scale = scale)
    hull <- grDevices::chull(coords)
    graphics::polygon(x = coords$x[hull], y = coords$y[hull], ...)

    coords <- utils::modifyList(coords, list(x = x, y = y, z = z))
    invisible(coords)
  }
)

#' @export
#' @rdname ternary_hull
#' @aliases ternary_hull,ANY,missing,missing-method
setMethod(
  f = "ternary_hull",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, center = FALSE, scale = FALSE, ...) {
    xyz <- grDevices::xyz.coords(x)
    coords <- methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z,
                                   center = center, scale = scale, ...)
    invisible(coords)
  }
)
