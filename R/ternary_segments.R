# TERNARY SEGMENTS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_segments
#' @aliases ternary_segments,numeric,numeric,numeric-method
setMethod(
  f = "ternary_segments",
  signature = c(x0 = "numeric", y0 = "numeric", z0 = "numeric"),
  definition = function(x0, y0, z0, x1 = x0, y1 = y0, z1 = z0, ...) {
    coords0 <- coordinates_ternary(x0, y0, z0)
    coords1 <- coordinates_ternary(x1, y1, z1)
    graphics::segments(x0 = coords0$x, y0 = coords0$y,
                       x1 = coords1$x, y1 = coords1$y, ...)

    invisible(NULL)
  }
)
