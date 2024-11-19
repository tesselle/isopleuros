# TERNARY CROSS-HAIRS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_crosshairs
#' @aliases ternary_crosshairs,numeric,numeric,numeric-method
setMethod(
  f = "ternary_crosshairs",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z,
                        x_mark = TRUE, y_mark = TRUE, z_mark = TRUE, ...) {
    total <- x + y + z
    zero <- rep(0, length(x))

    if (x_mark) {
      ternary_segments(x0 = x, y0 = y, z0 = z,
                       x1 = x / total, y1 = 1 - (x / total), z1 = zero, ...)
    }
    if (y_mark) {
      ternary_segments(x0 = x, y0 = y, z0 = z,
                       x1 = zero, y1 = y / total, z1 = 1 - (y / total), ...)
    }
    if (z_mark) {
      ternary_segments(x0 = x, y0 = y, z0 = z,
                       x1 = 1 - (z / total), y1 = zero, z1 = z / total, ...)
    }

    invisible(list(x = x, y = y, z = z))
  }
)

#' @export
#' @rdname ternary_crosshairs
#' @aliases ternary_crosshairs,ANY,missing,missing-method
setMethod(
  f = "ternary_crosshairs",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, x_mark = TRUE, y_mark = TRUE, z_mark = TRUE, ...) {
    xyz <- grDevices::xyz.coords(x)
    pt <- methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z,
                               x_mark = x_mark, y_mark = y_mark, z_mark = z_mark, ...)
    invisible(pt)
  }
)
