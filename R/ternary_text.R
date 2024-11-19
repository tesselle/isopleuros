# TERNARY TEXT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_text
#' @aliases ternary_text,numeric,numeric,numeric-method
setMethod(
  f = "ternary_text",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, center = FALSE, scale = FALSE,
                        labels = seq_along(x), ...) {
    coords <- coordinates_ternary(x, y, z, center = center, scale = scale)
    graphics::text(x = coords, labels = labels, ...)

    invisible(list(x = x, y = y, z = z))
  }
)

#' @export
#' @rdname ternary_text
#' @aliases ternary_text,ANY,missing,missing-method
setMethod(
  f = "ternary_text",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, center = FALSE, scale = FALSE, labels = seq_along(x$x), ...) {
    x <- grDevices::xyz.coords(x)
    force(labels)
    coords <- methods::callGeneric(x = x$x, y = x$y, z = x$z,
                                   center = center, scale = scale,
                                   labels = labels, ...)
    invisible(coords)
  }
)
