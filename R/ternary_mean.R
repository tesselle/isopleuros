# MEAN
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_mean
#' @aliases ternary_mean,numeric,numeric,numeric-method
setMethod(
  f = "ternary_mean",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, ...) {
    x <- gmean(x)
    y <- gmean(y)
    z <- gmean(z)

    pt <- ternary_points(x = x, y = y, z = z, ...)
    invisible(pt)
  }
)

#' @export
#' @rdname ternary_mean
#' @aliases ternary_mean,ANY,missing,missing-method
setMethod(
  f = "ternary_mean",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, ...) {
    xyz <- grDevices::xyz.coords(x)
    pt <- methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z, ...)
    invisible(pt)
  }
)

#' Geometric Mean
#'
#' @param x A [`numeric`] vector.
#' @param trim A length-one [`numeric`] vector specifying the fraction (0 to 0.5)
#'  of observations to be trimmed from each end of `x` before the mean is
#'  computed.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @return A [`numeric`] vector.
#' @keywords internal
#' @noRd
gmean <- function(x, trim = 0, na.rm = FALSE) {
  index <- is.finite(x) & x > 0
  exp(mean(log(unclass(x)[index]), trim = trim, na.rm = na.rm))
}
