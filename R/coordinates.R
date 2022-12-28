# TERNARY COORDINATES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname coordinates_ternary
#' @aliases coordinates_ternary,numeric,numeric,numeric-method
setMethod(
  f = "coordinates_ternary",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z) {
    ## Validation
    if (any(x < 0 | y < 0 | z < 0)) {
      stop("Positive values are expected.", call. = FALSE)
    }

    total <- x + y + z
    x <- x / total
    y <- y / total
    z <- z / total

    list(x = y + z / 2, y = z * sqrt(3) / 2)
  }
)

#' @export
#' @rdname coordinates_ternary
#' @aliases coordinates_ternary,ANY,missing,missing-method
setMethod(
  f = "coordinates_ternary",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x) {
    xyz <- grDevices::xyz.coords(x)
    methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z)
  }
)

#' @export
#' @rdname coordinates_cartesian
#' @aliases coordinates_cartesian,numeric,numeric-method
setMethod(
  f = "coordinates_cartesian",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    if (any(x < 0 | y < 0)) {
      stop("Positive values are expected.", call. = FALSE)
    }

    k = y * 2 / sqrt(3)
    j = x - k / 2
    i = 1 - (j + k)

    list(x = i, y = j, z = k)
  }
)

#' @export
#' @rdname coordinates_cartesian
#' @aliases coordinates_cartesian,ANY,missing-method
setMethod(
  f = "coordinates_cartesian",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x) {
    xy <- grDevices::xy.coords(x)
    methods::callGeneric(x = xy$x, y = xy$y)
  }
)
