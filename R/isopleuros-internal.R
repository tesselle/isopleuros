# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}

map_color <- function(values, palette = NULL, scheme = "viridis",
                      ignore_zero = FALSE) {
  if (isFALSE(palette)) return(values)

  if (is.null(palette)) {
    palette <- function(x) {
      x <- (x - min(x)) / (max(x) - min(x)) # Rescale to [0,1]
      col <- grDevices::hcl.colors(256L, palette = scheme)
      grDevices::rgb(grDevices::colorRamp(col)(x), maxColorValue = 255)
    }
  }

  color <- rep(NA, length(values))
  ok <- is.finite(values) # Remove NA/Inf (if any)
  if (ignore_zero) ok[ok] <- values[ok] > 0
  color[ok] <- palette(values[ok])
  color
}

#' Expand Range
#'
#' @param x A [`numeric`] vector.
#' @param mult A [`numeric`] value giving the multiplicative constant.
#' @param add A [`numeric`] value giving the additive constant.
#' @return A length-two [`numeric`] vector.
#' @keywords internal
#' @noRd
expand_range <- function(x, mult = 0, add = 0) {
  lims <- range(x)
  lims <- lims + c(-1, 1) * (diff(lims) * mult + add)
  lims
}

#' Rotate Around a Point
#'
#' @param x A column vector giving the x and y coordinates of the point to be
#'  rotated.
#' @param theta A length-one [`numeric`] vector specifying the rotation angle
#'  (in radian).
#' @param origin A length-two [`numeric`] vector specifying the coordinates
#'  of the point to rotate around.
#' @return A `matrix` of coordinates.
#' @keywords internal
#' @noRd
rotate <- function(x, theta = 0, origin = c(0.5, sqrt(3) / 6)) {
  ## Translation matrix
  trans <- diag(1, 3, 3)
  trans[, 3] <- c(origin, 1)

  ## Rotation matrix
  rot <- matrix(
    data = c(cos(theta), sin(theta), 0, -sin(theta), cos(theta), 0, 0, 0, 1),
    nrow = 3,
    ncol = 3
  )

  x <- as.matrix(x)
  if (nrow(x) < 3) x <- rbind(x, rep(1, ncol(x)))
  t(trans %*% rot %*% solve(trans) %*% x)
}

#' Check Object Length
#'
#' @param x An object to be checked.
#' @param expected An appropriate expected value.
#' @return
#'  Throws an error, if any, and returns `x` invisibly otherwise.
#' @keywords internal
#' @noRd
assert_length <- function(x, expected) {
  arg <- deparse(substitute(x))
  if (length(x) != expected) {
    str <- "%s must be of length %d; not %d."
    msg <- sprintf(str, sQuote(arg), expected, length(x))
    stop(msg, call. = FALSE)
  }
  invisible(x)
}

assert_center <- function(x, current = getOption("isopleuros.center")) {
  ok <- isTRUE(x) || is.numeric(x)
  if (!ok && is.numeric(current) && !all(current == 1)) {
    msg <- "The current plot has been centered, but your data doesn't seem to be."
    message(msg)
  }
  invisible(x)
}

assert_scale <- function(x, current = getOption("isopleuros.scale")) {
  ok <- isTRUE(x) || is.numeric(x)
  if (!ok && is.numeric(current) && !all(current == 1)) {
    msg <- "The current plot has been scaled, but your data doesn't seem to be."
    message(msg)
  }
  invisible(x)
}

assert_package <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    msg <- "Package %s needed for this function to work. Please install it."
    stop(sprintf(msg, sQuote(x)), call. = FALSE)
  }
  invisible(NULL)
}
