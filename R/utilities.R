# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}


boundary <- function(side, limits) {
  limits <- range(limits)
  side_min <- diag(1, 3, 3) - limits[1L]
  side_max <- diag(1, 3, 3) - limits[2L]
  side_range <- rbind(side_min, side_max)
  side_range[side_range < 0] <- 0
  side_range[, side] <- rep(limits, each = 3)
  side_range[rowSums(side_range) == 1, ]
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

#' Computes an Ellipse
#'
#' @param sigma A square positive definite \eqn{2 \times 2}{2 x 2} covariance
#'  or correlation `matrix`.
#' @param mu A length-two [`numeric`] vector giving the centre of the ellipse.
#' @param scale If `sigma` is a correlation matrix, then the standard deviations
#'  of each parameter can be given in the scale parameter.
#'  Defaults to `c(1, 1)`, so no rescaling will be done.
#' @param level A length-one [`numeric`] vector giving the confidence level of
#'  a pairwise confidence region.
#' @param radius The size of the ellipse may also be controlled by specifying
#'  the value of a t-statistic on its boundary.
#' @param n A length-one [`numeric`] vector specifying the number of points used
#'  in the ellipse.
#' @note Adapted from [ellipse::ellipse()].
#' @return An \eqn{n \times 2}{n x 2} `matrix`, suitable for plotting.
#' @keywords internal
#' @noRd
ellipse <- function(sigma, mu = c(0, 0), scale = c(1, 1), level = 0.95,
                    radius = sqrt(stats::qchisq(level, 2)), n = 100, ...) {
  r <- sigma[1, 2]

  if (missing(scale)) {
    scale <- sqrt(diag(sigma))
    if (scale[1] > 0) r <- r / scale[1]
    if (scale[2] > 0) r <- r / scale[2]
  }

  r <- min(max(r, -1), 1)  # clamp to -1..1, in case of rounding errors
  d <- acos(r)
  a <- seq(0, 2 * pi, len = n)

  lapply(
    X = radius,
    FUN = function(x) {
      matrix(
        data = c(x * scale[1] * cos(a + d / 2) + mu[1],
                 x * scale[2] * cos(a - d / 2) + mu[2]),
        nrow = n,
        ncol = 2,
        dimnames = list(NULL, c("x", "y"))
      )
    }
  )
}

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

#' Additive Log-Ratios (ALR)
#'
#' Computes ALR transformation.
#' @param x A [`numeric`] `matrix`.
#' @param j An [`integer`] giving the index of the rationing part (denominator).
#' @keywords internal
#' @noRd
alr <- function(x, j = ncol(x)) {
  D <- ncol(x)
  parts <- colnames(x)

  ## Reorder
  j <- if (is.character(j)) which(parts == j) else as.integer(j)
  ordering <- c(which(j != seq_len(D)), j)
  parts <- parts[ordering]
  x <- x[, ordering]

  base <- diag(1, nrow = D, ncol = D - 1)
  base[D, ] <- -1

  log(x, base = exp(1)) %*% base
}

#' Inverse Additive Log-Ratio Transformation
#'
#' Computes inverse ALR transformation.
#' @param x A [`numeric`] `matrix`.
#' @keywords internal
#' @noRd
alr_inv <- function(x) {
  y <- exp(x)
  y <- y / (1 + rowSums(y))
  z <- 1 - rowSums(y)

  cbind(y, z)
}
