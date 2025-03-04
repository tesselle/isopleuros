# TERNARY COORDINATES
#' @include AllGenerics.R
NULL

# Ternary to cartesian =========================================================
#' @export
#' @rdname coordinates_ternary
#' @aliases coordinates_ternary,numeric,numeric,numeric-method
setMethod(
  f = "coordinates_ternary",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, center = FALSE, scale = FALSE,
                        missing = getOption("isopleuros.missing")) {
    ## Validation
    n <- length(x)
    assert_length(y, n)
    assert_length(z, n)
    assert_center(center)
    assert_scale(scale)

    ## Missing values
    if (missing) {
      x[is.na(x)] <- 0
      y[is.na(y)] <- 0
      z[is.na(z)] <- 0
    }

    total <- x + y + z
    na <- is.na(x) | is.na(y) | is.na(z)
    zero <- total == 0

    x <- x[!na & !zero]
    y <- y[!na & !zero]
    z <- z[!na & !zero]
    total <- total[!na & !zero]

    ## Validation
    if (any(x < 0 | y < 0 | z < 0)) {
      stop(tr_("Positive values are expected."), call. = FALSE)
    }

    coord <- matrix(data = c(x, y, z), ncol = 3) / total
    coord <- scale(coord, center = center, scale = scale)

    list(
      x = coord$y + coord$z / 2,
      y = coord$z * sqrt(3) / 2,
      center = coord$center,
      scale = coord$scale
    )
  }
)

#' @export
#' @rdname coordinates_ternary
#' @aliases coordinates_ternary,ANY,missing,missing-method
setMethod(
  f = "coordinates_ternary",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, xlab = NULL, ylab = NULL, zlab = NULL,
                        center = FALSE, scale = FALSE,
                        missing = getOption("isopleuros.missing")) {
    xyz <- grDevices::xyz.coords(x, xlab = xlab, ylab = ylab, zlab = zlab)
    methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z,
                         center = center, scale = scale, missing = missing)
  }
)

# Cartesian to ternary =========================================================
#' @export
#' @rdname coordinates_cartesian
#' @aliases coordinates_cartesian,numeric,numeric-method
setMethod(
  f = "coordinates_cartesian",
  signature = c(x = "numeric", y = "numeric"),
  definition = function(x, y) {
    ## Validation
    n <- length(x)
    assert_length(y, n)

    k <- y * 2 / sqrt(3)
    j <- x - k / 2
    i <- 1 - (j + k)

    list(
      x = i,
      y = j,
      z = k
    )
  }
)

#' @export
#' @rdname coordinates_cartesian
#' @aliases coordinates_cartesian,ANY,missing-method
setMethod(
  f = "coordinates_cartesian",
  signature = c(x = "ANY", y = "missing"),
  definition = function(x, xlab = NULL, ylab = NULL) {
    xy <- grDevices::xy.coords(x, xlab = xlab, ylab = ylab)
    methods::callGeneric(x = xy$x, y = xy$y)
  }
)

# Scale ========================================================================
#' Center and Scale
#'
#' @param x,y,z The x, y and z coordinates of a set of points. Both y and z can
#'  be left at `NULL`. In this case, an attempt is made to interpret x in a way
#'  suitable for plotting.
#' @param center A [`logical`] scalar or a [`numeric`] vector giving the center.
#' @param scale A [`logical`] scalar or a length-one [`numeric`] vector giving a
#'  scaling factor.
#' @return
#'  A [`list`] with the components:
#'  \tabular{ll}{
#'   `x` \tab A [`numeric`] vector of x values. \cr
#'   `y` \tab A [`numeric`] vector of y values. \cr
#'   `z` \tab A [`numeric`] vector of z values. \cr
#'   `center` \tab A [`numeric`] vector giving the center. \cr
#'   `scale` \tab A [`numeric`] vector giving the scale factor. \cr
#'  }
#' @keywords internal
#' @noRd
scale <- function(x, y = NULL, z = NULL, center = TRUE, scale = TRUE) {
  xyz <- grDevices::xyz.coords(x = x, y = y, z = z)
  xyz <- matrix(data = c(xyz$x, xyz$y, xyz$z), ncol = 3)

  y <- xyz
  if (!isFALSE(center) && !is.null(center)) {
    if (isTRUE(center)) {
      center <- apply(X = xyz, MARGIN = 2, FUN = gmean, simplify = TRUE)
      center <- center / sum(center)
    }
    assert_length(center, NCOL(xyz))

    y <- t(t(y) / center)
    y <- y / rowSums(y)
  } else {
    center <- rep(1, NCOL(xyz))
  }

  if (!isFALSE(scale) && !is.null(scale)) {
    if (isTRUE(scale)) {
      scale <- sqrt(mean(diag(stats::cov(clr(xyz)))))
    }
    assert_length(scale, 1)

    y <- y^(1 / scale)
    y <- y / rowSums(y)
  } else {
    scale <- 1
  }

  list(
    x = y[, 1],
    y = y[, 2],
    z = y[, 3],
    center = center,
    scale = scale
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
  if (na.rm) x <- x[is.finite(x)]
  x <- x[x > 0]
  exp(mean(log(x), trim = trim))
}

# Centered Log-Ratios ==========================================================
#' Centered Log-Ratios (CLR)
#'
#' Computes CLR transformation.
#' @param x A [`numeric`] `matrix`.
#' @keywords internal
#' @noRd
clr <- function(x) {
  J <- ncol(x)
  clr <- log(x, base = exp(1)) %*% diag(J)

  clr
}

#' Inverse Centered Log-Ratios Transformation
#'
#' Computes inverse CLR transformation.
#' @param x A [`numeric`] `matrix` of log ratios.
#' @keywords internal
#' @noRd
clr_inv <- function(x) {
  y <- exp(x)
  y <- y / rowSums(y)

  y
}

# Isometric Log-Ratios =========================================================
#' Isometric Log-Ratios (ILR)
#'
#' Computes ILR transformation.
#' @param x A [`numeric`] `matrix`.
#' @keywords internal
#' @noRd
ilr <- function(x) {
  D <- ncol(x)
  H <- ilr_base(D)

  ## Rotated and centered values
  y <- log(x, base = exp(1))
  ilr <- y %*% H

  ilr
}

#' Canonical Basis for Isometric Log-Ratio transformation
#'
#' Computes the canonical basis in the CLR plane used for ILR transformation.
#' @param a A [`numeric`] value giving the number of parts of the simplex.
#' @keywords internal
#' @noRd
ilr_base <- function(n) {
  seq_parts <- seq_len(n - 1)

  ## Helmert matrix (rotation matrix)
  H <- stats::contr.helmert(n)                  # n x n-1
  H <- t(H) / sqrt((seq_parts + 1) * seq_parts) # n-1 x n

  ## Center
  m <- diag(x = 1, nrow = n) - matrix(data = 1 / n, nrow = n, ncol = n)
  H <- tcrossprod(m, H)

  H
}

#' Inverse Isometric Log-Ratio Transformation
#'
#' Computes inverse ILR transformation.
#' @param x A [`numeric`] `matrix` of log ratios.
#' @keywords internal
#' @noRd
ilr_inv <- function(x) {
  D <- ncol(x) + 1
  H <- ilr_base(D)

  y <- tcrossprod(x, H)
  y <- exp(y)
  y <- y / rowSums(y)

  y
}
