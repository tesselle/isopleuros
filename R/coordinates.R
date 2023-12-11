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
  definition = function(x, y, z, missing = getOption("isopleuros.missing")) {
    ## Validation
    n <- length(x)
    assert_length(y, n)
    assert_length(z, n)

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
      stop("Positive values are expected.", call. = FALSE)
    }

    x <- x / total
    y <- y / total
    z <- z / total

    list(
      x = y + z / 2,
      y = z * sqrt(3) / 2
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
                        missing = getOption("isopleuros.missing")) {
    xyz <- grDevices::xyz.coords(x, xlab = xlab, ylab = ylab, zlab = zlab)
    methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z, missing = missing)
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
