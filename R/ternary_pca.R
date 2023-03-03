# PCA
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_pca
#' @aliases ternary_pca,numeric,numeric,numeric-method
setMethod(
  f = "ternary_pca",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, axis = 1, ...) {
    ## CLR
    coda <- cbind(x, y, z)
    ratio <- clr(coda)

    z <- ratio - rowMeans(ratio) # Center
    m <- colMeans(z)

    ## Get eigenvectors
    eig <- eigen(stats::cov(z))$vectors[, axis[[1L]]] + m

    ## Standard coordinates
    std <- z %*% eig

    lam <- seq(-5, 5, length.out = nrow(ratio))
    axe <- cbind(eig[1L] * lam, eig[2L] * lam, eig[3L] * lam) +
      cbind(m[1L] * (1 - lam), m[2L] * (1 - lam), m[3L] * (1 - lam))

    ## Inverse CLR
    axe <- clr_inv(axe)
    coords <- coordinates_ternary(axe)

    ## Plot
    graphics::lines(coords, ...)
  }
)

#' @export
#' @rdname ternary_pca
#' @aliases ternary_pca,ANY,missing,missing-method
setMethod(
  f = "ternary_pca",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, axis = 1, ...) {
    x <- grDevices::xyz.coords(x)
    methods::callGeneric(x = x$x, y = x$y, z = x$z, axis = axis, ...)
  }
)
