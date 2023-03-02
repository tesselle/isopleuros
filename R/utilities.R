# HELPERS

`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
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
