# TERNARY GRID
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_grid
ternary_grid <- function(primary = NULL, secondary = NULL,
                         center = NULL, scale = NULL,
                         col.primary = "darkgray", col.secondary = "lightgray",
                         lty.primary = "dashed", lty.secondary = "dotted",
                         lwd.primary = 1, lwd.secondary = lwd.primary) {
  ## Center and scale
  if (is.null(center)) center <- get_center()
  if (is.null(scale)) scale <- get_scale()

  ## Primary grid
  if (is.null(primary) || (!anyNA(primary) && length(primary) == 1 && primary >= 1)) {
    if (is.null(primary)) primary <- graphics::par("xaxp")[3L]
    i <- seq(from = 0, to = 1, length.out = primary + 1)
    .ternary_grid(i, center = center, scale = scale,
                  col = col.primary, lty = lty.primary, lwd = lwd.primary)
  }

  ## Secondary grid
  if (!is.null(secondary) && !is.na(secondary) && length(secondary) == 1 && secondary > primary) {
    i <- seq(from = 0, to = 1, length.out = secondary + 1)
    .ternary_grid(i, center = center, scale = scale,
                  col = col.secondary, lty = lty.secondary, lwd = lwd.secondary)
  }

  invisible(NULL)
}

.ternary_grid <- function(x, center = NULL, scale = NULL,
                          col = "lightgray", lty = "dotted", lwd = 1, n = 100) {
  ## Reset values if needed
  if (!is.null(center) && all(center == 1)) center <- NULL
  if (!is.null(scale) && scale == 1) scale <- NULL

  x <- x[!(x == 0 | x == 1)]
  if (is.null(scale)) {
    for (i in x) {
      start <- matrix(data = c(i, 0, 1 - i, 1 - i, i, 0, 0, 1 - i, i), ncol = 3)
      end <- matrix(data = c(i, 1 - i, 0, 0, i, 1 - i, 1 - i, 0, i), ncol = 3)

      start <- coordinates_ternary(start, center = center)
      end <- coordinates_ternary(end, center = center)

      graphics::segments(
        x0 = start$x, x1 = end$x,
        y0 = start$y, y1 = end$y,
        lty = lty, lwd = lwd, col = col
      )
    }
  } else {
    for (i in x) {
      start <- matrix(data = c(i, 0, 1 - i, 1 - i, i, 0, 0, 1 - i, i), ncol = 3)
      end <- matrix(data = c(i, 1 - i, 0, 0, i, 1 - i, 1 - i, 0, i), ncol = 3)
      start <- list(x = start[, 2] + start[, 3] / 2, y = start[, 3] * sqrt(3) / 2)
      end <- list(x = end[, 2] + end[, 3] / 2, y = end[, 3] * sqrt(3) / 2)

      mapply(
        FUN = function(x_from, x_to, y_from, y_to, n, center, scale) {
          x <- seq(x_from, x_to, length.out = n)
          y <- seq(y_from, y_to, length.out = n)
          z <- coordinates_cartesian(x, y)
          zz <- coordinates_ternary(z, center = center, scale = scale)
          graphics::lines(
            zz,
            lty = lty, lwd = lwd, col = col
          )
        },
        x_from = start$x, x_to = end$x,
        y_from = start$y, y_to = end$y,
        MoreArgs = list(n = 100, center = center, scale = scale)
      )
    }
  }
}
