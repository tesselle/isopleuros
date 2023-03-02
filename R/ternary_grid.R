# TERNARY GRID
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_grid
ternary_grid <- function(primary = NULL, secondary = NULL,
                         col.primary = "darkgray", col.secondary = "lightgray",
                         lty.primary = "dashed", lty.secondary = "dotted",
                         lwd.primary = 1, lwd.secondary = lwd.primary) {

  ## Primary grid
  if (is.null(primary) || (!is.na(primary) && primary >= 1)) {
    if (is.null(primary)) primary <- graphics::par("xaxp")[3L]
    i <- seq(from = 0, to = 1, length.out = primary + 1)
    .ternary_grid(i, col = col.primary, lty = lty.primary, lwd = lwd.primary)
  }

  ## Secondary grid
  if (!is.null(secondary) && !is.na(secondary) && secondary > primary) {
    i <- seq(from = 0, to = 1, length.out = secondary + 1)
    .ternary_grid(i, col = col.secondary, lty = lty.secondary, lwd = lwd.secondary)
  }

  invisible()
}

.ternary_grid <- function(x, col = "lightgray", lty = "dotted", lwd = 1) {
  x <- x[-c(1, length(x))]
  for(i in x) {
    graphics::segments(
      x0 = 1 - i, x1 = (1 - i) / 2,
      y0 = 0,     y1 = c(1 - i) * .top,
      lty = lty, lwd = lwd, col = col
    )
    graphics::segments(
      x0 = 1 - i, x1 = 1 - i + i / 2,
      y0 = 0,     y1 = i * .top,
      lty = lty, lwd = lwd, col = col
    )
    graphics::segments(
      x0 = i / 2,    x1 = 1 - i + i / 2,
      y0 = i * .top, y1 = i * .top,
      lty = lty, lwd = lwd, col = col
    )
  }
}
