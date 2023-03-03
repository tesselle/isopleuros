# TERNARY AXES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_axis
ternary_axis <- function(side, at = NULL, labels = TRUE, tick = TRUE,
                         font = NA, lty = "solid",
                         lwd = 1, lwd.ticks = lwd,
                         col = NULL, col.ticks = NULL, ...) {

  ## Graphical parameters
  if (is.na(font)) font <- list(...)$font.axis %||% graphics::par("font.axis")
  if (is.null(col)) col <- list(...)$col.axis %||% graphics::par("col.axis")
  if (is.null(col.ticks)) col.ticks <- col
  cex <- list(...)$cex.axis %||% graphics::par("cex.axis")
  tcl <- list(...)$tcl %||% graphics::par("tcl")

  ## Ticks and labels position
  if (is.null(at)) {
    at <- seq(from = 0, to = 1, length.out = graphics::par("xaxp")[3L] + 1)
    at <- at[-c(1, length(at))]
  }

  axis_degree <- c(120, 240, 0)[side]
  axis_radian <- c(0, 2 * pi / 3, 4 * pi / 3)[side]

  tick_start <- matrix(c(at, rep(0, length(at))), ncol = 2)
  tick_start <- rotate(t(tick_start), theta = axis_radian)

  h <- abs(tcl * graphics::strheight("1", cex = 1))
  dx <- sin(pi / 6) * h
  dy <- cos(pi / 6) * h
  tick_end <- matrix(c(at + dx, rep(-dy, length(at))), ncol = 2)
  tick_end <- rotate(t(tick_end), theta = axis_radian)

  ## Labels
  if (labels) {
    if (length(labels) != length(at)) labels <- round(at * 100)
    graphics::text(x = tick_end, label = rev(labels), srt = axis_degree,
                   cex = cex, col = col, font = font, adj = c(1, 0.5))
  }

  ## Ticks
  if (tick) {
    axis_line <- rotate(matrix(c(0, 0, 1, 0), ncol = 2), theta = axis_radian)
    graphics::segments(
      x0 = axis_line[1, 1], x1 = axis_line[2, 1],
      y0 = axis_line[1, 2], y1 = axis_line[2, 2],
      col = col, lwd = lwd, lty = lty
    )
    graphics::segments(
      x0 = tick_start[, 1], x1 = tick_end[, 1],
      y0 = tick_start[, 2], y1 = tick_end[, 2],
      col = col.ticks, lwd = lwd.ticks, lty = lty
    )
  }

  invisible()
}
