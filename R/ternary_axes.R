# TERNARY AXES
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_axis
ternary_axis <- function(side, at = NULL, labels = TRUE, tick = TRUE,
                         center = getOption("isopleuros.center"),
                         scale = getOption("isopleuros.scale"),
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
    at <- at[!(at == 0 | at == 1)]
  }

  axis_degree <- c(120, 240, 0)[side]
  axis_radian <- c(0, 2 * pi / 3, 4 * pi / 3)[side]

  pos <- matrix(data = 0, nrow = length(at), ncol = 3)
  pos[, side] <- at
  pos[, c(2, 3, 1)[side]] <- 1 - at
  pos <- coordinates_ternary(pos, center = center, scale = scale)

  h <- abs(tcl * graphics::strheight("1", cex = 1))
  dx <- sin(pi / 6) * h
  dy <- cos(pi / 6) * h
  tick_start <- matrix(c(pos$x, pos$y), ncol = 2)
  tick_end <- matrix(c(pos$x + dx * c(1, 1, -2)[side],
                       pos$y + dy * c(-1, 1, 0)[side]), ncol = 2)

  ## Labels
  if (!isFALSE(labels)) {
    if (length(labels) != length(at)) labels <- round(at * 100)
    graphics::text(x = tick_end, label = labels, srt = axis_degree,
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
