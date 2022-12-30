# TERNARY PLOT
#' @include AllGenerics.R
NULL

.top <- sqrt(3) / 2

# Plot =========================================================================
#' @export
#' @rdname ternary_plot
#' @aliases ternary_plot,numeric,numeric,numeric-method
setMethod(
  f = "ternary_plot",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z,
                        xlim = NULL, ylim = NULL, zlim = NULL,
                        xlab = NULL, ylab = NULL, zlab = NULL,
                        main = NULL, sub = NULL,
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL, ...) {

    ## Save and restore graphical parameters
    old_par <- graphics::par(mar = c(1, 1, 1, 1), pty = "s", no.readonly = TRUE)
    on.exit(graphics::par(old_par))

    ## Zoom
    if (is.null(xlim) && is.null(ylim) && is.null(zlim)) {
      lim <- list(x = c(-0.05, 1.05), y = c(-0.05, 1.05))
    } else {
      xrange <- yrange <- zrange <- NULL
      if (!is.null(xlim)) xrange <- boundary(1, xlim)
      if (!is.null(ylim)) yrange <- boundary(2, ylim)
      if (!is.null(zlim)) zrange <- boundary(3, zlim)
      lim <- coordinates_ternary(rbind(xrange, yrange, zrange))
      lim$x <- range(lim$x)
      lim$y <- range(lim$y)
    }

    ## Graphical parameters
    fg <- list(...)$fg %||% graphics::par("fg")
    col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
    cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")

    ## Draw triangle
    graphics::plot(NULL, xlim = lim$x, ylim = lim$y, main = main, sub = sub,
                   xlab = "", ylab = "", axes = FALSE, asp = 1)

    panel.first

    if (frame.plot) {
      graphics::polygon(x = c(0, 0.5, 1), y = c(0, .top, 0),
                        border = fg, lty = "solid", lwd = 1)
    }

    if (axes) {
      ternary_axis(side = 1, col = fg)
      ternary_axis(side = 2, col = fg)
      ternary_axis(side = 3, col = fg)
    }

    if (!is.null(xlab)) {
      graphics::text(x = 0, y = 0, label = xlab, pos = 1,
                     col = col.lab, cex = cex.lab)
    }
    if (!is.null(ylab)) {
      graphics::text(x = 1, y = 0, label = ylab, pos = 1,
                     col = col.lab, cex = cex.lab)
    }
    if (!is.null(zlab)) {
      graphics::text(x = 0.5, y = .top, label = zlab, pos = 3,
                     col = col.lab, cex = cex.lab)
    }

    coords <- coordinates_ternary(x, y, z)
    graphics::points(x = coords, ...)

    panel.last

    invisible()
  }
)

#' @export
#' @rdname ternary_plot
#' @aliases ternary_plot,ANY,missing,missing-method
setMethod(
  f = "ternary_plot",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, xlim = NULL, ylim = NULL, zlim = NULL,
                        xlab = NULL, ylab = NULL, zlab = NULL,
                        main = NULL, sub = NULL,
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL, ...) {

    xyz <- grDevices::xyz.coords(x, xlab = xlab, ylab = ylab, zlab = zlab)
    methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z,
                         xlim = xlim, ylim = ylim, zlim = zlim,
                         xlab = xyz$xlab, ylab = xyz$ylab, zlab = xyz$zlab,
                         main = main, sub = sub, axes = axes,
                         frame.plot = frame.plot, panel.first = panel.first,
                         panel.last = panel.last, ...)
  }
)

# Axes =========================================================================
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

  h <- abs(tcl * graphics::par("cxy")[2L])
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

# Grid =========================================================================
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
