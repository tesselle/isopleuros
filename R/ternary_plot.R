# TERNARY PLOT
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_plot
#' @aliases ternary_plot,numeric,numeric,numeric-method
setMethod(
  f = "ternary_plot",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, center = FALSE, scale = FALSE,
                        xlim = NULL, ylim = NULL, zlim = NULL,
                        xlab = NULL, ylab = NULL, zlab = NULL,
                        main = NULL, sub = NULL, ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL, ...) {

    ## Save and restore graphical parameters
    ## pty: square plotting region, independent of device size
    old_par <- graphics::par(pty = "s", no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    ## Graphical parameters
    fg <- list(...)$fg %||% graphics::par("fg")
    cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")

    ## Open new window
    grDevices::dev.hold()
    on.exit(grDevices::dev.flush(), add = TRUE)
    graphics::plot.new()

    ## Set plotting coordinates
    if (is.null(xlim) && is.null(ylim) && is.null(zlim)) {
      dx <- max(graphics::strwidth(c(xlab, ylab, zlab), cex = cex.lab)) / 2
      rx <- expand_range(c(0, 1), add = dx)
      lim <- list(
        x = rx,
        y = .top / 2 + c(-1, 1) * diff(rx) / 2
      )
    } else {
      xrange <- yrange <- zrange <- NULL
      if (!is.null(xlim)) xrange <- boundary(1, xlim)
      if (!is.null(ylim)) yrange <- boundary(2, ylim)
      if (!is.null(zlim)) zrange <- boundary(3, zlim)
      lim <- coordinates_ternary(rbind(xrange, yrange, zrange))
      lim$x <- range(lim$x)
      lim$y <- range(lim$y)
    }
    graphics::plot.window(xlim = lim$x, ylim = lim$y, asp = 1)

    ## Compute ternary coordinates
    pt <- coordinates_ternary(x = x, y = y, z = z, center = center, scale = scale)

    ## Save center and scale for further use, e.g. grid or axes.
    options(isopleuros.center = pt$center)
    options(isopleuros.scale = pt$scale)

    ## Evaluate pre-plot expressions
    panel.first

    ## Plot
    graphics::points(x = pt, ...)

    ## Evaluate post-plot and pre-axis expressions
    panel.last

    ## Construct Axis
    if (axes) {
      ternary_axis(side = 1, center = pt$center, scale = pt$scale, col = fg)
      ternary_axis(side = 2, center = pt$center, scale = pt$scale, col = fg)
      ternary_axis(side = 3, center = pt$center, scale = pt$scale, col = fg)
    }

    ## Plot frame
    if (frame.plot) {
      ternary_box(lty = "solid", lwd = 1, col = fg)
    }

    ## Add annotation
    if (ann) {
      ternary_title(main = main, sub = sub, xlab = xlab, ylab = ylab,
                    zlab = zlab, ...)
    }

    invisible(pt)
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
                        main = NULL, sub = NULL, ann = graphics::par("ann"),
                        axes = TRUE, frame.plot = axes,
                        panel.first = NULL, panel.last = NULL, ...) {

    xyz <- grDevices::xyz.coords(x, xlab = xlab, ylab = ylab, zlab = zlab)
    pt <- methods::callGeneric(
      x = xyz$x, y = xyz$y, z = xyz$z,
      xlim = xlim,
      ylim = ylim,
      zlim = zlim,
      xlab = xlab %||% xyz$xlab %||% "x",
      ylab = ylab %||% xyz$ylab %||% "y",
      zlab = zlab %||% xyz$zlab %||% "z",
      main = main, sub = sub, ann = ann,
      axes = axes,
      frame.plot = frame.plot,
      panel.first = panel.first,
      panel.last = panel.last,
      ...
    )

    invisible(pt)
  }
)

boundary <- function(side, limits) {
  limits <- range(limits)
  side_min <- diag(1, 3, 3) - limits[1L]
  side_max <- diag(1, 3, 3) - limits[2L]
  side_range <- rbind(side_min, side_max)
  side_range[side_range < 0] <- 0
  side_range[, side] <- rep(limits, each = 3)
  side_range[rowSums(side_range) == 1, ]
}
