# TERNARY TITLE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_title
ternary_title <- function(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                          zlab = NULL, line = NA, outer = FALSE, ...) {
  ## Graphical parameters
  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")

  ## Axes labels
  xlab <- grDevices::as.graphicsAnnot(xlab)
  ylab <- grDevices::as.graphicsAnnot(ylab)
  zlab <- grDevices::as.graphicsAnnot(zlab)

  if (!is.null(xlab)) {
    graphics::text(x = 0, y = 0, label = xlab, pos = 1,
                   col = col.lab, cex = cex.lab, font = font.lab)
  }
  if (!is.null(ylab)) {
    graphics::text(x = 1, y = 0, label = ylab, pos = 1,
                   col = col.lab, cex = cex.lab, font = font.lab)
  }
  if (!is.null(zlab)) {
    graphics::text(x = 0.5, y = .top, label = zlab, pos = 3,
                   col = col.lab, cex = cex.lab, font = font.lab)
  }

  ## Title
  graphics::title(main = main, sub = sub, xlab = NULL, ylab = NULL,
                  line = line, outer = outer)

  invisible(NULL)
}
