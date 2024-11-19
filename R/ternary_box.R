# TERNARY BOX
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_box
ternary_box <- function(lty = "solid", ...) {

  ## Graphical parameters
  col <- list(...)$col %||% graphics::par("fg")
  lwd <- list(...)$lwd %||% 1

  graphics::polygon(x = c(0, 0.5, 1), y = c(0, .top, 0),
                    border = col, lty = lty, lwd = lwd)

  invisible(NULL)
}
