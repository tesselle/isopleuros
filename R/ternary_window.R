# TERNARY WINDOW
#' @include AllGenerics.R
NULL

#' Set up Ternary Coordinates for Graphics Window
#'
#' @param xlim A length-three [`numeric`] vector giving the `x` limits in the
#'  range \eqn{[0,1]}.
#' @param ylim A length-three [`numeric`] vector giving the `y` limits in the
#'  range \eqn{[0,1]}.
#' @param zlim A length-three [`numeric`] vector giving the `z` limits in the
#'  range \eqn{[0,1]}.
#' @param xlab,ylab,zlab A [`character`] string giving a label for the x, y and
#'  z axes.
#' @return
#'  `ternary_window()` is called it for its side-effects
#'  (see [graphics::plot.window()]).
#' @keywords internal
#' @noRd
ternary_window <- function(xlim = NULL, ylim = NULL, zlim = NULL,
                           xlab = NULL, ylab = NULL, zlab = NULL,
                           cex = 1) {

  n_null <- is.null(xlim) + is.null(ylim) + is.null(zlim)

  if (n_null == 3) {
    dx <- max(graphics::strwidth(c(xlab, ylab, zlab), cex = cex)) / 2
    rx <- expand_range(c(0, 1), add = dx)
    lim <- list(
      x = rx,
      y = .top / 2 + c(-1, 1) * diff(rx) / 2
    )
  }
  if (n_null == 2) {
    stop(tr_("You must provide at least two coordinates ranges."), call. = FALSE)
  }
  if (n_null < 2) {
    xlims <- if (is.null(xlim)) 1 - ylim - zlim else xlim
    ylims <- if (is.null(ylim)) 1 - xlim - zlim else ylim
    zlims <- if (is.null(zlim)) 1 - xlim - ylim else zlim

    assert_length(xlims, 3)
    assert_length(ylims, 3)
    assert_length(zlims, 3)

    lim <- coordinates_ternary(abs(xlims), abs(ylims), abs(zlims))
    lim$x <- range(lim$x)
    lim$y <- range(lim$y)
  }
  graphics::plot.window(xlim = lim$x, ylim = lim$y, asp = 1)
}
