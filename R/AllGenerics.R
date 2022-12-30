# GENERIC METHODS
#' @importFrom methods setGeneric setMethod .valueClassTest
NULL

# Coordinates ==================================================================
#' Ternary Coordinates
#'
#' Computes ternary coordinates.
#' @param x,y,z A [`numeric`] vector giving the x, y and z cartesian coordinates
#'  of a set of points.
#'  If `y` and `z` are missing, an attempt is made to interpret `x` in a
#'  suitable way (see [grDevices::xyz.coords()]).
#' @param ... Currently not used.
#' @return A [`list`].
#' @example inst/examples/ex-coordinates.R
#' @author N. Frerebeau
#' @docType methods
#' @family coordinates
#' @aliases coordinates_ternary-method
#' @keywords internal
setGeneric(
  name = "coordinates_ternary",
  def = function(x, y, z, ...) standardGeneric("coordinates_ternary"),
  valueClass = "list"
)

#' Cartesian Coordinates
#'
#' Computes cartesian coordinates.
#' @param x,y A [`numeric`] vector giving the x and y ternary coordinates of a
#'  set of points. If `y` is missing, an attempt is made to interpret `x` in a
#'  suitable way (see [grDevices::xy.coords()]).
#' @param ... Currently not used.
#' @return A [`list`].
#' @example inst/examples/ex-coordinates.R
#' @author N. Frerebeau
#' @docType methods
#' @family coordinates
#' @aliases coordinates_cartesian-method
#' @keywords internal
setGeneric(
  name = "coordinates_cartesian",
  def = function(x, y, z, ...) standardGeneric("coordinates_cartesian"),
  valueClass = "list"
)

# Plot =========================================================================
#' Ternary Plot
#'
#' Produces a ternary plot.
#' @param x,y,z A [`numeric`] vector giving the x, y and z cartesian coordinates
#'  of a set of points.
#'  If `y` and `z` are missing, an attempt is made to interpret `x` in a
#'  suitable way (see [grDevices::xyz.coords()]).
#' @param xlim A length-two [`numeric`] vector giving the `x` limits in the
#'  range \eqn{[0,1]}.
#' @param ylim A length-two [`numeric`] vector giving the `y` limits in the
#'  range \eqn{[0,1]}.
#' @param zlim A length-two [`numeric`] vector giving the `z` limits in the
#'  range \eqn{[0,1]}.
#' @param xlab,ylab,zlab A [`character`] string giving a label for the x, y and
#'  z axes.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Other [graphical parameters][graphics::par] (see [graphics::par()]).
#' @return
#'  `ternary_plot()` is called it for its side-effects: it results in a graphic
#'  being displayed.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @aliases ternary_plot-method
setGeneric(
  name = "ternary_plot",
  def = function(x, y, z, ...) standardGeneric("ternary_plot")
)

#' Add Grid to a Ternary Plot
#'
#' Adds a triangular grid to an existing plot.
#' @param primary An [`integer`] specifying the number of cells of the primary
#'  grid in `x`, `y` and `z` direction.
#' @param secondary An [`integer`] specifying the number of cells of the
#'  secondary grid in `x`, `y` and `z` direction.
#' @param col.primary,col.secondary A [`character`] string specifying the color
#'  of the grid lines.
#' @param lty.primary,lty.secondary A [`character`] string or [`numeric`]
#'  value specifying the line type of the grid lines.
#' @param lwd.primary,lwd.secondary A non-negative [`numeric`] value specifying
#'  the line width of the grid lines.
#' @return
#'  `ternary_grid()` is called it for its side-effects.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name ternary_grid
NULL

#' Add an Axis to a Ternary Plot
#'
#' Adds an axis to the current plot.
#' @param side An [`integer`] specifying which side of the plot the axis is to
#'  be drawn on. The axis is placed as follows: 1=below, 2=right and 3=left.
#' @param at A [`numeric`] vector giving the points at which tick-marks are to
#'  be drawn.
#' @param labels A [`logical`] scalar specifying whether (numerical) annotations
#'  are to be made at the tickmarks, or a [`character`] vector of labels to be
#'  placed at the tickpoints. If this is not `logical`, `at` should also be
#'  supplied and of the same length.
#' @param tick A [`logical`] scalar: should tickmarks and an axis line be drawn?
#' @param font font for text. Defaults to `par("font.axis")`.
#' @param lty A [`character`] string or [`numeric`] value specifying the line
#'  type for both the axis line and the tick marks.
#' @param lwd,lwd.ticks A non-negative [`numeric`] value specifying the line
#'  widths for the axis line and the tick marks.
#' @param col,col.ticks Colors for the axis line and the tick marks
#'  respectively. Defaults to `par("col.axis")`.
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function, particularly, `cex.axis`, `col.axis` and
#'  `font.axis` for axis annotation.
#' @return
#'  `ternary_axis()` is called it for its side-effects.
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name ternary_axis
NULL