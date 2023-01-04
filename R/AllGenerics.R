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
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
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
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
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
#' @example inst/examples/ex-axis.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name ternary_axis
NULL

#' Ternary Plot Annotation
#'
#' @param main A [`character`] string specifying the main title (on top).
#' @param sub A [`character`] string specifying the sub-title (at bottom).
#' @param xlab,ylab,zlab A [`character`] string giving a label for the x, y and
#'  z axes.
#' @param line Specifying a value for `line` overrides the default placement of
#'  labels, and places them this many lines outwards from the plot edge.
#' @param outer A [`logical`] scalar: should the titles be placed in the outer
#'  margins of the plot?
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function, particularly, `font.main`, `cex.main` and
#'  `col.main`, `font.sub`, `cex.sub` and `col.sub` for title annotation;
#'  `font.lab`, `cex.lab` and `col.lab` for axis label.
#' @return
#'  `ternary_title()` is called it for its side-effects.
#' @example inst/examples/ex-title.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
#' @name ternary_title
NULL

# Geometry =====================================================================
#' Add Arrows to a Ternary Plot
#'
#' Draw arrows between pairs of points.
#' @param x0,y0,z0 A [`numeric`] vector giving the x, y and z ternary
#'  coordinates of points from which to draw.
#' @param x1,y1,z1 A [`numeric`] vector giving the x, y and z ternary
#'  coordinates of points to which to draw.
#' @param ... Further arguments to be passed to [graphics::arrows()].
#' @return
#'  `ternary_arrows()` is called it for its side-effects.
#' @seealso [graphics::arrows()]
#' @example inst/examples/ex-arrows.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
setGeneric(
  name = "ternary_arrows",
  def = function(x0, y0, z0, ...) standardGeneric("ternary_arrows")
)

#' Add Connected Line Segments to a Ternary Plot
#'
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param type A [`character`] string indicating the type of plotting; actually
#'  any of the types as in [graphics::plot.default()].
#' @param ... Further graphical parameters (see [graphics::par()]) may also be
#'  supplied as arguments, particularly, line type, `lty`, line width, `lwd`,
#'  color, `col` and for `type = "b"`, `pch`. Also the line characteristics
#'  `lend`, `ljoin` and `lmitre`.
#' @return
#'  `ternary_lines()` is called it for its side-effects.
#' @seealso [graphics::lines()]
#' @example inst/examples/ex-lines.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
setGeneric(
  name = "ternary_lines",
  def = function(x, y, z, ...) standardGeneric("ternary_lines")
)

#' Add Points to a Ternary Plot
#'
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param type A [`character`] string indicating the type of plotting; actually
#'  any of the types as in [graphics::plot.default()].
#' @param ... Further graphical parameters (see [graphics::par()]) may also be
#'  supplied as arguments, particularly, plotting character, `pch`, character
#'  expansion, `cex` and color, `col`.
#' @return
#'  `ternary_points()` is called it for its side-effects.
#' @seealso [graphics::points()]
#' @example inst/examples/ex-points.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
setGeneric(
  name = "ternary_points",
  def = function(x, y, z, ...) standardGeneric("ternary_points")
)

#' Polygon Drawing
#'
#' Draws the polygons whose vertices are given in `x`, `y` and `z`.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param ... Further arguments to be passed to [graphics::polygon()].
#' @return
#'  `ternary_polygon()` is called it for its side-effects.
#' @seealso [graphics::polygon()]
#' @example inst/examples/ex-polygon.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
setGeneric(
  name = "ternary_polygon",
  def = function(x, y, z, ...) standardGeneric("ternary_polygon")
)

#' Add Text to a Ternary Plot
#'
#' Draws the strings given in the vector `labels` at the coordinates given by
#' `x`, `y` and `z`.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param labels A [`character`] vector or [`expression`] specifying the text
#'  to be written.
#' @param ... Further arguments to be passed to [graphics::text()].
#' @return
#'  `ternary_text()` is called it for its side-effects.
#' @seealso [graphics::text()]
#' @example inst/examples/ex-text.R
#' @author N. Frerebeau
#' @docType methods
#' @family plot methods
setGeneric(
  name = "ternary_text",
  def = function(x, y, z, ...) standardGeneric("ternary_text")
)

# Statistics ===================================================================
#' Add an Ellipse to a Ternary Plot
#'
#' Computes and draws a confidence/tolerance ellipse.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param radius A [`numeric`] vector specifying the scaling of the
#'  half-diameters.
#' @param level A [`numeric`] vector specifying the confidence/tolerance level.
#' @param ... Further arguments to be passed to [graphics::polygon()].
#' @return
#'  `ternary_ellipse()` is called it for its side-effects.
#' @seealso [graphics::polygon()]
#' @example inst/examples/ex-ellipse.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
setGeneric(
  name = "ternary_ellipse",
  def = function(x, y, z, ...) standardGeneric("ternary_ellipse")
)

#' @rdname ternary_ellipse
setGeneric(
  name = "ternary_confidence",
  def = function(x, y, z, ...) standardGeneric("ternary_confidence")
)

#' @rdname ternary_ellipse
setGeneric(
  name = "ternary_tolerance",
  def = function(x, y, z, ...) standardGeneric("ternary_tolerance")
)

#' Convex Hull of a Set of Points
#'
#' Computes and draws the convex hull of the set of points specified.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param ... Further arguments to be passed to [graphics::polygon()].
#' @return
#'  `ternary_hull()` is called it for its side-effects.
#' @seealso [grDevices::chull()], [graphics::polygon()]
#' @example inst/examples/ex-hull.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
setGeneric(
  name = "ternary_hull",
  def = function(x, y, z, ...) standardGeneric("ternary_hull")
)
