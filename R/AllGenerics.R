# GENERIC METHODS

.top <- sqrt(3) / 2

# Coordinates ==================================================================
#' Ternary Coordinates
#'
#' Computes ternary coordinates.
#' @param x,y,z A [`numeric`] vector giving the x, y and z cartesian coordinates
#'  of a set of points.
#'  If `y` and `z` are missing, an attempt is made to interpret `x` in a
#'  suitable way (see [grDevices::xyz.coords()]).
#' @param center A [`logical`] scalar or a [`numeric`] vector giving the center.
#' @param scale A [`logical`] scalar or a length-one [`numeric`] vector giving a
#'  scaling factor.
#' @param xlab,ylab,zlab A [`character`] string specifying the names for the x,
#'  y and z variables to be extracted.
#' @param missing A [`logical`] scalar: should [missing values][NA] be replaced
#'  with zeros before the computation proceeds? If `FALSE` (the default),
#'  incomplete cases are removed.
#' @param ... Currently not used.
#' @return
#'  A [`list`] with the components:
#'  \tabular{ll}{
#'   `x` \tab A [`numeric`] vector of x coordinates. \cr
#'   `y` \tab A [`numeric`] vector of y coordinates. \cr
#'   `center` \tab A [`numeric`] vector giving the center. \cr
#'   `scale` \tab A [`numeric`] vector giving the scale factor. \cr
#'  }
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
#' @param xlab,ylab A [`character`] string specifying the names for the x and y
#'  variables to be extracted.
#' @param ... Currently not used.
#' @return
#'  A [`list`] with the components:
#'  \tabular{ll}{
#'   `x` \tab A [`numeric`] vector of x coordinates. \cr
#'   `y` \tab A [`numeric`] vector of y coordinates. \cr
#'   `z` \tab A [`numeric`] vector of z coordinates. \cr
#'  }
#' @example inst/examples/ex-coordinates.R
#' @author N. Frerebeau
#' @docType methods
#' @family coordinates
#' @aliases coordinates_cartesian-method
#' @keywords internal
setGeneric(
  name = "coordinates_cartesian",
  def = function(x, y, ...) standardGeneric("coordinates_cartesian"),
  valueClass = "list"
)

# Plot =========================================================================
#' Ternary Plot
#'
#' Produces a ternary plot.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param center A [`logical`] scalar: should the data be centered?
#' @param scale A [`logical`] scalar: should the data be scaled?
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
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function.
#' @return
#'  `ternary_plot()` is called it for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns a [`list`] with the components:
#'  \tabular{ll}{
#'   `x` \tab A [`numeric`] vector of x values. \cr
#'   `y` \tab A [`numeric`] vector of y values. \cr
#'   `z` \tab A [`numeric`] vector of z values. \cr
#'   `center` \tab A [`numeric`] vector giving the center. \cr
#'   `scale` \tab A [`numeric`] vector giving the scale factor. \cr
#'  }
#' @example inst/examples/ex-plot.R
#' @author N. Frerebeau
#' @docType methods
#' @family graphical elements
#' @aliases ternary_plot-method
setGeneric(
  name = "ternary_plot",
  def = function(x, y, z, ...) standardGeneric("ternary_plot")
)

## Grid ------------------------------------------------------------------------
#' Add Grid to a Ternary Plot
#'
#' Adds a triangular grid to an existing plot.
#' @param primary An [`integer`] specifying the number of cells of the primary
#'  grid in `x`, `y` and `z` direction.
#' @param secondary An [`integer`] specifying the number of cells of the
#'  secondary grid in `x`, `y` and `z` direction.
#' @param center A [`numeric`] vector giving the center. If `NULL`
#'  (the default), data are assumed not centered.
#' @param scale A [`numeric`] vector giving the scale factor. If `NULL`
#'  (the default), data are assumed not scaled.
#' @param col.primary,col.secondary A [`character`] string specifying the color
#'  of the grid lines.
#' @param lty.primary,lty.secondary A [`character`] string or [`numeric`]
#'  value specifying the line type of the grid lines.
#' @param lwd.primary,lwd.secondary A non-negative [`numeric`] value specifying
#'  the line width of the grid lines.
#' @return
#'  `ternary_grid()` is called it for its side-effects.
#' @example inst/examples/ex-scale.R
#' @author N. Frerebeau
#' @docType methods
#' @family graphical elements
#' @name ternary_grid
NULL

## Axis ------------------------------------------------------------------------
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
#' @param center A [`numeric`] vector giving the center. If `NULL`
#'  (the default), data are assumed not centered.
#' @param scale A [`numeric`] vector giving the scale factor. If `NULL`
#'  (the default), data are assumed not scaled.
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
#' @family graphical elements
#' @name ternary_axis
NULL

## Annotation ------------------------------------------------------------------
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
#'  arguments to this function, particularly, `font.main`, `cex.main`,
#'  `col.main` and `font.sub`, `cex.sub`, `col.sub` for title annotation;
#'  `font.lab`, `cex.lab` and `col.lab` for axis label.
#' @return
#'  `ternary_title()` is called it for its side-effects.
#' @example inst/examples/ex-title.R
#' @author N. Frerebeau
#' @docType methods
#' @family graphical elements
#' @name ternary_title
NULL

## Box -------------------------------------------------------------------------
#' Draw a Box around a Ternary Plot
#'
#' @param lty A [`character`] string or [`numeric`] value specifying the line
#'  type of the box.
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function, particularly, `col` or `lwd`.
#' @return
#'  `ternary_box()` is called it for its side-effects.
#' @example inst/examples/ex-axis.R
#' @author N. Frerebeau
#' @docType methods
#' @family graphical elements
#' @name ternary_box
NULL

## Pairs -----------------------------------------------------------------------
#' Ternary Plot Matrices
#'
#' Produces a matrix of ternary plots.
#' @param x A [`matrix`] or a [`data.frame`]. Columns are converted to `numeric`
#'  in the same way that [data.matrix()] does.
#' @param margin A [`character`] string or an [`integer`] giving the index of
#'  the column to be used as the third part of the ternary plots. If `NULL`
#'  (the default), marginal compositions will be used (i.e. the geometric mean
#'  of the non-selected parts).
#' @param ... Further [graphical parameters][graphics::par()].
#' @return
#'  `ternary_pairs()` is called it for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-pairs.R
#' @author N. Frerebeau
#' @docType methods
#' @family graphical elements
#' @aliases ternary_pairs-method
setGeneric(
  name = "ternary_pairs",
  def = function(x, ...) standardGeneric("ternary_pairs")
)

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
#' @family geometries
#' @aliases ternary_arrows-method
setGeneric(
  name = "ternary_arrows",
  def = function(x0, y0, z0, ...) standardGeneric("ternary_arrows")
)

#' Add Line Segments to a Ternary Plot
#'
#' Draw line segments between pairs of points.
#' @param x0,y0,z0 A [`numeric`] vector giving the x, y and z ternary
#'  coordinates of points from which to draw.
#' @param x1,y1,z1 A [`numeric`] vector giving the x, y and z ternary
#'  coordinates of points to which to draw.
#' @param ... Further graphical parameters (see [graphics::par()]) may also be
#'  supplied as arguments, particularly, line type, `lty`, line width, `lwd` and
#'  color, `col`. Also the line characteristics `lend`, `ljoin` and `lmitre`.
#' @return
#'  `ternary_segments()` is called it for its side-effects.
#' @seealso [graphics::segments()]
#' @example inst/examples/ex-segments.R
#' @author N. Frerebeau
#' @docType methods
#' @family geometries
#' @aliases ternary_segments-method
setGeneric(
  name = "ternary_segments",
  def = function(x0, y0, z0, ...) standardGeneric("ternary_segments")
)

#' Add Cross-Hairs to a Ternary Plot
#'
#' Draw lines that intersect at a point.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param x_mark,y_mark,z_mark A [`logical`] scalar: should the `x`, `y` or `z`
#'  axis component be drawn?
#' @param ... Further graphical parameters (see [graphics::par()]) may also be
#'  supplied as arguments, particularly, line type, `lty`, line width, `lwd` and
#'  color, `col`. Also the line characteristics `lend`, `ljoin` and `lmitre`.
#' @return
#'  `ternary_crosshairs()` is called it for its side-effects.
#' @example inst/examples/ex-crosshairs.R
#' @author N. Frerebeau
#' @docType methods
#' @family geometries
#' @aliases ternary_crosshairs-method
setGeneric(
  name = "ternary_crosshairs",
  def = function(x, y, z, ...) standardGeneric("ternary_crosshairs")
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
#' @family geometries
#' @aliases ternary_lines-method
setGeneric(
  name = "ternary_lines",
  def = function(x, y, z, ...) standardGeneric("ternary_lines")
)

#' Add Points to a Ternary Plot
#'
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param center A [`logical`] scalar specifying wether the data should be
#'  centered, or a [`numeric`] vector giving the center.
#' @param scale A [`logical`] scalar specifying wether the data should be
#'  scaled, or a [`numeric`] vector giving the scale factor.
#' @param type A [`character`] string indicating the type of plotting; actually
#'  any of the types as in [graphics::plot.default()].
#' @param ... Further graphical parameters (see [graphics::par()]) may also be
#'  supplied as arguments, particularly, plotting character, `pch`, character
#'  expansion, `cex` and color, `col`.
#' @return
#'  `ternary_points()` is called it for its side-effects. Invisibly returns
#'  a [`list`] with the components:
#'  \tabular{ll}{
#'   `x` \tab A [`numeric`] vector of x values. \cr
#'   `y` \tab A [`numeric`] vector of y values. \cr
#'   `z` \tab A [`numeric`] vector of z values. \cr
#'   `center` \tab A [`numeric`] vector giving the center. \cr
#'   `scale` \tab A [`numeric`] vector giving the scale factor. \cr
#'  }
#' @seealso [graphics::points()]
#' @example inst/examples/ex-points.R
#' @author N. Frerebeau
#' @docType methods
#' @family geometries
#' @aliases ternary_points-method
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
#' @family geometries
#' @aliases ternary_polygon-method
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
#' @param center A [`logical`] scalar specifying wether the data should be
#'  centered, or a [`numeric`] vector giving the center.
#' @param scale A [`logical`] scalar specifying wether the data should be
#'  scaled, or a [`numeric`] vector giving the scale factor.
#' @param labels A [`character`] vector or [`expression`] specifying the text
#'  to be written.
#' @param ... Further arguments to be passed to [graphics::text()].
#' @return
#'  `ternary_text()` is called it for its side-effects.
#' @seealso [graphics::text()]
#' @example inst/examples/ex-text.R
#' @author N. Frerebeau
#' @docType methods
#' @family geometries
#' @aliases ternary_text-method
setGeneric(
  name = "ternary_text",
  def = function(x, y, z, ...) standardGeneric("ternary_text")
)

#' Non-Overlapping Text Labels
#'
#' Optimize the location of text labels to minimize overplotting text.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param center A [`logical`] scalar specifying wether the data should be
#'  centered, or a [`numeric`] vector giving the center.
#' @param scale A [`logical`] scalar specifying wether the data should be
#'  scaled, or a [`numeric`] vector giving the scale factor.
#' @param labels A [`character`] vector or [`expression`] specifying the text
#'  to be written.
#' @param type A [`character`] string specifying the shape of the field.
#'  It must be one of "`text`" or "`shadow`". Any unambiguous substring
#'  can be given.
#' @param ... Further graphical parameters (see [graphics::par()]) may also be
#'  supplied as arguments, particularly, character expansion, `cex` and
#'  color, `col`.
#' @return
#'  `ternary_labels()` is called it for its side-effects.
#' @seealso [graphics::text()]
#' @example inst/examples/ex-labels.R
#' @author N. Frerebeau
#' @docType methods
#' @family geometries
#' @aliases ternary_labels-method
setGeneric(
  name = "ternary_labels",
  def = function(x, y, z, ...) standardGeneric("ternary_labels")
)

## Image -----------------------------------------------------------------------
#' Display a Color Image
#'
#' Creates a grid of colored triangles with colors corresponding to the output
#' of a function.
#' @param f A [`function`] that takes three arguments (x, y and z coordinates)
#'  and returns a `numeric` vector.
#' @param n A length-one [`integer`] vector specifying the maximum number of
#'  tiles on each axis.
#' @param palette A [`function`] that takes a single `numeric` vector
#'  (the output of `f`) as argument and returns a vector of color.
#' @param ... Further parameters to be passed to `f`.
#' @example inst/examples/ex-image.R
#' @author N. Frerebeau
#' @docType methods
#' @family geometries
#' @aliases ternary_image-method
setGeneric(
  name = "ternary_image",
  def = function(f, ...) standardGeneric("ternary_image")
)

# Statistics ===================================================================
## Ellipse ---------------------------------------------------------------------
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
#' @details
#'  Ellipse coordinates are computed after an isometric log ratio transformation
#'  of the original data.
#' @return
#'  `ternary_ellipse()` is called it for its side-effects.
#' @seealso [graphics::polygon()]
#' @example inst/examples/ex-ellipse.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases ternary_ellipse-method
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

## Convex hull -----------------------------------------------------------------
#' Convex Hull of a Set of Points
#'
#' Computes and draws the convex hull of the set of points specified.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param center A [`logical`] scalar specifying wether the data should be
#'  centered, or a [`numeric`] vector giving the center.
#' @param scale A [`logical`] scalar specifying wether the data should be
#'  scaled, or a [`numeric`] vector giving the scale factor.
#' @param ... Further arguments to be passed to [graphics::polygon()].
#' @return
#'  `ternary_hull()` is called it for its side-effects.
#' @seealso [grDevices::chull()], [graphics::polygon()]
#' @example inst/examples/ex-hull.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases ternary_hull-method
setGeneric(
  name = "ternary_hull",
  def = function(x, y, z, ...) standardGeneric("ternary_hull")
)

## Contour ---------------------------------------------------------------------
#' Contour Lines
#'
#' Computes and draws contour lines.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param value A [`numeric`] vector giving the values to be plotted.
#' @param n A length-one [`numeric`] specifying the number of grid points.
#' @param nlevels A length-one [`numeric`] vector specifying the number of
#'  contour levels desired. Only used if `levels` is `NULL`.
#' @param levels A [`numeric`] vector of levels at which to draw contour lines.
#' @param palette A color palette [`function`] that takes a single integer
#'  argument (the number of levels) and returns a vector of colors.
#' @param ilr A [`logical`] scalar: should interpolation be computed in ILR
#'  space? If `FALSE`, interpolation is computed in Cartesian space.
#' @param method A [`character`] string: specifying the method for interpolation
#'  (see [interp::interp()]).
#' @param extrapolate A [`logical`] scalar: should extrapolation be used outside
#'  of the convex hull determined by the data points (see [interp::interp()])?
#' @param ... Further arguments to be passed to [ternary_lines()].
#' @details
#'  Contour are computed from a bivariate interpolation onto a grid,
#'  after an isometric log ratio transformation of the original data.
#' @return
#'  `ternary_contour()` is called it for its side-effects.
#'
#'  Invisibly returns a [`list`] with elements `levels` (the contour levels) and
#'  `colors` (the contour colors) that can be used for a legend.
#' @note
#'  The \pkg{interp} package needs to be installed on your machine.
#' @seealso [interp::interp()], [grDevices::contourLines()]
#' @example inst/examples/ex-contour.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases ternary_contour-method
setGeneric(
  name = "ternary_contour",
  def = function(x, y, z, ...) standardGeneric("ternary_contour")
)

## Density ---------------------------------------------------------------------
#' Density Contour Lines
#'
#' Computes and draws density contour lines.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param h A length-one [`numeric`] vector giving the bandwidth.
#' @param n A length-one [`numeric`] specifying the number of grid points.
#' @param nlevels A length-one [`numeric`] vector specifying the number of
#'  contour levels desired. Only used if `levels` is `NULL`.
#' @param levels A [`numeric`] vector of levels at which to draw contour lines.
#' @param palette A color palette [`function`] that takes a single integer
#'  argument (the number of levels) and returns a vector of colors.
#' @param ... Further arguments to be passed to [ternary_lines()].
#' @details
#'  Two-dimensional kernel density estimation with an axis-aligned bivariate
#'  normal kernel. Normal kernel is evaluated on a square grid, after an
#'  isometric log ratio transformation of the original data.
#' @return
#'  `ternary_density()` is called it for its side-effects.
#'
#'  Invisibly returns a [`list`] with elements `levels` (the contour levels) and
#'  `colors` (the contour colors) that can be used for a legend.
#' @note
#'  **This must be considered as experimental and subject to major changes
#'  in a future release.**
#' @source
#'  Two-dimensional kernel density estimation is adapted from [`MASS::kde2d()`].
#' @seealso [grDevices::contourLines()]
#' @example inst/examples/ex-density.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases ternary_density-method
setGeneric(
  name = "ternary_density",
  def = function(x, y, z, ...) standardGeneric("ternary_density")
)

## Mean ------------------------------------------------------------------------
#' Compositional Mean
#'
#' Computes and draws the closed geometric mean of the set of points specified.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param ... Further arguments to be passed to [graphics::points()].
#' @return
#'  `ternary_mean()` is called it for its side-effects.
#' @example inst/examples/ex-mean.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases ternary_mean-method
setGeneric(
  name = "ternary_mean",
  def = function(x, y, z, ...) standardGeneric("ternary_mean")
)

## PCA -------------------------------------------------------------------------
#' Principal Component Analysis
#'
#' Computes and draws principal component.
#' @param x,y,z A [`numeric`] vector giving the x, y and z ternary coordinates
#'  of a set of points. If `y` and `z` are missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xyz.coords()]).
#' @param axis An [`integer`] specifying the dimension to be plotted.
#' @param ... Further arguments to be passed to [graphics::lines()].
#' @return
#'  `ternary_pca()` is called it for its side-effects.
#' @example inst/examples/ex-pca.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @aliases ternary_pca-method
setGeneric(
  name = "ternary_pca",
  def = function(x, y, z, ...) standardGeneric("ternary_pca")
)

# Chart ========================================================================
#' Ceramic Phase Diagram
#'
#' @param labels A [`logical`] scalar: should labels be displayed?
#' @param symbol A [`logical`] scalar: should symbol be used instead of full
#'  labels? Only used if `labels` is `TRUE`.
#' @param mol A [`logical`] scalar: should molarity be used instead of molar
#'  mass?
#' @param ... Further arguments to be passed to [graphics::polygon()].
#' @example inst/examples/ex-phases.R
#' @author N. Frerebeau
#' @docType methods
#' @family charts
#' @name triangle_phase_cas
NULL

#' Soil Texture Triangle
#'
#' @param labels A [`logical`] scalar: should labels be displayed?
#' @param symbol A [`logical`] scalar: should symbol be used instead of full
#'  labels? Only used if `labels` is `TRUE`.
#' @param ... Further arguments to be passed to [graphics::polygon()].
#' @example inst/examples/ex-soil.R
#' @author N. Frerebeau
#' @docType methods
#' @family charts
#' @name triangle_soil
NULL
