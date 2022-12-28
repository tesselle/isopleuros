# GENERIC METHODS
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
