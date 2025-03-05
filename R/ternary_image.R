# IMAGE
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_image
#' @aliases ternary_image,function-method
setMethod(
  f = "ternary_image",
  signature = c(f = "function"),
  definition = function(f, n = 48, palette = NULL, ...) {

    tri <- .triangle_center(n)
    xyz <- coordinates_cartesian(tri$x, tri$y)
    val <- f(xyz$x, xyz$y, xyz$z, ...)
    col <- map_color(val, palette = palette)

    coords <- .triangle_vertex(tri$x, tri$y, tri$direction, tri$resolution)
    for (i in seq_along(coords)) {
      polygon(coords[[i]][, 1], coords[[i]][, 2], col = col[i], border = NA)
    }

    invisible(list(levels = val, colors = col))
  }
)

#' @export
#' @rdname ternary_tile
#' @aliases tile_bin,numeric,numeric,numeric-method
setMethod(
  f = "tile_bin",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z) {

    total <- x + y + z
    a <- x / total
    b <- y / total
    c <- z / total

    function(x, y, z) {
      tri <- .triangle_center(sqrt(length(x)))
      coords <- .triangle_vertex(tri$x, tri$y, tri$direction, tri$resolution)

      count <- numeric(length(coords))
      for (i in seq_along(coords)) {
        xyz <- coordinates_cartesian(coords[[i]][, 1], coords[[i]][, 2])
        count[[i]] <- sum(min(xyz$x) <= a & a < max(xyz$x) &
                            min(xyz$y) <= b & b < max(xyz$y) &
                            min(xyz$z) <= c & c < max(xyz$z))
      }

      count[count == 0] <- NA
      count
    }
  }
)

#' @export
#' @rdname ternary_tile
#' @aliases tile_bin,ANY,missing,missing-method
setMethod(
  f = "tile_bin",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x) {
    xyz <- grDevices::xyz.coords(x)
    methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z)
  }
)

#' @export
#' @rdname ternary_tile
#' @aliases tile_density,numeric,numeric,numeric-method
setMethod(
  f = "tile_density",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z) {
    ## ILR
    coda <- cbind(x, y, z)
    ratio <- ilr(coda)

    ## Compute KDE
    function(x, y, z) {
      xyz <- cbind(x, y, z)
      xy <- ilr(xyz)
      dens <- kde(
        x = ratio[, 1],
        y = ratio[, 2],
        gx = sort(unique(xy[, 1])),
        gy = sort(unique(xy[, 2]))
      )

      i <- as.numeric(as.factor(rank(xy[, 1])))
      j <- as.numeric(as.factor(rank(xy[, 2])))

      dens$z[cbind(i, j)]
    }
  }
)

#' @export
#' @rdname ternary_tile
#' @aliases tile_density,ANY,missing,missing-method
setMethod(
  f = "tile_density",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x) {
    xyz <- grDevices::xyz.coords(x)
    methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z)
  }
)

#' @export
#' @rdname ternary_tile
#' @aliases tile_interpolate,numeric,numeric,numeric-method
setMethod(
  f = "tile_interpolate",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, value, method = "linear", ...) {
    ## Validation
    assert_package("interp")
    assert_length(value, length(x))

    ## ILR
    coda <- cbind(x, y, z)
    ratio <- ilr(coda)

    ## Interpolate
    function(x, y, z) {
      xyz <- cbind(x, y, z)
      xy <- ilr(xyz)

      interp <- interp::interp(
        x = ratio[, 1],
        y = ratio[, 2],
        z = value,
        xo = sort(unique(xy[, 1])),
        yo = sort(unique(xy[, 2])),
        method = method,
        ...
      )

      i <- as.numeric(as.factor(rank(xy[, 1])))
      j <- as.numeric(as.factor(rank(xy[, 2])))

      interp$z[cbind(i, j)]
    }
  }
)

#' @export
#' @rdname ternary_tile
#' @aliases tile_interpolate,ANY,missing,missing-method
setMethod(
  f = "tile_interpolate",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, value, method = "linear", ...) {
    xyz <- grDevices::xyz.coords(x)
    methods::callGeneric(x = xyz$x, y = xyz$y, z = xyz$z,
                         value = value, method = method, ...)
  }
)

#' Tile Center Coordinates
#'
#' Computes tile center cartesian coordinates.
#' @param resolution A length-one [`integer`] vector specifying the maximum
#'  number of tiles on each axis.
#' @return
#'  A [`list`] with the following elements:
#'  \describe{
#'   \item{`x`}{x cartesian coordinates.}
#'   \item{`y`}{y cartesian coordinates.}
#'   \item{`direction`}{`1` means up, `-1` means down.}
#'   \item{`resolution`}{}
#'  }
#' @examples
#' .triangle_center(5)
#' @keywords internal
#' @noRd
.triangle_center <- function(resolution) {

  offset <- 1 / resolution / 2L
  height <- .top / resolution

  X <- seq(from = offset, to = 1 - offset, by = offset)
  X <- lapply(
    X = seq_len(resolution),
    FUN = function(step) {
      up <- X[seq(from = step, to = (2L * resolution) - step, by = 2L)]
      down <- integer(0)
      if (step != resolution) {
        down <- X[seq(from = step + 1, to = (2L * resolution) - step - 1, by = 2L)]
      }
      c(rbind(up, c(down, NA)))[-(resolution - step + 1) * 2L]
    }
  )

  in_row <- 2L * rev(seq_len(resolution)) - 1L
  is_down <- (1 + unlist(lapply(in_row, seq_len))) %% 2L

  Y <- seq(from = height / 3, to = .top - (2 * height / 3), length.out = resolution)
  Y <- rep(Y[seq_len(resolution)], in_row)
  Y <- Y + (is_down * height / 3)

  dir <- rep(1, length(is_down))
  dir[is_down == 1] <- -1

  list(
    x = unlist(X),
    y = Y,
    direction = dir,
    resolution = resolution
  )
}

#' Tile Vertex Coordinates
#'
#' Computes tile vertex cartesian coordinates.
#' @param x,y A [`numeric`] vector giving the cartesian coordinates of the
#'  center.
#' @param direction An [`integer`] vector specifying the triangle direction
#'  (`1` means up, `-1` means down).
#' @param resolution A length-one [`integer`] vector specifying the maximum
#'  number of tiles on each axis.
#' @return
#'  A [`list`] of `numeric` [`matrix`].
#' @examples
#' m <- .triangle_center(5)
#' .triangle_vertex(m$x, m$y, m$direction, m$resolution)
#' @keywords internal
#' @noRd
.triangle_vertex <- function(x, y, direction, resolution) {
  n <- length(x)

  width <- 1 / resolution / 2
  height <- .top / resolution / 3

  tiles <- vector(mode = "list", length = n)
  for (i in seq_len(n)) {
    tiles[[i]] <- matrix(
      data = c(x[i] + c(0, width, -width), y[i] + c(2 * height, -height, -height) * direction[i]),
      ncol = 2, dimnames = list(NULL, c("x", "y"))
    )
  }

  tiles
}
