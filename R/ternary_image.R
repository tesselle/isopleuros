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
    ok <- seq_along(tri$x)

    if (isFALSE(palette)) {
      color <- val
    }
    if (is.null(palette)) {
      palette <- function(x) {
        x <- (x - min(x)) / (max(x) - min(x)) # Rescale to [0,1]
        col <- grDevices::hcl.colors(256L, palette = "viridis")
        grDevices::rgb(grDevices::colorRamp(col)(x), maxColorValue = 255)
      }
    }
    if (is.function(palette)) {
      ok <- is.finite(val) # Remove NA/Inf (if any)
      color <- palette(val[ok])
    }

    .triangle_tile(
      x = tri$x[ok],
      y = tri$y[ok],
      direction = tri$direction[ok],
      resolution = tri$resolution,
      col = color
    )
  }
)

xyz_density <- function(x, y, z) {
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

xyz_interpolate <- function(x, y, z, value, method = "linear", ...) {
  ## Validation
  if (!requireNamespace("interp", quietly = TRUE)) {
    msg <- "The interp package is required. Please install it."
    stop(msg, call. = FALSE)
  }
  assert_length(value, length(x))

  ## ILR
  coda <- cbind(x, y, z)
  ratio <- ilr(coda)

  ## Compute KDE
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

.triangle_tile <- function(x, y, direction, resolution, col = NA) {
  n <- length(x)
  if (length(col) == 1) col <- rep(col, n)

  width <- 1 / resolution / 2
  height <- .top / resolution / 3

  for (i in seq_len(n)) {
    xi <- x[i] + c(0, width, -width)
    yi <- y[i] + c(2 * height, -height, -height) * direction[i]
    polygon(xi, yi, col = col[i], border = NA)
  }

  invisible()
}
