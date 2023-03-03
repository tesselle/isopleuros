test_that("Ellipse", {
  skip_if_not_installed("vdiffr")

  coda <- data.frame(
    X = c(96, 49, 57, 92, 62, 85, 75, 76, 30, 36,
          55, 93, 78, 10, 50, 69, 50, 91, 13, 24),
    Y = c(59, 90, 18, 99, 80, 22, 74, 43, 68, 80,
          30, 84, 84, 48, 79, 53, 32, 91, 19, 48),
    Z = c(33, 88, 7, 58, 81, 16, 48, 91, 85, 12,
          94, 81, 32, 39, 39, 32, 65, 35, 4, 12)
  )

  ## Ellipse
  ellipse_radius <- function() {
    ternary_plot(coda)
    ternary_ellipse(coda, radius = 1)
  }
  vdiffr::expect_doppelganger("ellipse_radius", ellipse_radius)

  ## Confidence ellipse
  ellipse_confidence <- function() {
    ternary_plot(coda)
    ternary_confidence(coda, level = c(0.95, 0.99))
  }
  vdiffr::expect_doppelganger("ellipse_confidence", ellipse_confidence)

  ## Tolerance ellipse
  ellipse_tolerance <- function() {
    ternary_plot(coda)
    ternary_tolerance(coda, level = c(0.95, 0.99))
  }
  vdiffr::expect_doppelganger("ellipse_tolerance", ellipse_tolerance)
})
test_that("Convex hull", {
  skip_if_not_installed("vdiffr")

  coda <- data.frame(
    X = c(96, 49, 57, 92, 62, 85, 75, 76, 30, 36,
          55, 93, 78, 10, 50, 69, 50, 91, 13, 24),
    Y = c(59, 90, 18, 99, 80, 22, 74, 43, 68, 80,
          30, 84, 84, 48, 79, 53, 32, 91, 19, 48),
    Z = c(33, 88, 7, 58, 81, 16, 48, 91, 85, 12,
          94, 81, 32, 39, 39, 32, 65, 35, 4, 12)
  )

  ## Convex hull
  convex_hull <- function() {
    ternary_plot(coda, panel.first = ternary_grid(5, 10))
    ternary_hull(coda, border = "red")
  }
  vdiffr::expect_doppelganger("convex_hull", convex_hull)
})
test_that("Contour", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("akima")

  a <- matrix(rep(seq(0, 1, length = 50), each = 50), nrow = 50, byrow = TRUE)
  b <- matrix(rep(seq(0, 1, length = 50), each = 50), nrow = 50, byrow = FALSE)
  mask <- a + b <= 1
  a <- a[mask]
  b <- b[mask]
  coords <- cbind(b, 1 - a - b, a)
  value <- sin(3.2 * pi * (a + b)) + sin(3 * pi * (a - b))

  # col <- colorRamp(c("blue", "red"))(scales::rescale_mid(value))
  # col <- rgb(col[, 1], col[, 2], col[, 3], maxColorValue = 255)
  # ternary_plot(coords, panel.first = ternary_grid(), pch = 16, col = col)

  ## Contour ILR
  contour_ilr <- function() {
    ternary_plot(NULL)
    ternary_contour(coords, value = value, n = 100, nlevels = 10, ilr = TRUE)
  }
  vdiffr::expect_doppelganger("contour_ilr", contour_ilr)

  ## Contour Cartesian
  contour_cartesian <- function() {
    ternary_plot(NULL)
    suppressWarnings( # Remove warning from akima (collinear points)
      ternary_contour(coords, value = value, n = 100, nlevels = 10, ilr = FALSE)
    )
  }
  vdiffr::expect_doppelganger("contour_cartesian", contour_cartesian)
})
test_that("Density", {
  skip_if_not_installed("vdiffr")

  coda <- data.frame(
    X = c(96, 49, 57, 92, 62, 85, 75, 76, 30, 36,
          55, 93, 78, 10, 50, 69, 50, 91, 13, 24),
    Y = c(59, 90, 18, 99, 80, 22, 74, 43, 68, 80,
          30, 84, 84, 48, 79, 53, 32, 91, 19, 48),
    Z = c(33, 88, 7, 58, 81, 16, 48, 91, 85, 12,
          94, 81, 32, 39, 39, 32, 65, 35, 4, 12)
  )

  ## Density contours
  density_contours <- function() {
    ternary_plot(coda, panel.first = ternary_grid(5, 10))
    ternary_density(coda, n = 500, nlevels = 10)
  }
  vdiffr::expect_doppelganger("density_contours", density_contours)
})
test_that("Mean", {
  skip_if_not_installed("vdiffr")

  coda <- data.frame(
    X = c(96, 49, 57, 92, 62, 85, 75, 76, 30, 36,
          55, 93, 78, 10, 50, 69, 50, 91, 13, 24),
    Y = c(59, 90, 18, 99, 80, 22, 74, 43, 68, 80,
          30, 84, 84, 48, 79, 53, 32, 91, 19, 48),
    Z = c(33, 88, 7, 58, 81, 16, 48, 91, 85, 12,
          94, 81, 32, 39, 39, 32, 65, 35, 4, 12)
  )

  ## Mean
  geometric_mean <- function() {
    ternary_plot(coda, panel.first = ternary_grid())
    ternary_mean(coda, pch = 16, col = "red")
  }
  vdiffr::expect_doppelganger("geometric_mean", geometric_mean)
})
