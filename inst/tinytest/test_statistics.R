if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

  coda <- data.frame(
    X = c(96, 49, 57, 92, 62, 85, 75, 76, 30, 36,
          55, 93, 78, 10, 50, 69, 50, 91, 13, 24),
    Y = c(59, 90, 18, 99, 80, 22, 74, 43, 68, 80,
          30, 84, 84, 48, 79, 53, 32, 91, 19, 48),
    Z = c(33, 88, 7, 58, 81, 16, 48, 91, 85, 12,
          94, 81, 32, 39, 39, 32, 65, 35, 4, 12)
  )

  # Ellipse ====================================================================
  ## Ellipse
  ellipse_radius <- function() {
    ternary_plot(coda)
    ternary_ellipse(coda, radius = 1)
  }
  expect_snapshot_plot(ellipse_radius, "ellipse_radius")

  ## Confidence ellipse
  ellipse_confidence <- function() {
    ternary_plot(coda)
    ternary_confidence(coda, level = c(0.95, 0.99))
  }
  expect_snapshot_plot(ellipse_confidence, "ellipse_confidence")

  ## Tolerance ellipse
  ellipse_tolerance <- function() {
    ternary_plot(coda)
    ternary_tolerance(coda, level = c(0.95, 0.99))
  }
  expect_snapshot_plot(ellipse_tolerance, "ellipse_tolerance")

  # Convex hull ================================================================
  convex_hull <- function() {
    ternary_plot(coda, panel.first = ternary_grid(5, 10))
    ternary_hull(coda, border = "red")
  }
  expect_snapshot_plot(convex_hull, "convex_hull")

  # Density contours ===========================================================
  density_contours <- function() {
    ternary_plot(coda, panel.first = ternary_grid(5, 10))
    ternary_density(coda, n = 500, nlevels = 10)
  }
  expect_snapshot_plot(density_contours, "density_contours")

  # Mean =======================================================================
  geometric_mean <- function() {
    ternary_plot(coda, panel.first = ternary_grid())
    ternary_mean(coda, pch = 16, col = "red")
  }
  expect_snapshot_plot(geometric_mean, "geometric_mean")

  # PCA ========================================================================
  pca_1 <- function() {
    ternary_plot(lava)
    ternary_pca(lava, axis = 1)
  }
  expect_snapshot_plot(pca_1, "pca_1")

  # Contour ====================================================================
  if (requireNamespace("interp", quietly = TRUE)) {
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
    expect_snapshot_plot(contour_ilr, "contour_ilr")

    ## Contour Cartesian
    contour_cartesian <- function() {
      ternary_plot(NULL)
      suppressWarnings( # Remove warnings (collinear points)
        ternary_contour(coords, value = value, n = 100, nlevels = 10, ilr = FALSE)
      )
    }
    expect_snapshot_plot(contour_cartesian, "contour_cartesian")
  }
}
