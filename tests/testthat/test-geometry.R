test_that("Arrows", {
  skip_if_not_installed("vdiffr")

  ## Add Arrows
  geom_arrows <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_arrows(x0 = 40, y0 = 20, z0 = 40,
                   x1 = 20, y1 = 40, z1 = 40)
  }
  vdiffr::expect_doppelganger("geom_arrows", geom_arrows)
})
test_that("Lines", {
  skip_if_not_installed("vdiffr")

  coda <- data.frame(
    X = c(20, 60, 20, 20),
    Y = c(20, 20, 60, 40),
    Z = c(60, 20, 20, 40)
  )

  ## Add lines
  geom_lines <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_lines(coda, col = "red", lwd = 2)
  }
  vdiffr::expect_doppelganger("geom_lines", geom_lines)
})
test_that("Points", {
  skip_if_not_installed("vdiffr")

  coda <- data.frame(
    X = c(20, 60, 20, 20),
    Y = c(20, 20, 60, 40),
    Z = c(60, 20, 20, 40)
  )

  ## Add points
  geom_points <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_points(coda, col = "red", pch = 16)
  }
  vdiffr::expect_doppelganger("geom_points", geom_points)
})
test_that("Polygon", {
  skip_if_not_installed("vdiffr")

  coda <- data.frame(
    X = c(20, 60, 20, 20),
    Y = c(20, 20, 60, 40),
    Z = c(60, 20, 20, 40)
  )

  ## Add a polygon
  geom_polygon <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_polygon(coda, density = 5, border = "red")
  }
  vdiffr::expect_doppelganger("geom_polygon", geom_polygon)
})
test_that("Segments", {
  skip_if_not_installed("vdiffr")

  ## Add Segments
  geom_segments <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_segments(x0 = 40, y0 = 20, z0 = 40,
                     x1 = 20, y1 = 40, z1 = 40)
  }
  vdiffr::expect_doppelganger("geom_segments", geom_segments)
})
test_that("Text", {
  skip_if_not_installed("vdiffr")

  coda <- data.frame(
    X = c(20, 60, 20, 20),
    Y = c(20, 20, 60, 40),
    Z = c(60, 20, 20, 40)
  )

  ## Add text
  geom_text <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_text(coda, col = "red")
  }
  vdiffr::expect_doppelganger("geom_text", geom_text)
})
