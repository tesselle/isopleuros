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
