test_that("Plot", {
  skip_if_not_installed("vdiffr")

  coda <- data.frame(
    X = c(1, 0, 0, 1/3),
    Y = c(0, 1, 0, 1/3),
    Z = c(0, 0, 1, 1/3)
  )

  ## Blank plot
  plot_blank <- function() ternary_plot(NULL)
  vdiffr::expect_doppelganger("plot_blank", plot_blank)

  ## Basic plot
  plot_basic <- function() ternary_plot(coda)
  vdiffr::expect_doppelganger("plot_basic", plot_basic)

  ## Remove axis
  plot_axes <- function() ternary_plot(coda, axes = FALSE)
  vdiffr::expect_doppelganger("plot_axes", plot_axes)

  ## Change labels
  plot_labels <- function() ternary_plot(coda, xlab = "A", ylab = "B", zlab = "C")
  vdiffr::expect_doppelganger("plot_labels", plot_labels)

  ## Add title
  plot_title <- function() ternary_plot(coda, main = "Title", sub = "Subtitle")
  vdiffr::expect_doppelganger("plot_title", plot_title)

  ## Graphical parameters
  plot_par <- function() ternary_plot(coda, fg = "red", cex.lab = 2, col.lab = "blue")
  vdiffr::expect_doppelganger("plot_par", plot_par)
})
test_that("Zoom", {
  skip_if_not_installed("vdiffr")

  ## Zoom x
  zoom_x <- function() ternary_plot(NULL, xlim = c(0.5, 1), panel.first = ternary_grid())
  vdiffr::expect_doppelganger("zoom_x", zoom_x)

  ## Zoom y
  zoom_y <- function() ternary_plot(NULL, ylim = c(0.5, 1), panel.first = ternary_grid())
  vdiffr::expect_doppelganger("zoom_y", zoom_y)

  ## Zoom z
  zoom_z <- function() ternary_plot(NULL, zlim = c(0.5, 1), panel.first = ternary_grid())
  vdiffr::expect_doppelganger("zoom_z", zoom_z)
})
test_that("Axes", {
  skip_if_not_installed("vdiffr")

  ## Custom axes
  axes_add <- function() {
    ternary_plot(NULL, axes = FALSE)
    ternary_axis(side = 1, col = "red", cex.axis = 1)
    ternary_axis(side = 2, col = "blue", cex.axis = 2)
    ternary_axis(side = 3, col = "green", cex.axis = 3)
  }
  vdiffr::expect_doppelganger("axes_add", axes_add)
})
test_that("Grid", {
  skip_if_not_installed("vdiffr")

  ## Panel first
  grid_panel <- function() ternary_plot(NULL, panel.first = ternary_grid(5, 10))
  vdiffr::expect_doppelganger("grid_panel", grid_panel)

  ## Custom grid
  grid_add <- function() {
    ternary_plot(NULL)
    ternary_grid()
  }
  vdiffr::expect_doppelganger("grid_add", grid_add)

  ## Graphical parameters
  grid_par <- function() {
    ternary_plot(NULL)
    ternary_grid(4, 12, col.primary = "red", col.secondary = "blue",
                 lty.primary = "dotted", lty.secondary = "dashed",
                 lwd.primary = 3, lwd.secondary = 2)
  }
  vdiffr::expect_doppelganger("grid_par", grid_par)
})
