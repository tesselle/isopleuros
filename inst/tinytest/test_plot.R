if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  # Plot =======================================================================
  coda <- data.frame(
    X = c(1, 0, 0, 1/3),
    Y = c(0, 1, 0, 1/3),
    Z = c(0, 0, 1, 1/3)
  )

  ## Blank plot
  plot_blank <- function() ternary_plot(NULL)
  expect_snapshot_plot(plot_blank, "plot_blank")

  ## Basic plot
  plot_basic <- function() ternary_plot(coda)
  expect_snapshot_plot(plot_basic, "plot_basic")

  ## Remove axis
  plot_axes <- function() ternary_plot(coda, axes = FALSE)
  expect_snapshot_plot(plot_axes, "plot_axes")

  ## Change labels
  plot_labels <- function() ternary_plot(coda, xlab = "A", ylab = "B", zlab = "C")
  expect_snapshot_plot(plot_labels, "plot_labels")

  ## Add title
  plot_title <- function() ternary_plot(coda, main = "Title", sub = "Subtitle")
  expect_snapshot_plot(plot_title, "plot_title")

  ## Graphical parameters
  plot_par <- function() ternary_plot(coda, fg = "red", cex.lab = 2, col.lab = "blue")
  expect_snapshot_plot(plot_par, "plot_par")

  # Zoom =======================================================================
  expect_error(ternary_plot(NULL, xlim = c(0.5, 1), panel.first = ternary_grid()))

  ## Zoom x
  zoom_x <- function() ternary_plot(NULL, ylim = c(0, 0.4, 0), zlim = c(0, 0, 0.4), panel.first = ternary_grid(5))
  expect_snapshot_plot(zoom_x, "zoom_x")

  ## Zoom y
  zoom_y <- function() ternary_plot(NULL, xlim = c(0, 0.4, 0), zlim = c(0, 0, 0.4), panel.first = ternary_grid(5))
  expect_snapshot_plot(zoom_y, "zoom_y")

  ## Zoom z
  zoom_z <- function() ternary_plot(NULL, xlim = c(0.4, 0, 0), ylim = c(0, 0.4, 0), panel.first = ternary_grid(5))
  expect_snapshot_plot(zoom_z, "zoom_z")

  # Axes =======================================================================
  axes_add <- function() {
    ternary_plot(NULL, axes = FALSE)
    ternary_axis(side = 1, col = "red", cex.axis = 1)
    ternary_axis(side = 2, col = "blue", cex.axis = 2)
    ternary_axis(side = 3, col = "green", cex.axis = 3)
  }
  expect_snapshot_plot(axes_add, "axes_add")

  # Grid =======================================================================
  ## Panel first
  grid_panel <- function() ternary_plot(NULL, panel.first = ternary_grid(5, 10))
  expect_snapshot_plot(grid_panel, "grid_panel")

  ## Custom grid
  grid_add <- function() {
    ternary_plot(NULL)
    ternary_grid()
  }
  expect_snapshot_plot(grid_add, "grid_add")

  ## Graphical parameters
  grid_par <- function() {
    ternary_plot(NULL)
    ternary_grid(4, 12, col.primary = "red", col.secondary = "blue",
                 lty.primary = "dotted", lty.secondary = "dashed",
                 lwd.primary = 3, lwd.secondary = 2)
  }
  expect_snapshot_plot(grid_par, "grid_par")
}
