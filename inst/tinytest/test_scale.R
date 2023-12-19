if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  # Plot =======================================================================
  data(lava)

  ## Center
  plot_center <- function() {
    ternary_plot(lava, center = TRUE, scale = FALSE, panel.first = ternary_grid(5))
  }
  expect_snapshot_plot(plot_center, "plot_center")

  ## Scale
  plot_scale <- function() {
    ternary_plot(lava, center = FALSE, scale = TRUE, panel.first = ternary_grid(5))
  }
  expect_snapshot_plot(plot_scale, "plot_scale")

  ## Center and scale
  plot_center_scale <- function() {
    ternary_plot(lava, center = TRUE, scale = TRUE, panel.first = ternary_grid(5))
  }
  expect_snapshot_plot(plot_center_scale, "plot_center_scale")
}
