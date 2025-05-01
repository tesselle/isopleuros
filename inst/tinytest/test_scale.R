Sys.setenv(LANGUAGE = "en") # Force locale

ternary_plot(lava, center = TRUE, scale = FALSE)
expect_message(ternary_points(lava), "The current plot has been centered")

ternary_plot(lava, center = FALSE, scale = TRUE)
expect_message(ternary_points(lava), "The current plot has been scaled")

ternary_plot(lava, center = TRUE, scale = TRUE)
expect_message(ternary_points(lava))

ternary_plot(lava, center = FALSE, scale = FALSE)
expect_silent(ternary_points(lava))

if (at_home()) {
  using("tinysnapshot")
  source("helpers.R")

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
