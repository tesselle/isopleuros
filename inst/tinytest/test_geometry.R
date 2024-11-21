if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  coda <- data.frame(
    X = c(20, 60, 20, 20),
    Y = c(20, 20, 60, 40),
    Z = c(60, 20, 20, 40)
  )

  # Arrows =====================================================================
  geom_arrows <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_arrows(x0 = 40, y0 = 20, z0 = 40,
                   x1 = 20, y1 = 40, z1 = 40)
  }
  expect_snapshot_plot(geom_arrows, "geom_arrows")

  # Cross-hairs ================================================================
  geom_crosshairs_x <- function() {
    ternary_plot(coda, panel.first = ternary_grid())
    ternary_crosshairs(coda, y_mark = FALSE, z_mark = FALSE, col = "red")
  }
  expect_snapshot_plot(geom_crosshairs_x, "geom_crosshairs_x")

  geom_crosshairs_y <- function() {
    ternary_plot(coda, panel.first = ternary_grid())
    ternary_crosshairs(coda, x_mark = FALSE, z_mark = FALSE, col = "green")
  }
  expect_snapshot_plot(geom_crosshairs_y, "geom_crosshairs_y")

  geom_crosshairs_z <- function() {
    ternary_plot(coda, panel.first = ternary_grid())
    ternary_crosshairs(coda, x_mark = FALSE, y_mark = FALSE, col = "blue")
  }
  expect_snapshot_plot(geom_crosshairs_z, "geom_crosshairs_z")

  # Lines ======================================================================
  geom_lines <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_lines(coda, col = "red", lwd = 2)
  }
  expect_snapshot_plot(geom_lines, "geom_lines")

  # Points =====================================================================
  geom_points <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_points(coda, col = "red", pch = 16)
  }
  expect_snapshot_plot(geom_points, "geom_points")

  # Polygon ====================================================================
  geom_polygon <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_polygon(coda, density = 5, border = "red")
  }
  expect_snapshot_plot(geom_polygon, "geom_polygon")

  # Segments ===================================================================
  geom_segments <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_segments(x0 = 40, y0 = 20, z0 = 40,
                     x1 = 20, y1 = 40, z1 = 40)
  }
  expect_snapshot_plot(geom_segments, "geom_segments")

  # Text =======================================================================
  geom_text <- function() {
    ternary_plot(NULL, panel.first = ternary_grid(5, 10))
    ternary_text(coda, col = "red")
  }
  expect_snapshot_plot(geom_text, "geom_text")

  # Image ======================================================================
  image_rgb <- function() {
    ternary_plot(NULL, xlab = "Red", ylab = "Green", zlab = "Blue")
    ternary_image(f = grDevices::rgb, n = 20, palette = NULL)
  }
  expect_snapshot_plot(image_rgb, "image_rgb")
}
