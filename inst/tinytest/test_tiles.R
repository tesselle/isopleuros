if (at_home()) {
  using("tinysnapshot")
  options(tinysnapshot_device = "svglite")
  options(tinysnapshot_height = 7) # inches
  options(tinysnapshot_width = 7)
  options(tinysnapshot_tol = 200) # pixels
  options(tinysnapshot_os = "Linux")

  # Image ======================================================================
  image_rgb <- function() {
    ternary_plot(NULL, xlab = "Red", ylab = "Green", zlab = "Blue")
    ternary_image(f = grDevices::rgb, n = 20, palette = FALSE)
  }
  expect_snapshot_plot(image_rgb, "image_rgb")

  # Density ====================================================================
  f <- isopleuros:::xyz_density(lava[, 1], lava[, 2], lava[, 3])
  image_density <- function() {
    ternary_plot(NULL)
    ternary_image(f = f, n = 24)
    ternary_points(lava, col = "red", pch = 16)
  }
  expect_snapshot_plot(image_density, "image_density")

  # Interpolation ==============================================================
  if (requireNamespace("interp", quietly = TRUE)) {
    f <- isopleuros:::xyz_interpolate(arctic[, 1], arctic[, 2], arctic[, 3], value = arctic$depth)
    image_interpolate <- function() {
      ternary_plot(NULL)
      ternary_image(f = f, n = 24)
      ternary_points(arctic, col = "red", pch = 16)
    }
    expect_snapshot_plot(image_interpolate, "image_interpolate")
  }
}
