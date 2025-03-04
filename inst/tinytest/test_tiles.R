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

  # Bin ====================================================================
  f <- tile_bin(lava)
  image_bin <- function() {
    ternary_plot(NULL)
    ternary_image(f = f, n = 24)
    # ternary_points(lava, col = "red", pch = 16)
  }
  expect_snapshot_plot(image_bin, "image_bin")

  # Density ====================================================================
  f <- tile_density(lava)
  image_density <- function() {
    ternary_plot(NULL)
    ternary_image(f = f, n = 24)
    ternary_points(lava, col = "red", pch = 16)
  }
  expect_snapshot_plot(image_density, "image_density")

  # Interpolation ==============================================================
  if (requireNamespace("interp", quietly = TRUE)) {
    f <- tile_interpolate(arctic, value = arctic$depth)
    image_interpolate <- function() {
      ternary_plot(NULL)
      ternary_image(f = f, n = 24)
      ternary_points(arctic, col = "red", pch = 16)
    }
    expect_snapshot_plot(image_interpolate, "image_interpolate")
  }
}
