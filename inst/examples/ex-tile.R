## Data from Aitchison 1986
## Bin
f <- tile_bin(lava)
ternary_plot(NULL)
ternary_image(f = f, n = 12)

## Custom color palette
pal <- function(x) {
  x <- (x - min(x)) / (max(x) - min(x)) # Rescale to [0,1]
  YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
  RGB <- grDevices::colorRamp(YlOrBr)(x)
  grDevices::rgb(RGB, maxColorValue = 255)
}

ternary_plot(NULL)
ternary_image(f = f, n = 12, palette = pal)

## Density
f <- tile_density(lava)
ternary_plot(NULL)
ternary_image(f = f, n = 12, palette = pal)

if (requireNamespace("interp", quietly = TRUE)) {
  ## Interpolation
  f <- tile_interpolate(arctic, value = arctic$depth)
  ternary_plot(NULL)
  ternary_image(f = f, n = 24, palette = pal)
  ternary_points(arctic, col = "red", pch = 16)
}
