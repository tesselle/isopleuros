## Add density
## Data from Aitchison 1986
ternary_plot(lava, panel.first = ternary_grid())
levels <- ternary_density(lava, n = 500, nlevels = 10, col = c("yellow", "red"))

## Add a legend
legend_image <- grDevices::as.raster(rev(levels$colors))
graphics::rasterImage(legend_image, 0.85, 0.75, 0.9, 1)
graphics::text(x = 0.9, y = c(0.75, 1), labels = range(levels$levels), pos = 4)
