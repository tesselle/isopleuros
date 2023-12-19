## Data from Aitchison 1986
ternary_plot(lava, center = FALSE, scale = FALSE, col = "red", pch = 16)
ternary_grid(5)

## Center
z <- ternary_plot(lava, center = TRUE, col = "blue", pch = 16)
ternary_grid(5, center = z$center)

## Center and scale
z <- ternary_plot(lava, center = TRUE, scale = TRUE, col = "green", pch = 16)
ternary_grid(5, center = z$center, scale = z$scale)
