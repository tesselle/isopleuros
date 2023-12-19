## Add points
## Data from Aitchison 1986
ternary_plot(NULL, panel.first = ternary_grid())
ternary_points(lava, col = "red", pch = 16)

## Center and scale
ternary_plot(NULL, axes = FALSE, frame.plot = TRUE)
ternary_points(lava, col = "red", pch = 16)
ternary_points(lava, center = TRUE, col = "blue", pch = 16)
ternary_points(lava, center = TRUE, scale = TRUE, col = "green", pch = 16)
