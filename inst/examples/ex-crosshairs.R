## Add cross-hairs
## Data from Aitchison 1986
ternary_plot(lava, panel.first = ternary_grid())
ternary_crosshairs(lava)

ternary_plot(lava, panel.first = ternary_grid())
ternary_crosshairs(lava, y_mark = FALSE, z_mark = FALSE, col = "red")

ternary_plot(lava, panel.first = ternary_grid())
ternary_crosshairs(lava, x_mark = FALSE, z_mark = FALSE, col = "green")

ternary_plot(lava, panel.first = ternary_grid())
ternary_crosshairs(lava, x_mark = FALSE, y_mark = FALSE, col = "blue")
