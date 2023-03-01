## Compositional data
coda <- data.frame(
  X = c(20, 60, 20, 20),
  Y = c(20, 20, 60, 40),
  Z = c(60, 20, 20, 40)
)

## Add cross-hairs
ternary_plot(coda, panel.first = ternary_grid())
ternary_crosshairs(coda)

ternary_plot(coda, panel.first = ternary_grid())
ternary_crosshairs(coda, y_mark = FALSE, z_mark = FALSE, col = "red")

ternary_plot(coda, panel.first = ternary_grid())
ternary_crosshairs(coda, x_mark = FALSE, z_mark = FALSE, col = "green")

ternary_plot(coda, panel.first = ternary_grid())
ternary_crosshairs(coda, x_mark = FALSE, y_mark = FALSE, col = "blue")
