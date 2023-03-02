## Compositional data
coda <- data.frame(
  X = c(20, 60, 20),
  Y = c(20, 20, 60),
  Z = c(60, 20, 20)
)

## Add a polygon
ternary_plot(NULL, panel.first = ternary_grid())
ternary_polygon(coda, density = 5, border = "red")
