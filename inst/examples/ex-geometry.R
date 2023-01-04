## Compositional data
coda <- data.frame(
  X = c(20, 60, 20, 20),
  Y = c(20, 20, 60, 40),
  Z = c(60, 20, 20, 40)
)

## Ternary plot
ternary_plot(coda, panel.first = ternary_grid(5, 10))

## Add points
ternary_plot(NULL, panel.first = ternary_grid(5, 10))
ternary_points(coda, col = "red", pch = 16)

## Add lines
ternary_plot(NULL, panel.first = ternary_grid(5, 10))
ternary_lines(coda, col = "red", lwd = 2)

## Add a polygon
ternary_plot(NULL, panel.first = ternary_grid(5, 10))
ternary_polygon(coda, density = 5, border = "red")

## Add text
ternary_plot(NULL, panel.first = ternary_grid(5, 10))
ternary_text(coda, col = "red")

## Add arrows
ternary_plot(NULL, panel.first = ternary_grid(5, 10))
ternary_arrows(x0 = 40, y0 = 20, z0 = 40,
               x1 = 20, y1 = 40, z1 = 40)
