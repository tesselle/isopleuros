## Blank plot
ternary_plot(NULL)

## Compositional data
coda <- data.frame(
  X = c(20, 60, 20, 20),
  Y = c(20, 20, 60, 40),
  Z = c(60, 20, 20, 40)
)

## Ternary plot
ternary_plot(coda, pch = 16, col = "red")

## Add a grid
ternary_plot(coda, panel.first = ternary_grid(5, 10))

## Add axis
ternary_plot(coda, axes = FALSE)
ternary_axis(side = 1, col = "red")
ternary_axis(side = 2, col = "blue")
ternary_axis(side = 3, col = "green")

## Zoom
ternary_plot(coda, xlim = c(0.5, 1), panel.first = ternary_grid())
ternary_plot(coda, ylim = c(0.5, 1), panel.first = ternary_grid())
ternary_plot(coda, zlim = c(0.5, 1), panel.first = ternary_grid())
