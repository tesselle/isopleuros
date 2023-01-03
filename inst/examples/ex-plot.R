## Blank plot
ternary_plot(NULL)

## Compositional data
coda <- data.frame(
  X = c(1, 0, 0, 1/3),
  Y = c(0, 1, 0, 1/3),
  Z = c(0, 0, 1, 1/3)
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
