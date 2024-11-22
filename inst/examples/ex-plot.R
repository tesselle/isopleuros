## Blank plot
ternary_plot(NULL)

## Compositional data
coda <- data.frame(
  X = c(20, 60, 20, 1/3),
  Y = c(20, 20, 60, 1/3),
  Z = c(60, 20, 20, 1/3)
)

## Ternary plot
ternary_plot(coda, pch = 16, col = "red")

## Add a grid
ternary_plot(coda, panel.first = ternary_grid(5, 10))

## Zoom
ternary_plot(coda, ylim = c(0, 0.4, 0), zlim = c(0, 0, 0.4),
             panel.first = ternary_grid())
ternary_plot(coda, xlim = c(0, 0.4, 0), zlim = c(0, 0, 0.4),
             panel.first = ternary_grid())
ternary_plot(coda, xlim = c(0.4, 0, 0), ylim = c(0, 0.4, 0),
             panel.first = ternary_grid())

## Color according to a supplementary variable
## Data from Aitchison 1986
col <- grDevices::colorRampPalette(c("red", "blue"))(nrow(arctic))
ternary_plot(arctic, panel.first = ternary_grid(), pch = 16, col = col)
