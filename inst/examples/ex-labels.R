## Compositional data
coda <- data.frame(
  X = c(41.0, 40, 39.0),
  Y = c(19.5, 20, 20.5),
  Z = c(39.5, 40, 40.5)
)

## Add text
ternary_plot(NULL, panel.first = ternary_grid())
ternary_points(coda)
ternary_labels(coda, labels = c("A", "B", "C"))
