## Compositional data
coda <- data.frame(
  X = c(20, 60, 20, 20),
  Y = c(20, 20, 60, 40),
  Z = c(60, 20, 20, 40)
)

## Add text
ternary_plot(NULL, panel.first = ternary_grid())
ternary_text(coda, col = "red", cex = 2)