## Compositional data
coda <- data.frame(
  X = c(20, 60, 20),
  Y = c(20, 20, 60),
  Z = c(60, 20, 20)
)

## Add text
ternary_plot(NULL, panel.first = ternary_grid())
ternary_text(coda, labels = c("A", "B", "C"), col = "red", cex = 2)

