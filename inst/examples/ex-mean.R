## Mean
## Data from Aitchison 1986
ternary_plot(lava, panel.first = ternary_grid())
ternary_mean(lava, pch = 16, col = "red")
ternary_confidence(lava, level = 0.95, border = "red", lty = 1)
