## Add density
## Data from Aitchison 1986
ternary_plot(lava, panel.first = ternary_grid())
ternary_density(lava, n = 500, nlevels = 10, col = c("yellow", "red"))
