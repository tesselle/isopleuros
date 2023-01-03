## Compositional data
coda <- data.frame(
  X = c(96, 49, 57, 92, 62, 85, 75, 76, 30, 36,
        55, 93, 78, 10, 50, 69, 50, 91, 13, 24),
  Y = c(59, 90, 18, 99, 80, 22, 74, 43, 68, 80,
        30, 84, 84, 48, 79, 53, 32, 91, 19, 48),
  Z = c(33, 88, 7, 58, 81, 16, 48, 91, 85, 12,
        94, 81, 32, 39, 39, 32, 65, 35, 4, 12)
)

## Convex hull
ternary_plot(coda, panel.first = ternary_grid(5, 10))
ternary_hull(coda, border = "red")
