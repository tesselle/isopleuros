## Compositional data
coda <- data.frame(
  x = c(1, 0, 0, 1/3),
  y = c(0, 1, 0, 1/3),
  z = c(0, 0, 1, 1/3)
)

## Ternary coordinates
(tern <- coordinates_ternary(coda))

## Cartesian coordinates
(cart <- coordinates_cartesian(tern))
