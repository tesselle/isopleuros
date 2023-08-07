## Compositional data
coda <- list(
  x = c(1, 0, 0, 1/3),
  y = c(0, 1, 0, 1/3),
  z = c(0, 0, 1, 1/3)
)

## Ternary coordinates
tern <- isopleuros:::coordinates_ternary(coda)
expect_equal_to_reference(tern, file = "_snaps/coord_ternary.rds")

## Cartesian coordinates
cart <- isopleuros:::coordinates_cartesian(tern)
expect_equal_to_reference(cart, file = "_snaps/coord_cartesian.rds")
expect_equal(coda, cart)

coda$x <- -coda$x
expect_error(coordinates_ternary(coda))
tern$x <- -tern$x
expect_error(coordinates_cartesian(tern))
