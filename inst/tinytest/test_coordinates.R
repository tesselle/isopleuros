# Compositional data
coda <- list(
  x = c(1, 0, 0, 1/3),
  y = c(0, 1, 0, 1/3),
  z = c(0, 0, 1, 1/3)
)

# Ternary coordinates ==========================================================
tern <- coordinates_ternary(coda)
expect_equal_to_reference(tern, file = "_snaps/coord_ternary.rds")

expect_error(coordinates_ternary(x = -coda$x, y = coda$y, z = coda$z))
expect_error(coordinates_ternary(x = 1:5, y = 1, z = 1:5))
expect_error(coordinates_ternary(x = 1:5, y = 1:5, z = 1))

# Cartesian coordinates ========================================================
cart <- coordinates_cartesian(tern)
expect_equal_to_reference(cart, file = "_snaps/coord_cartesian.rds")
expect_equal(coda, cart)

expect_error(coordinates_cartesian(x = 1:5, y = 1))
