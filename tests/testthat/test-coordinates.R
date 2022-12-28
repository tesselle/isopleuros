test_that("Ternary coordinates", {
  ## Compositional data
  coda <- list(
    x = c(1, 0, 0, 1/3),
    y = c(0, 1, 0, 1/3),
    z = c(0, 0, 1, 1/3)
  )

  ## Ternary coordinates
  tern <- coordinates_ternary(coda)
  testthat::expect_snapshot(tern)

  ## Cartesian coordinates
  cart <- coordinates_cartesian(tern)
  testthat::expect_snapshot(cart)
  testthat::expect_equal(coda, cart)

  coda$x <- -coda$x
  testthat::expect_error(coordinates_ternary(coda))
  tern$x <- -tern$x
  testthat::expect_error(coordinates_cartesian(tern))
})
