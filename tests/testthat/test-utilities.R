test_that("CLR transformation", {
  coda <- data.frame(
    X = c(20, 60, 20, 20),
    Y = c(20, 20, 60, 40),
    Z = c(60, 20, 20, 40)
  )
  coda <- coda / rowSums(coda)
  coda <- as.matrix(coda)

  x <- clr(coda)
  y <- clr_inv(x)

  expect_equal(y, coda, ignore_attr = TRUE)
})
test_that("ILR transformation", {
  coda <- data.frame(
    X = c(20, 60, 20, 20),
    Y = c(20, 20, 60, 40),
    Z = c(60, 20, 20, 40)
  )
  coda <- coda / rowSums(coda)
  coda <- as.matrix(coda)

  x <- ilr(coda)
  y <- ilr_inv(x)

  expect_equal(y, coda, ignore_attr = TRUE)
})
