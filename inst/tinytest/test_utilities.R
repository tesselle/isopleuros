# CLR transformation ===========================================================
coda <- data.frame(
  X = c(20, 60, 20, 20),
  Y = c(20, 20, 60, 40),
  Z = c(60, 20, 20, 40)
)
coda <- coda / rowSums(coda)
coda <- as.matrix(coda)

x <- isopleuros:::clr(coda)
y <- isopleuros:::clr_inv(x)

expect_equivalent(y, coda)

# ILR transformation ===========================================================
coda <- data.frame(
  X = c(20, 60, 20, 20),
  Y = c(20, 20, 60, 40),
  Z = c(60, 20, 20, 40)
)
coda <- coda / rowSums(coda)
coda <- as.matrix(coda)

x <- isopleuros:::ilr(coda)
y <- isopleuros:::ilr_inv(x)

expect_equivalent(y, coda)
