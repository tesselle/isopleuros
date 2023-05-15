test_that("Plot", {
  skip_on_cran()

  ## Capture and cleanup plot output
  path <- file.path(tempdir(), "Rplot.pdf")
  on.exit(unlink(path))
  pdf(file = path)

  ## Save graphical parameters
  old_par <- graphics::par(no.readonly = TRUE)

  ## Plot...
  ternary_plot(NULL)

  ## ...changes the user's graphical parameters...
  new_par <- graphics::par(no.readonly = TRUE)
  # expect_identical(new_par, old_par) # Fails

  dev.off()

  ## ...but these are the dimensions of the plotting region
  ## (so it should be OK?)
  keys <- new_par[!(new_par %in% old_par)]
  expect_named(keys, c('pin', 'plt', 'usr', 'yaxp'))
})
test_that("Pairs", {
  skip_on_cran()

  ## Capture and cleanup plot output
  path <- file.path(tempdir(), "Rplot.pdf")
  on.exit(unlink(path))
  pdf(file = path)

  ## Save graphical parameters
  old_par <- graphics::par(no.readonly = TRUE)

  ## Plot...
  ternary_pairs(lava)

  ## ...does not change the user's graphical parameters
  new_par <- graphics::par(no.readonly = TRUE)
  expect_identical(new_par, old_par)

  dev.off()
})
