test_that("CAS", {
  skip_if_not_installed("vdiffr")

  ## CAS
  chart_cas_mass <- function() {
    ternary_plot(NULL, axes = FALSE, ann = FALSE, frame.plot = TRUE)
    triangle_phase_cas(mol = FALSE, pch = 16)
  }
  vdiffr::expect_doppelganger("chart_cas_mass", chart_cas_mass)

  chart_cas_molarity <- function() {
    ternary_plot(NULL, axes = FALSE, ann = FALSE, frame.plot = TRUE)
    triangle_phase_cas(mol = TRUE, pch = 16)
  }
  vdiffr::expect_doppelganger("chart_cas_molarity", chart_cas_molarity)

  ## Ceramic
  chart_ceramic_mass <- function() {
    ternary_plot(NULL, axes = FALSE, ann = FALSE, frame.plot = TRUE)
    triangle_phase_ceramic(mol = FALSE, pch = 16)
  }
  vdiffr::expect_doppelganger("chart_ceramic_mass", chart_ceramic_mass)

  chart_ceramic_molarity <- function() {
    ternary_plot(NULL, axes = FALSE, ann = FALSE, frame.plot = TRUE)
    triangle_phase_ceramic(symbol = TRUE, mol = TRUE, pch = 16)
  }
  vdiffr::expect_doppelganger("chart_ceramic_molarity", chart_ceramic_molarity)
})
test_that("Soil texture", {
  ## HYPRES soil texture
  chart_soil_hypres <- function() {
    ternary_plot(NULL, xlab = "sand", ylab = "silt", zlab = "clay")
    triangle_soil_hypres()
  }
  vdiffr::expect_doppelganger("chart_soil_hypres", chart_soil_hypres)

  ## USDA (1951) soil texture
  chart_soil_usda <- function() {
    ternary_plot(NULL, xlab = "sand", ylab = "silt", zlab = "clay")
    triangle_soil_usda(symbol = TRUE)
  }
  vdiffr::expect_doppelganger("chart_soil_usda", chart_soil_usda)

  ## Folk (1954) soil texture
  chart_soil_folk <- function() {
    ternary_plot(NULL, xlab = "sand", ylab = "silt", zlab = "clay")
    triangle_soil_folk(symbol = TRUE)
  }
  vdiffr::expect_doppelganger("chart_soil_folk", chart_soil_folk)

  ## Shepard (1954) soil texture
  chart_soil_shepard <- function() {
    ternary_plot(NULL, xlab = "sand", ylab = "silt", zlab = "clay")
    triangle_soil_shepard()
  }
  vdiffr::expect_doppelganger("chart_soil_shepard", chart_soil_shepard)
})
