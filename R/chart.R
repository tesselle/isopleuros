# TERNARY DIAGRAM
#' @include AllGenerics.R
NULL

# Phase diagram ================================================================
#' @export
#' @rdname triangle_phase_cas
triangle_phase_cas <- function(labels = TRUE, symbol = FALSE,
                               mol = FALSE, ...) {
  oxide_mass <- c(CaO  = 56.0774, Al2O3 = 101.9600, SiO2 = 60.0800)
  .triangle_phases("CAS", oxide_mass = oxide_mass, mol = mol,
                   labels = labels, symbol = symbol, ...)
}

#' @export
#' @rdname triangle_phase_cas
triangle_phase_ceramic <- function(labels = TRUE, symbol = FALSE,
                                   mol = FALSE, ...) {
  oxide_mass <- c(CaO  = 56.0774, Al2O3 = 101.9600, SiO2 = 60.0800)
  .triangle_phases("ceramic", oxide_mass = oxide_mass, mol = mol,
                   labels = labels, symbol = symbol, ...)
}

.triangle_phases <- function(chart, oxide_mass, mol = FALSE,
                             labels = TRUE, symbol = FALSE, ...) {
  ## Graphical parameters
  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")

  poly <- .phases[[chart]]

  if (!mol) {
    oxyde_mol <- as.matrix(poly[, c(1, 2, 3)])

    ## Molar mass (g/mol)
    phase_mass <- oxyde_mol %*% oxide_mass

    ## Oxide weight (%)
    poly[, c(1, 2, 3)] <- t(t(oxyde_mol) * oxide_mass) / as.vector(phase_mass)
  }

  lab <- poly[!duplicated(poly$label), ]
  txt <- if (symbol && !all(lab$symbol == "")) lab$symbol else lab$label

  poly$group <- factor(poly$group, levels = unique(poly$group))
  poly <- split(poly, f = poly$group)

  for (i in poly) {
    ternary_polygon(i, ...)
  }
  if (labels) {
    ternary_points(lab, cex = cex.lab, col = col.lab, ...)
    ternary_text(lab, label = txt, pos = lab$pos,
                 cex = cex.lab, col = col.lab, font = font.lab)
  }
}

# Soil texture =================================================================
#' @export
#' @rdname triangle_soil
triangle_soil_hypres <- function(labels = TRUE, symbol = FALSE, ...) {
  .triangle_soil("HYPRES", labels = labels, symbol = symbol, ...)
}

#' @export
#' @rdname triangle_soil
triangle_soil_folk <- function(labels = TRUE, symbol = FALSE, ...) {
  .triangle_soil("folk1954", labels = labels, symbol = symbol, ...)
}

#' @export
#' @rdname triangle_soil
triangle_soil_shepard <- function(labels = TRUE, symbol = FALSE, ...) {
  .triangle_soil("shepard1954", labels = labels, symbol = symbol, ...)
}

#' @export
#' @rdname triangle_soil
triangle_soil_usda <- function(labels = TRUE, symbol = FALSE, ...) {
  .triangle_soil("USDA1951", labels = labels, symbol = symbol, ...)
}

.triangle_soil <- function(chart, labels = TRUE, symbol = FALSE, ...) {
  ## Graphical parameters
  cex.lab <- list(...)$cex.lab %||% graphics::par("cex.lab")
  col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
  font.lab <- list(...)$font.lab %||% graphics::par("font.lab")

  poly <- .soil[[chart]]
  txt_lab <- unique(poly$label)
  txt_symb <- unique(poly$symbol)

  poly$label <- factor(poly$label, levels = unique(poly$label))
  poly <- split(poly, f = poly$label)

  for (i in poly) {
    ternary_polygon(i, ...)
  }
  if (labels) {
    lab <- lapply(X = poly, FUN = function(x) colMeans(x[, c(1, 2, 3, 4)]))
    txt <- if (symbol && !all(txt_symb == "")) txt_symb else txt_lab

    for (i in seq_along(lab)) {
      ternary_text(
        x = lab[[i]][[1]],
        y = lab[[i]][[2]],
        z = lab[[i]][[3]],
        labels = txt[[i]],
        srt = lab[[i]][[4]],
        cex = cex.lab,
        col = col.lab,
        font = font.lab
      )
    }
  }
}
