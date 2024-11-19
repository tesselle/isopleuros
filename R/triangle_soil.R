# TERNARY SOIL DIAGRAM
#' @include AllGenerics.R
NULL

#' @export
#' @rdname triangle_soil
triangle_soil_hypres <- function(labels = TRUE, symbol = FALSE, ...) {
  .triangle_soil("HYPRES", labels = labels, symbol = symbol, ...)
  invisible(NULL)
}

#' @export
#' @rdname triangle_soil
triangle_soil_folk <- function(labels = TRUE, symbol = FALSE, ...) {
  .triangle_soil("folk1954", labels = labels, symbol = symbol, ...)
  invisible(NULL)
}

#' @export
#' @rdname triangle_soil
triangle_soil_shepard <- function(labels = TRUE, symbol = FALSE, ...) {
  .triangle_soil("shepard1954", labels = labels, symbol = symbol, ...)
  invisible(NULL)
}

#' @export
#' @rdname triangle_soil
triangle_soil_usda <- function(labels = TRUE, symbol = FALSE, ...) {
  .triangle_soil("USDA1951", labels = labels, symbol = symbol, ...)
  invisible(NULL)
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
