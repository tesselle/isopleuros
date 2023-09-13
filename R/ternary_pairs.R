# PAIRS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_pairs
#' @aliases ternary_pairs,matrix-method
setMethod(
  f = "ternary_pairs",
  signature = c(x = "matrix"),
  definition = function(x, margin = NULL, ...) {

    ## Save and restore graphical parameters
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    ## Layout
    n <- ncol(x)
    ni <- seq_len(n)
    parts <- colnames(x) %||% paste0("X", ni)
    zlab <- "*"

    if (!is.null(margin)) {
      margin <- if (is.character(margin)) which(parts == margin) else as.integer(margin)
      star <- x[, margin]
      zlab <- parts[margin]
      parts <- parts[-margin]
      n <- n - 1
      ni <- ni[-margin]
    }

    k <- (n^2 - n) / 2
    lay <- matrix(0, nrow = n, ncol = n)
    lay[lower.tri(lay, diag = FALSE)] <- seq_len(k)
    diag(lay) <- seq(k + 1, k + n)
    lay <- t(lay)

    graphics::layout(lay)
    graphics::par(mar = c(0, 0, 0, 0) + 0.1, oma = c(1, 1, 1, 1))

    ## Ternary plots
    p <- utils::combn(ni, 2, simplify = FALSE)
    for (i in p) {
      if (is.null(margin)) {
        star <- apply(X = x[, -i, drop = FALSE], MARGIN = 1, FUN = gmean)
      }
      z <- cbind(x[, i], star)
      ternary_plot(z, zlab = zlab, axes = FALSE, frame.plot = TRUE, ...)
    }

    ## Graphical parameters
    str.wid <- max(graphics::strwidth(parts, "user"))
    cex.lab <- list(...)$cex.lab %||% max(0.8, min(2, 0.9 / str.wid))
    col.lab <- list(...)$col.lab %||% graphics::par("col.lab")
    font.lab <- list(...)$font.lab %||% graphics::par("font.lab")

    ## Add labels
    for (part in parts) {
      graphics::plot(NULL, xlim = c(0, 1), ylim = c(0, 1),
                     axes = FALSE, ann = FALSE)
      graphics::text(x = 0.5, y = 0.5, labels = part,
                     cex = cex.lab, col = col.lab, font = font.lab)
    }

    invisible(x)
  }
)

#' @export
#' @rdname ternary_pairs
#' @aliases ternary_pairs,data.frame,missing,missing-method
setMethod(
  f = "ternary_pairs",
  signature = c(x = "data.frame"),
  definition = function(x, margin = NULL, ...) {
    x <- data.matrix(x)
    methods::callGeneric(x = x, margin = margin, ...)

    invisible(x)
  }
)
