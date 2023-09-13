# TERNARY LABELS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_labels
#' @aliases ternary_labels,numeric,numeric,numeric-method
setMethod(
  f = "ternary_labels",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, labels = seq_along(x), ...) {
    coords <- coordinates_ternary(x, y, z)

    ## Compute label positions
    labs <- compute_labels(x = coords$x, y = coords$y, labels = labels, ...)
    xt <- labs$x
    yt <- labs$y
    wt <- labs$width
    ht <- labs$height

    shadowtext(x = xt, y = yt, labels = labels, ...)
    invisible(data.frame(x = x, y = y, z = z))
  }
)

#' @export
#' @rdname ternary_labels
#' @aliases ternary_labels,ANY,missing,missing-method
setMethod(
  f = "ternary_labels",
  signature = c(x = "ANY", y = "missing", z = "missing"),
  definition = function(x, labels = seq_along(x$x), ...) {
    x <- grDevices::xyz.coords(x)
    methods::callGeneric(x = x$x, y = x$y, z = x$z, labels = labels, ...)
  }
)

# Adapted from vegan::ordipointlabel() by Jari Oksanen
compute_labels <- function(x, y, labels, cex = graphics::par("cex"),
                           font = graphics::par("font")) {
  xy <- cbind.data.frame(x, y)

  em <- graphics::strwidth("m", cex = min(cex))
  ex <- graphics::strheight("x", cex = min(cex))
  ltr <- em * ex

  width <- graphics::strwidth(labels, cex = cex, font = font) + em
  height <- graphics::strheight(labels, cex = cex, font = font) + ex
  box <- cbind.data.frame(width, height)

  makeoff <- function(pos, lab) {
    cbind(
      c(0, 1, 0, -1, 0.9, 0.9, -0.9, -0.9)[pos] * lab[, 1] / 2,
      c(1, 0, -1, 0, 0.8, -0.8, -0.8, 0.8)[pos] * lab[, 2] / 2
    )
  }
  overlap <- function(xy1, off1, xy2, off2) {
    pmax(0, pmin(xy1[, 1] + off1[, 1]/2, xy2[, 1] + off2[, 1]/2) -
           pmax(xy1[, 1] - off1[, 1]/2, xy2[, 1] - off2[, 1]/2)) *
      pmax(0, pmin(xy1[, 2] + off1[, 2]/2, xy2[, 2] + off2[, 2]/2) -
             pmax(xy1[, 2] - off1[, 2]/2, xy2[, 2] - off2[, 2]/2))
  }

  n <- nrow(xy)
  j <- as.vector(stats::as.dist(row(matrix(0, n, n))))
  k <- as.vector(stats::as.dist(col(matrix(0, n, n))))

  maylap <- overlap(xy[j, ], 2 * box[j, ], xy[k, ], 2 * box[k, ]) > 0
  j <- j[maylap]
  k <- k[maylap]
  jk <- sort(unique(c(j, k)))

  nit <- min(48 * length(jk), 10000)
  pos <- rep(1, n)

  ## Simulated annealing
  fn <- function(pos) {
    off <- makeoff(pos, box)
    val <- sum(overlap(xy[j, ] + off[j, ], box[j, ], xy[k, ] + off[k, ], box[k, ]))
    val <- val / ltr + sum(pos > 1) * 0.1 + sum(pos > 4) * 0.1
  }
  gr <- function(pos) {
    take <- sample(jk, 1)
    pos[take] <- sample((1:8)[-pos[take]], 1)
    pos
  }
  sol <- stats::optim(par = pos, fn = fn, gr = gr, method = "SANN",
                      control = list(maxit = nit))

  coord <- xy + makeoff(sol$par, box)
  coord$width <- width
  coord$height <- height
  coord
}

shadowtext <- function(x, y, labels, ...,
                       theta = seq(0, 2 * pi, length.out = 50),
                       r = 0.1,
                       cex = graphics::par("cex"),
                       col = graphics::par("fg"),
                       bg = graphics::par("bg"),
                       font = graphics::par("font"),
                       xpd = TRUE) {

  xo <- r * graphics::strwidth("A", cex = cex, font = font, ...)
  yo <- r * graphics::strheight("A", cex = cex, font = font, ...)

  for (i in theta) {
    graphics::text(x + cos(i) * xo, y + sin(i) * yo, labels,
                   col = bg, cex = cex, font = font, xpd = xpd)
  }

  graphics::text(x, y, labels, col = col, cex = cex, font = font, xpd = xpd)
}
