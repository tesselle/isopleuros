# TERNARY LABELS
#' @include AllGenerics.R
NULL

#' @export
#' @rdname ternary_labels
#' @aliases ternary_labels,numeric,numeric,numeric-method
setMethod(
  f = "ternary_labels",
  signature = c(x = "numeric", y = "numeric", z = "numeric"),
  definition = function(x, y, z, labels = seq_along(x),
                        type = c("text", "shadow"), ...) {
    ## Validation
    type <- match.arg(type, several.ok = FALSE)

    ## Compute label positions
    coords <- coordinates_ternary(x, y, z)
    labs <- compute_labels(x = coords$x, y = coords$y, labels = labels, ...)

    ## Draw labels
    fun <- switch(
      type,
      text = graphics::text,
      shadow = text_shadow
    )
    fun(labs, labels = labels, ...)

    coords <- utils::modifyList(coords, list(x = x, y = y, z = z))
    invisible(coords)
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
    coords <- methods::callGeneric(x = x$x, y = x$y, z = x$z,
                                   labels = labels, ...)
    invisible(coords)
  }
)


#' Compute Label Positions
#'
#' @return A [`list`] with elements `x`, `y` and `labels`.
#' @source
#'  This function is modeled after [car::pointLabel()] (originally from the
#'  \pkg{maptools} package).
#' @keywords internal
#' @noRd
compute_labels <- function(x, y, labels, ..., iter = 50,
                           cex = graphics::par("cex"),
                           font = NULL, vfont = NULL) {
  ## Coordinates
  bound <- graphics::par("usr")
  ratio <- graphics::par("pin")[1] / graphics::par("pin")[2] # x/y ratio

  to_unity <- function(x, y) {
    list(x = (x - bound[1]) / (bound[2] - bound[1]) * ratio,
         y = (y - bound[3]) / (bound[4] - bound[3]) / ratio)
  }
  to_usr <- function(x, y) {
    list(x = bound[1] + x / ratio * (bound[2] - bound[1]),
         y = bound[3] + y * ratio * (bound[4] - bound[3]))
  }

  xy <- to_unity(x = x, y = y)
  x <- xy$x
  y <- xy$y
  n <- length(x)

  ## 8 positions: corners and side mid-points of the rectangle
  ## Position 7 (top right) is the most preferred
  width <- graphics::strwidth(labels, units = "figure", cex = cex,
                              font = font, vfont = vfont)
  height <- graphics::strheight(labels, units = "figure", cex = cex,
                                font = font, vfont = vfont)
  width <- (width + 0.02) * ratio
  height <- (height + 0.02) / ratio

  makeoff <- function(pos) {
    c(-1, -1, -1, 0, 0, 1, 1, 1)[pos] * (width / 2) +
      1i * c(-1, 0, 1, -1, 1, -1, 0, 1)[pos] * (height / 2)
  }

  ## Find intersection area of two rectangles
  overlap <- function(xy1, off1, xy2, off2) {
    w <- pmin(Re(xy1 + off1 / 2), Re(xy2 + off2 / 2)) -
      pmax(Re(xy1 - off1 / 2), Re(xy2 - off2 / 2))
    h <- pmin(Im(xy1 + off1 / 2), Im(xy2 + off2 / 2)) -
      pmax(Im(xy1 - off1 / 2), Im(xy2 - off2 / 2))
    w[w <= 0] <- 0
    h[h <= 0] <- 0
    w * h
  }

  objective <- function(gene) {
    offset <- makeoff(gene)

    if (!is.null(rectidx1)) {
      area <- sum(overlap(xy[rectidx1] + offset[rectidx1], rectv[rectidx1],
                          xy[rectidx2] + offset[rectidx2], rectv[rectidx2]))
    } else {
      area <- 0
    }

    ## Penalize labels which go outside the image area
    ## Count points outside of the image
    a <- Re(xy + offset - rectv / 2) < 0 | Re(xy + offset + rectv / 2) > ratio
    b <- Im(xy + offset - rectv / 2) < 0 | Im(xy + offset + rectv / 2) > 1 / ratio
    outside <- sum(a | b)
    res <- 1000 * area + outside
    res
  }

  # Make a list of label rectangles in their reference positions,
  # centered over the map feature; the real labels are displaced
  # from these positions so as not to overlap
  # Note that some labels can be bigger than others
  xy <- x + 1i * y
  rectv <- width + 1i * height

  rectidx1 <- rectidx2 <- array(0, (length(x)^2 - length(x)) / 2)
  k <- 0
  for (i in seq_along(x))
    for (j in seq_len(i - 1)) {
      k <- k + 1
      rectidx1[k] <- i
      rectidx2[k] <- j
    }
  maylap <- overlap(xy[rectidx1], 2 * rectv[rectidx1],
                    xy[rectidx2], 2 * rectv[rectidx2]) > 0
  rectidx1 <- rectidx1[maylap]
  rectidx2 <- rectidx2[maylap]

  ## Simulated annealing
  ## Initial state
  gene <- rep(8, n)
  score <- objective(gene)
  ## Initial "best" solution
  bestgene <- gene
  bestscore <- score
  iter <- seq_len(iter)
  temp <- 2.5
  for (i in iter) {
    k <- 1 # Energy evaluation count
    for (j in iter) {
      newgene <- gene
      newgene[sample(n, 1)] <- sample(8, 1)
      newscore <- objective(newgene)
      if (newscore <= score || stats::runif(1) < exp((score - newscore) / temp)) {
        ## keep the new set if it has the same or better score or
        ## if it's worse randomly based on the annealing criteria
        k <- k + 1
        score <- newscore
        gene <- newgene
      }
      if (score <= bestscore) {
        bestscore <- score
        bestgene <- gene
      }
      if (bestscore == 0 || k == 10) break
    }
    if (bestscore == 0) break
    temp <- 0.9 * temp
  }

  nx <- Re(xy + makeoff(bestgene))
  ny <- Im(xy + makeoff(bestgene))

  xy <- to_usr(x = nx, y = ny)
  xy$labels <- labels
  xy
}

#' Shadow Text
#'
#' @param x,y A [`numeric`] vector. If `y` is `NULL`, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param labels A [`character`] vector specifying the text to be written.
#' @param width Thickness of the shadow, as a fraction of the plotting size.
#' @param theta Angles for plotting the background.
#' @param cex A [`numeric`] character expansion factor.
#' @param col The color to be used for the text.
#' @param bg The color to be used for the shadow.
#' @param font,vfont The font to be used (see [graphics::text()]).
#' @param ... Further parameters to be passed to [graphics::text()].
#' @return
#'  `text_shadow()` is called it for its side-effects: it results in a graphic
#'  being displayed.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
text_shadow <- function(x, y = NULL, labels = seq_along(x$x),
                        width = 1/10, theta = seq(0, 2 * pi, length.out = 50),
                        cex = graphics::par("cex"), col = graphics::par("fg"),
                        bg = graphics::par("bg"), font = NULL, vfont = NULL, ...) {

  x <- grDevices::xy.coords(x = x, y = y)

  xo <- width * graphics::strwidth("M", units = "user", cex = cex, font = font, vfont = vfont)
  yo <- width * graphics::strheight("X", units = "user", cex = cex, font = font, vfont = vfont)

  for (i in theta) {
    graphics::text(x = x$x + cos(i) * xo, y = x$y + sin(i) * yo, labels = labels,
                   col = bg, cex = cex, font = font, vfont = vfont, ...)
  }

  graphics::text(x = x$x, y = x$y, labels = labels, col = col, cex = cex,
                 font = font, vfont = vfont, ...)

  invisible(NULL)
}
