% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/ternary_text.R
\docType{methods}
\name{ternary_text}
\alias{ternary_text}
\alias{ternary_text-method}
\alias{ternary_text,numeric,numeric,numeric-method}
\alias{ternary_text,ANY,missing,missing-method}
\title{Add Text to a Ternary Plot}
\usage{
ternary_text(x, y, z, ...)

\S4method{ternary_text}{numeric,numeric,numeric}(
  x,
  y,
  z,
  center = FALSE,
  scale = FALSE,
  labels = seq_along(x),
  ...
)

\S4method{ternary_text}{ANY,missing,missing}(x, center = FALSE, scale = FALSE, labels = seq_along(x$x), ...)
}
\arguments{
\item{x, y, z}{A \code{\link{numeric}} vector giving the x, y and z ternary coordinates
of a set of points. If \code{y} and \code{z} are missing, an attempt is made to
interpret \code{x} in a suitable way (see \code{\link[grDevices:xyz.coords]{grDevices::xyz.coords()}}).}

\item{...}{Further arguments to be passed to \code{\link[graphics:text]{graphics::text()}}.}

\item{center}{A \code{\link{logical}} scalar specifying wether the data should be
centered, or a \code{\link{numeric}} vector giving the center.}

\item{scale}{A \code{\link{logical}} scalar specifying wether the data should be
scaled, or a \code{\link{numeric}} vector giving the scale factor.}

\item{labels}{A \code{\link{character}} vector or \code{\link{expression}} specifying the text
to be written.}
}
\value{
\code{ternary_text()} is called it for its side-effects.
}
\description{
Draws the strings given in the vector \code{labels} at the coordinates given by
\code{x}, \code{y} and \code{z}.
}
\examples{
## Compositional data
coda <- data.frame(
  X = c(20, 60, 20),
  Y = c(20, 20, 60),
  Z = c(60, 20, 20)
)

## Add text
ternary_plot(NULL, panel.first = ternary_grid())
ternary_text(coda, labels = c("A", "B", "C"), col = "red", cex = 2)

}
\seealso{
\code{\link[graphics:text]{graphics::text()}}

Other geometries: 
\code{\link{ternary_arrows}()},
\code{\link{ternary_crosshairs}()},
\code{\link{ternary_image}()},
\code{\link{ternary_labels}()},
\code{\link{ternary_lines}()},
\code{\link{ternary_points}()},
\code{\link{ternary_polygon}()},
\code{\link{ternary_segments}()}
}
\author{
N. Frerebeau
}
\concept{geometries}
