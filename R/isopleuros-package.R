#' @details
#' \tabular{ll}{
#'  **Package:** \tab isopleuros \cr
#'  **Type:** \tab Package \cr
#'  **Version:** \tab 1.4.0 \cr
#'  **License:** \tab GPL-3 \cr
#'   **Zenodo:** \tab \doi{10.5281/zenodo.7940389} \cr
#' }
#'
#' @section Package options:
#'  `isopleuros` uses the following [options()] to configure behavior:
#'  * `isopleuros.missing`: a [`logical`] scalar. Should [missing values][NA]
#'    be replaced with zeros before the ternary coordinates computation
#'    proceeds? If `FALSE` (the default), incomplete cases are removed.
#'
#' @author
#' **Full list of authors and contributors** (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab *Université Bordeaux Montaigne, France* \cr
#'  Brice Lebrun \tab *Université Bordeaux Montaigne, France* \cr
#' }
#'
#' **Package maintainer**
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' Archéosciences Bordeaux (UMR 6034)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' 33607 Pessac cedex\cr
#' France
#' @name isopleuros-package
#' @aliases isopleuros
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @importFrom graphics arrows layout lines par points polygon segments
#' strheight strwidth text par plot plot.default plot.new plot.window
#' rasterImage
#' @importFrom grDevices as.graphicsAnnot as.raster chull colorRampPalette
#' contourLines dev.flush dev.hold hcl.colors xyz.coords
#' @importFrom methods setGeneric setMethod .valueClassTest
#' @importFrom utils combn modifyList
#' @importFrom stats as.dist optim
NULL
