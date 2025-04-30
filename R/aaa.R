# INTERNAL ENVIRONMENT

the <- new.env(parent = emptyenv())
the$top <- sqrt(3) / 2
the$center <- NULL # Updated by plot()
the$scale <- NULL  # Updated by plot()

get_center <- function(...) {
  get("center", envir = the)
}

get_scale <- function(...) {
  get("scale", envir = the)
}
