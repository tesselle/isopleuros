.onLoad <- function(libname, pkgname) {
  op <- options()
  op.isopleuros <- list(
    isopleuros.missing = FALSE,
    isopleuros.center = NULL,
    isopleuros.scale = NULL
  )
  toset <- !(names(op.isopleuros) %in% names(op))
  if(any(toset)) options(op.isopleuros[toset])

  invisible()
}
