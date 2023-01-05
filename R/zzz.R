.onLoad <- function(libname, pkgname) {
  op <- options()
  op.isopleuros <- list(
    isopleuros.missing = FALSE
  )
  toset <- !(names(op.isopleuros) %in% names(op))
  if(any(toset)) options(op.isopleuros[toset])

  invisible()
}
