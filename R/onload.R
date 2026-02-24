# nocov start
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.spanishoddata <- list(
    spanishoddata.graphql_api_endpoint = "https://mapas-movilidad.transportes.gob.es/api/graphql",
    spanishoddata.user_agent = "spanishoddata R package, https://github.com/rOpenSpain/spanishoddata/"
  )
  toset <- !(names(op.spanishoddata) %in% names(op))
  if (any(toset)) {
    options(op.spanishoddata[toset])
  }

  invisible()
}
# nocov end
