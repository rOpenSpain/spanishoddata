#' View codebooks for v1 and v2 open mobility data
#' 
#' @description
#' Opens relevant vignette.
#' 
#' 
#' @param ver An `integer` or `numeric` value. The version of the data. Defaults to 1. Can be 1 for v1 (2020-2021) data and 2 for v2 (2022 onwards) data.
#' @return Nothing, calls relevant vignette.
#' @export
spod_codebook = function(ver = 1) {
  if (ver == 1){
    vignette(
      topic = "v1-2020-2021-mitma-data-codebook",
      package = "spanishoddata"
    )
  } else if (ver == 2) {
    stop("Not implemented yet")
  }
}
