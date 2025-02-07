#' View codebooks for v1 and v2 open mobility data
#' 
#' @description
#' 
#' `r lifecycle::badge("stable")`
#' 
#' Opens relevant vignette with a codebook for v1 (2020-2021) and v2 (2022 onwards) data or provide a webpage if vignette is missing.
#' 
#' 
#' @param ver An `integer` or `numeric` value. The version of the data. Defaults to 1. Can be `1` for v1 (2020-2021) data and 2 for v2 (2022 onwards) data.
#' @return Nothing, opens vignette if it is installed. If vignette is missing, prints a message with a link to a webpage with the codebook.
#' @importFrom utils vignette
#' @export
#' @examples
#' 
#' # View codebook for v1 (2020-2021) data
#' spod_codebook(ver = 1)
#' 
#' # View codebook for v2 (2022 onwards) data
#' spod_codebook(ver = 2)
#' 
spod_codebook <- function(ver = 1) {
  # Validate input
  checkmate::assertIntegerish(ver, max.len = 1)
  if (!ver %in% c(1, 2)) {
    stop("Invalid version number. Must be 1 (for v1 2020-2021 data) or 2 (for v2 2022 onwards).")
  }

  if (ver == 1){
    help <- vignette(
      topic = "v1-2020-2021-mitma-data-codebook",
      package = "spanishoddata"
    )
    if( inherits(help, what = "vignette") ){
      return(help)
    } else {
      message("For some reason the codebook was not installed with the package. Please refer to the online version at: https://ropenspain.github.io/spanishoddata/articles/v1-2020-2021-mitma-data-codebook.html")
    }
  } else if (ver == 2) {
    help <- vignette(
      topic = "v2-2022-onwards-mitma-data-codebook",
      package = "spanishoddata"
    ) 
    if( inherits(help, what = "vignette") ){
      return(help)
    } else {
      message("For some reason the codebook was not installed with the package. Please refer to the online version at: https://ropenspain.github.io/spanishoddata/articles/v2-2022-onwards-mitma-data-codebook.html")
    }
  }
}
