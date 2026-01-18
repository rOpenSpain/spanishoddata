spod_get_tourists <- function(
  year = 2024
) {
  # install.packages("pxmake")
  # install.packages("pxR")
  px_file <- "private/53000.px"
  pxm <- pxmake::px(input = px_file, validate = FALSE)
  pxr <- pxR::read.px(filename = px_file)
  class(pxr)
  str(pxr)
  head(pxr)
  head(pxr$DATA$value)

  library(pxmake)
  x1 <- pxmake::px(population_gl)
  class(population_gl)
  str(population_gl)
}
