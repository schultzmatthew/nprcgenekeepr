#' Get Logo file name
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' @export
getLogo <- function() {
  logo <- list()
  if (getSiteInfo()$center == "SNPRC") {
    logo$file <- "../nprcmanager_2_color_logo.jpg"
    logo$height <- 200L
    logo$width <- 350L
  } else {
    logo$file <- "../nprcmanager_2_color_logo.jpg"
    logo$height <- 200L
    logo$width <- 350L
  }
  logo
}
