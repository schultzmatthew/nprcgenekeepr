#' Get Logo file name
#'
#' @export
getLogo <- function() {
  logo <- list()
  if (getSiteInfo()$center == "SNPRC") {
    logo$file <- "../NPRCmanager 2c logo stamp w-tagline_300dpi.jpg"
    logo$height <- 200L
    logo$width <- 350L
  } else {
    logo$file <- "../NPRCmanager 2c logo stamp w-tagline_300dpi.jpg"
    logo$height <- 200L
    logo$width <- 350L
  }
  logo
}
