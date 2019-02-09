#' Get Logo file name
#'
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
