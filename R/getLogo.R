#' Get Logo file name
#'
#' @export
getLogo <- function() {
  logo <- list()
  if (getSiteInfo()$center == "SNPRC") {
    logo$file <- "../combined_ONPRC_SNPRC.png"
    logo$height <- 160L
    logo$width <- 400L
  } else {
    logo$file <- "../combined_ONPRC_SNPRC.png"
    logo$height <- 160L
    logo$width <- 350L
  }
  logo
}
