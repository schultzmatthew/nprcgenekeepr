#' Get Logo file name
#'
#' @export
getLogo <- function() {
  logo <- list()
  if (getSiteInfo()$center == "SNPRC") {
    logo$file <- "../snprc-new2color.png"
    logo$height <- 80
    logo$width <- 400
  } else {
    logo$file <- "../ONPRC_Logo.png"
    logo$height <- 80
    logo$width <- 250
  }
  logo
}