#' Get Logo file name
#'
#' @export
getLogo <- function() {
  logo <- list()
  if (getSiteInfo()$center == "SNPRC") {
    logo$file <- "../snprc-new2color.png"
    logo$height <- 80L
    logo$width <- 400L
  } else {
    logo$file <- "../ONPRC_Logo.png"
    logo$height <- 80L
    logo$width <- 250L
  }
  logo
}
