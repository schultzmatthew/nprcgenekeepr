#' Get proportion of Low genetic value animals
#'
#' @return List of the proportion of Low genetic value animals and the
#' dashboard color to be assigned base on that proportion.
#' @param geneticValues character vector of the genetic values. This vector
#' is to have already been filtered to remove animals that should not be
#' included in the calculation.
getProportionLow <- function(geneticValues) {
  proportion <-
    length(geneticValues[stri_detect_fixed(geneticValues, "Low")]) /
    length(geneticValues)
  if (proportion > 0.5)
    color <- "red"
  else if (proportion <= 0.5 & proportion >= 0.3)
    color <- "yellow"
  else if (proportion < 0.3)
    color <- "green"
  list(proportion = proportion, color = color)
}
#' #' Genetic Diversity \code{red()}
#' #'
#' #' @return \code{TRUE} if \code{value} tests as \code{TRUE} otherwise
#' #' \code{FALSE}.
#' #' @param func function to test value
#' diversityRed <- function(func) {
#'   function(value) func(value)
#' }
#' dRed <- diversityRed(function(value) {value > 0.5})
