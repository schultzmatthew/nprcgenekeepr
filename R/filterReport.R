#' Filters a genetic value report down to only the specified animals
#'
#' @param ids character vector of animal IDs
#' @param rpt a dataframe with required colnames \code{id}, \code{gu},
#' \code{zScores}, \code{import}, \code{totalOffspring}, which is
#' a data.frame of results from a genetic value analysis.
#' @return A copy of report specific to the specified animals
#' @export
filterReport <- function(ids, rpt) {
  return(rpt[rpt$id %in% ids, ])
}
