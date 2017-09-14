#' Get the direct ancestors of selected animals
#'
#' Gets direct ancestors from labkey \code{study} schema and \code{demographics}
#' table.
#' @return dataframe with pedigree structure having all of the direct ancestors
#' for the Ids provided.
#' @param labkeyNode a string specifying the \code{labkeyNode} for the labkey
#' server. Defaults to "boomer.txbiomed.org",
#' @param ids character vector with Ids.
#' @param colSelect optional character vector of columns to return in the
#' dataframe.
#' @export
getLkDirectAncestors <- function(ids) {
  siteInfo <- getSiteInfo()
  colSet <- siteInfo$lkPedColumns
  pedSourceDf <- getDemographics(colSelect = colSet)
  names(pedSourceDf) <- siteInfo$mapPedColumns
  parents <- ids
  len <- length(parents)
  ancestorsDf <- pedSourceDf[pedSourceDf$id %in% ids, ]
  while (len > 0) {
    parents <- getParents(pedSourceDf, parents)
    len <- length(parents)
    if (len > 0) {
      ancestorsDf <- rbind(ancestorsDf,
                           pedSourceDf[pedSourceDf$id %in% parents, ],
                           stringsAsFactors = FALSE)
    }
  }
  ancestorsDf[!duplicated(ancestorsDf$id), ]
}
