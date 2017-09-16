#' Get the direct ancestors of selected animals
#'
#' Gets direct ancestors from labkey \code{study} schema and \code{demographics}
#' table.
#' @return dataframe with pedigree structure having all of the direct ancestors
#' for the Ids provided.
#' @param ids character vector with Ids.
#' @export
getLkDirectRelatives <- function(ids) {
  siteInfo <- getSiteInfo()
  colSet <- siteInfo$lkPedColumns
  pedSourceDf <- getDemographics(colSelect = colSet)
  names(pedSourceDf) <- siteInfo$mapPedColumns
  parents <- ids
  offspring <- ids
  len <- length(parents)
  relativesDf <- pedSourceDf[pedSourceDf$id %in% ids, ]
  while (len > 0) {
    parents <- getParents(pedSourceDf, parents)
    offspring <- getOffspring(pedSourceDf, offspring)
    len <- length(parents)
    if (len > 0) {
      relativesDf <- rbind(relativesDf,
                           pedSourceDf[pedSourceDf$id %in% parents, ],
                           stringsAsFactors = FALSE)
      relativesDf <- rbind(relativesDf,
                           pedSourceDf[pedSourceDf$id %in% offspring, ],
                           stringsAsFactors = FALSE)
      relativesDf <- relativesDf[!duplicated(relativesDf$id), ]
    }
  }
  relativesDf[!duplicated(relativesDf$id), ]
}
