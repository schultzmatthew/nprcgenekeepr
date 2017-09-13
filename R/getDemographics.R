#' Get demographic data
#'
#' This is a thin wrapper around \code{labkey.selectRows()}.
#'
#' @param colSelect (optional) a vector of comma separated strings specifying
#' which columns of a dataset or view to import
#' @export
getDemographics <- function(colSelect = NULL) {
  demoDf <- labkey.selectRows(
    baseUrl = siteInfo$baseUrl, folderPath = siteInfo$folderPath,
    schemaName = siteInfo$schemaName, queryName = siteInfo$queryName,
    viewName = "", colSort = NULL, colFilter = NULL,
    containerFilter = NULL, colNameOpt = "fieldname",
    maxRows = NULL, colSelect = colSelect,
    showHidden = TRUE)
  demoDf
}
