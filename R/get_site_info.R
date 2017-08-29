#' Get site information
#'
#' @returns A list of site specific informatio used by the application.
#'
#' This version is a stub in that it has to be modified by each site.
#' The plan is to either replace this routine or give it a dynamic way to
#' define the needed information.
#'
#' Currently this returns the following character strings in a named list.
#' \enumerate{
#'   \item{center}{One of "SNPRC" or "ONPRC"}
#'   \item{baseUrl}{If \code{center} is "SNPRC", baseUrl is one of
#'   "https://boomer.txbiomed.org/labkey" or "https://vger.txbiomed.org/labkey".
#'   To allow testing, if \code{center} is "ONPRC" baseUrl is
#'   "https://boomer.txbiomed.org/labkey".}
#'   \item{schemaName}{If \code{center} is "SNPRC", schemaName is "study".
#'   If \code{center} is "ONPRC", schemaName is "study"}
#'   \item{folderPath} If \code{center} is "SNPRC", folderPath is "/SNPRC".
#'   If \code{center} is "ONPRC", folderPath is "/ONPRC"}
#'   \item{queryName}{is "demographics"}
#'}
#' @export
get_site_info <- function() {
  list(
    center = "SNPRC",
    baseUrl = "https://boomer.txbiomed.org/labkey",
    schemaName = "study",
    folderPath = "/SNPRC",
    queryName = "demographics")
}