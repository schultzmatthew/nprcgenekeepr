#' getChangedColsTab skeleton of list of errors
#'
#' @return HTML formated error list
#' @param errorLst list of errors and changes made by \code{qcStudbook}
#' @param pedigreeFileName name of file provided by user on Input tab
#' @export
getChangedColsTab <- function(errorLst, pedigreeFileName) {
  tabPanel("Changed Columns",
           div(HTML(insertChangedColsTab(errorLst, pedigreeFileName))))
}
