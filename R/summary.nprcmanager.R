#' summary.nprcmanagerErr Summary function for class nprcmanagerErr
#'
#' @return object of class summary.nprcmanagerErr
#'
#' @param errorLst object of class nprcmanagerErr and class list
#' @export
summary.nprcmanagerErr <- function(errorLst) {
  class(errorLst) <- "summary.nprcmanagerErr"
  errorLst
}
