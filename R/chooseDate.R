#' Choose date based on \code{earlier} flag.
#'
#' Part of Pedigree Curation
#'
#' Given two dates, one is selected to be returned based on whether
#' it occurred earlier or later than the other. \code{NAs} are ignored if
#' possible.
#'
#' @param d1 \code{Date} vector with the first of two dates to compare.
#' @param d2 \code{Date} vector with the second of two dates to compare.
#' @param earlier logical variable with \code{TRUE} if the earlier of the two
#' dates is to be returned, otherwise the later is returned. Default is
#' \code{TRUE}.
#'
#' @return \code{Date} vector of chosen dates or \code{NA} where neither
#' is provided
#' @export
chooseDate <- function(d1, d2, earlier = TRUE) {
  if (is.na(d1)) {
    return(d2)
  }
  else if (is.na(d2)) {
    return(d1)
  }
  else if ((d1 < d2) & earlier) {
    return(d1)
  }
  else if ((d1 > d2) & !earlier) {
    return(d1)
  }
  else{
    return(d2)
  }
}
