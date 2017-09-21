#' Is vector empty or all NA values.
#'
#' @return \code{TRUE} if x is a zero-length vector.
#'
#' @param x vector of any type.
#' @export
isEmpty <- function(x) {
  x <- x[!is.na(x)]
  return(length(x) == 0)
}
