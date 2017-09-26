#' Round up the provided integer vector \code{int} according to the
#' \code{modulas}.
#'
#' @param int integer vector
#' @param modulas integer value to use as the divisor.
#' @export
makeRoundUp <- function(int, modulas) {
  int + modulas - int %% modulas
}
