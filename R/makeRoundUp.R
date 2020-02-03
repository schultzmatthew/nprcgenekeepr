#' Round up the provided integer vector \code{int} according to the
#' \code{modulas}.
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @param int integer vector
#' @param modulas integer value to use as the divisor.
makeRoundUp <- function(int, modulas) {
  int + modulas - int %% modulas
}
