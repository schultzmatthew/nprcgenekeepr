#' Get the maximum of the absolute values of the negative (males) and positive
#' (female) animal counts.
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' This is used to scale the pyramid plot symmetrically.
#'
#' @param bins integer vector with numbers of individuals in each bin
#' @param axModulas integer value used in the modulas function to determine
#' the interval between possible maxAx values.
#'
getMaxAx <- function(bins, axModulas) {
  makeRoundUp(max(max(bins$male), max(bins$female)), axModulas)
}
