#' Get the maximum age of live animals in the pedigree.
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' @param ped dataframe with pedigree
#' @export
getPedMaxAge <- function(ped) {
  max(ped$age, na.rm = TRUE)
}
