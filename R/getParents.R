#' Get parents to corresponding animal IDs provided
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcgenekeepr
#' @param pedSourceDf dataframe with pedigree structure having at least the
#' columns id, sire, and dam.
#' @param ids character vector of animal IDs
#' @export
getParents <- function(pedSourceDf, ids) {
  unique(c(pedSourceDf$sire[(pedSourceDf$id %in% ids &
                                 !is.na(pedSourceDf$sire))],
           pedSourceDf$dam[(pedSourceDf$id %in% ids &
                                !is.na(pedSourceDf$dam))]))
}
