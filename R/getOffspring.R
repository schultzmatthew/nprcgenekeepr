#' Get offspring to corresponding animal IDs provided
#'
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#' @param pedSourceDf dataframe with pedigree structure having at least the
#' columns id, sire, and dam.
#' @param ids character vector of animal IDs
#' @export
getOffspring <- function(pedSourceDf, ids) {
  unique(c(pedSourceDf$id[(pedSourceDf$sire %in% ids &
                                 !is.na(pedSourceDf$sire))],
           pedSourceDf$id[(pedSourceDf$dam %in% ids &
                                !is.na(pedSourceDf$dam))]))
}
