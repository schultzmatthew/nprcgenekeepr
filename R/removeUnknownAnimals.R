#' removeUnknownAnimals Removes unknown animals added to pedigree that serve as
#' placeholders for unknown parents.
## Copyright(c) 2017-2019 R. Mark Sharp
## This file is part of nprcmanager
#'
#' @return pedigree with unknown animals removed
#'
#' @param ped pedigree dataframe
#' @export
removeUnknownAnimals <- function(ped) {
  ped[getRecordStatusIndex(ped, status = "original"), ]
}
