#' removeUnknownAnimals Removes unknown animals added to pedigree that serve as
#' placeholders for unknown parents.
#'
#' @return pedigree with unknown animals removed
#'
#' @param ped pedigree dataframe
#' @export
removeUnknownAnimals <- function(ped) {
  ped[getRecordStatusIndex(ped, status = "original"), ]
}
