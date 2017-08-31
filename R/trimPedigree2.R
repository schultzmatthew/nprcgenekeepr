#' Trim pedigree to ancestors of provided group removing uninformative
#' ancestors.
#'
#' Filters a pedigree down to only the ancestors of the provided group,
#' removing unnecessary individuals from the studbook. This version builds
#' the pedigree back in time starting from a group of probands, then moves
#' back down the tree trimming off uninformative ancestors.
#'
#' @param probands a character vector with the list of animals whose ancestors
#' should be included in the final pedigree.
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The fields \code{sire} and \code{dam} are required.
#'
#' @return A pedigree that has been trimmed, had uninformative founders
#' removed and single parents added back.
#' @export
trimPedigree2 <- function(probands, ped) {
  ped <- trimPedigree(probands, ped)
  p <- removeUniformativeFounders(ped)
  p <- addBackSingleParents(p, ped)
  return(p)
}
