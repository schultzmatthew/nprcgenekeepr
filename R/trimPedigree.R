#' Trim pedigree to ancestors of provided group leaving uninformative ancestors.
#'
#' Filters a pedigree down to only the ancestors of the provided group,
#' removing unnecessary individuals from the studbook. This version builds
#' the pedigree back in time starting from a group of probands. This will
#' include all ancestors of the probands, even ones that might be
#' uninformative.
#'
#' @param probands a character vector with the list of animals whose ancestors
#' should be included in the final pedigree.
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information. The fields \code{sire} and \code{dam} are required.
#'
#' @return A reduced pedigree.
#' @export
trimPedigree <- function(probands, ped) {
  animals <- probands

  while (TRUE) {
    sires <- ped$sire[ped$id %in% animals]
    dams <- ped$dam[ped$id %in% animals]

    parents <- unique(union(sires, dams))
    parents <- parents[!is.na(parents)]
    added <- setdiff(parents, animals)

    if (identical(added, character(0))) {
      break
    }
    if (identical(added, numeric(0))) {
      break
    }
    if (identical(added, integer(0))) {
      break
    }
    animals <- union(animals, parents)
  }

  ped <- ped[ped$id %in% animals, ]
  return(ped)
}
