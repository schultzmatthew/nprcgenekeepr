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
#' @return A reduced pedigree.
#' @export
trimPedigree2 <- function(probands, ped) {
  ped <- trimPedigree(probands, ped)
  p <- removeUniformativeFounders(ped)

  # Adding back second parents where one is known
  single_parents <- p$id[(is.na(p$sire) & !is.na(p$dam)) |
                           (!is.na(p$sire) & is.na(p$dam))]

  add_back <- c()
  for (id in single_parents) {
    if (!is.na(ped$sire[ped$id == id]) & !is.na(ped$dam[ped$id == id])) {

      if (is.na(p$sire[p$id == id])) {
        add_back <- c(add_back, ped$sire[ped$id == id])
        p[(p$id == id), "sire"] <- ped$sire[ped$id == id]
      } else{
        add_back <- c(add_back, ped$dam[ped$id == id])
        p[(p$id == id), "dam"] <- ped$dam[ped$id == id]
      }
    }
  }
  add_back <- ped[(ped$id %in% add_back), ]
  add_back$sire <- NA
  add_back$dam <- NA

  p <- rbind(p, add_back)

  return(p)
}
