#' Calculates the sex ratio (number of non-males / number of males) given
#' animal Ids and their pedigree
#'
#' @return Numeric value of sex ratio of the animals provided.
#'
#' @description The Males are counted when the \code{ped$sex} value is
#' \code{"M"}.
#' When females are counted when the \code{ped$sex} value is not
#' \code{"M"}. This means animals with ambiguous sex are counted with the
#' females.
#'
#' @param ids character vector of animal Ids
#' @param ped datatable that is the `Pedigree`. It contains pedigree
#' information including the IDs listed in \code{candidates}.
#' @export
calculateSexRatio <- function(ids, ped) {
  if (length(ids) == 0 | length(ped$sex[ped$id %in% ids & ped$sex != "M"]) == 0) {
    ratio <- 0.0
  } else if (length(ped$sex[ped$id %in% ids & ped$sex == "M"]) == 0) {
    ratio <- Inf
  } else {
    ratio <- length(ped$sex[ped$id %in% ids & ped$sex != "M"]) /
      length(ped$sex[ped$id %in% ids & ped$sex == "M"])
  }
  ratio
}
